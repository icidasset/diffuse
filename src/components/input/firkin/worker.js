import * as TID from "@atcute/tid";

import { ostiary, rpc } from "~/common/worker.js";
import { isAudioFile } from "~/components/input/common.js";
import {
  blobURL,
  buildURI,
  FILE_URL_TTL_SECONDS,
  fileURL,
  listFiles,
  mimeType,
  parseBlobURI,
  parseURI,
  readFile,
  SCHEME,
  SOURCE_URI,
} from "~/common/firkin/files.js";
import { getSessionClient } from "~/common/firkin/session.js";

/**
 * @import { Track } from "~/definitions/types.d.ts"
 * @import { InputActions as Actions, ConsultGrouping } from "@specs/components/input/types.d.ts"
 */

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const NOT_CONNECTED =
  "Firkin is not connected. Sign in on the Connect → Firkin page first.";

/** @type {Map<string, string>} */
const blobUrls = new Map();

/** Cache a *definitive* status for a moment; a timeout is retried. */
const STATUS_TTL = 5_000;
/** @type {{ connected: boolean; did?: string; apiBase?: string } | undefined} */
let lastStatus;
let lastStatusAt = 0;

/** @returns {Promise<{ connected: boolean; did?: string; apiBase?: string } | undefined>} */
async function storageStatus() {
  if (lastStatus && Date.now() - lastStatusAt < STATUS_TTL) return lastStatus;

  try {
    const client = await getSessionClient();
    lastStatus = client
      ? {
        connected: true,
        did: client.session.did,
        apiBase: client.session.apiBase,
      }
      : { connected: false };
  } catch {
    // No session yet, or IndexedDB is unavailable. Don't cache: sign-in may
    // complete while this worker is alive.
    lastStatus = undefined;
  }

  lastStatusAt = Date.now();
  return lastStatus;
}

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions['artwork']}
 */
export async function artwork(_uri) {
  return null;
}

/**
 * @type {Actions['consult']}
 */
export async function consult(uriOrScheme) {
  if (!uriOrScheme.includes("://")) {
    return { supported: true, consult: "undetermined" };
  }

  // Generic CID-addressed blobs need no session: RASL is anonymous by
  // default, and an optional token is carried by the URI itself.
  if (parseBlobURI(uriOrScheme)) {
    return { supported: true, consult: "yes" };
  }

  const status = await storageStatus();
  if (!status) return { supported: true, consult: "unsure" };

  return { supported: true, consult: status.connected ? "yes" : "no" };
}

/**
 * @type {Actions['detach']}
 */
export async function detach({ tracks }) {
  const removed = tracks.filter((t) => t.uri.startsWith(`${SCHEME}://`));

  for (const track of removed) {
    const url = blobUrls.get(track.uri);
    if (url) {
      URL.revokeObjectURL(url);
      blobUrls.delete(track.uri);
    }
  }

  return tracks.filter((t) => !t.uri.startsWith(`${SCHEME}://`));
}

/**
 * @type {Actions['groupConsult']}
 */
export async function groupConsult(uris) {
  // A group containing generic CID sources is usable without the account
  // session; only an account-only group follows the session state.
  if (uris.some((uri) => parseBlobURI(uri) !== null)) {
    return { [SCHEME]: { available: "yes", scheme: SCHEME, uris } };
  }

  const status = await storageStatus();
  const available = !status ? "unsure" : status.connected ? "yes" : "no";

  /** @type {ConsultGrouping} */
  const grouping = available === "no"
    ? {
      available,
      reason: "Firkin is not connected",
      scheme: SCHEME,
      uris,
    }
    : { available, scheme: SCHEME, uris };

  return { [SCHEME]: grouping };
}

/**
 * @type {Actions['list']}
 */
export async function list(cachedTracks = []) {
  let client;
  try {
    client = await getSessionClient();
  } catch {
    // IndexedDB/session access failed. Keep the cached tracks rather than
    // dropping the user's library.
    return cachedTracks;
  }

  if (!client) return cachedTracks;

  let files;
  try {
    files = (await listFiles(client.rb)).filter((file) =>
      isAudioFile(file.name)
    );
  } catch {
    // The session became unusable (expired, revoked, offline). Keep the
    // cached tracks rather than dropping the user's library.
    return cachedTracks;
  }

  const cached = new Map(cachedTracks.map((track) => [track.uri, track]));

  /** @type {Track[]} */
  const tracks = files.map((file) => {
    const uri = buildURI(file.name);
    const previous = cached.get(uri);
    const now = new Date().toISOString();

    /** @type {Track} */
    return {
      $type: "sh.diffuse.output.track",
      id: previous?.id ?? TID.now(),
      createdAt: previous?.createdAt ?? now,
      updatedAt: previous?.updatedAt ?? now,
      stats: previous?.stats,
      tags: previous?.tags,
      uri,
    };
  });

  // Generic CID-addressed tracks are self-contained; keep them alongside the
  // account library when the session is available.
  const genericTracks = cachedTracks.filter((track) =>
    parseBlobURI(track.uri) !== null
  );

  const allTracks = [...tracks, ...genericTracks];

  if (allTracks.length > 0) return allTracks;

  // Keep the source discoverable while the registry is empty.
  const placeholder = cachedTracks.find((track) =>
    track.kind === "placeholder"
  );
  if (placeholder) return [placeholder];

  const now = new Date().toISOString();

  /** @type {Track} */
  const track = {
    $type: "sh.diffuse.output.track",
    id: TID.now(),
    createdAt: now,
    updatedAt: now,
    kind: "placeholder",
    uri: SOURCE_URI,
  };

  return [track];
}

/**
 * @type {Actions['resolve']}
 */
export async function resolve({ uri }) {
  const account = parseURI(uri);
  if (account) return await resolveAccountFile(uri, account.name);

  const blob = parseBlobURI(uri);
  if (blob) {
    // RASL by default; a `token` in the URI opts into the authenticated blob
    // route. RASL content is immutable, so there is nothing to refresh.
    return { url: blobURL(blob), expiresAt: Infinity };
  }

  return undefined;
}

/**
 * @param {string} uri
 * @param {string} name
 */
async function resolveAccountFile(uri, name) {
  const client = await getSessionClient();
  if (!client) throw new Error(NOT_CONNECTED);

  // Stream directly from Firkin with a CID-scoped URL: the media element and
  // metadata workers get HTTP range support without buffering in between.
  try {
    const url = await fileURL(
      client.rb,
      name,
      FILE_URL_TTL_SECONDS,
    );

    return {
      url,
      expiresAt: Math.round(Date.now() / 1000) + FILE_URL_TTL_SECONDS,
    };
  } catch {
    // Read URLs unavailable — fall back to a blob.
  }

  let url = blobUrls.get(uri);

  if (!url) {
    const bytes = await readFile(client.rb, name);
    const blob = new Blob(
      [/** @type {BlobPart} */ (/** @type {unknown} */ (bytes))],
      { type: mimeType(name) },
    );
    url = URL.createObjectURL(blob);
    blobUrls.set(uri, url);
  }

  return { expiresAt: Infinity, url };
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, {
    artwork,
    consult,
    detach,
    groupConsult,
    list,
    resolve,
  });
});
