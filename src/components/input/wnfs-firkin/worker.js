import * as TID from "@atcute/tid";

import { ostiary, rpc } from "~/common/worker.js";
import { isAudioFile } from "~/components/input/common.js";
import {
  buildURI,
  FILE_URL_TTL_SECONDS,
  mimeType,
  parseURI,
  SCHEME,
  SOURCE_URI,
  StorageClient,
} from "~/components/output/bytes/wnfs/files.js";

/**
 * @import { Track } from "~/definitions/types.d.ts"
 * @import { InputActions as Actions, ConsultGrouping } from "@specs/components/input/types.d.ts"
 */

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const params = new URL(globalThis.location.href).searchParams;

const client = new StorageClient({
  group: params.get("group") || undefined,
  namespace: params.get("namespace") || undefined,
});

/** @type {Map<string, string>} */
const blobUrls = new Map();

/** Cache a *definitive* status for a moment; a timeout is retried. */
const STATUS_TTL = 5_000;
/** @type {Awaited<ReturnType<StorageClient["status"]>> | undefined} */
let lastStatus;
let lastStatusAt = 0;

/** @returns {Promise<Awaited<ReturnType<StorageClient["status"]>> | undefined>} */
async function storageStatus() {
  if (lastStatus && Date.now() - lastStatusAt < STATUS_TTL) return lastStatus;

  try {
    lastStatus = await client.status();
  } catch {
    // No output element answered. Don't cache: a leader may come back.
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
  const status = await storageStatus();
  const available = !status ? "unsure" : status.connected ? "yes" : "no";

  /** @type {ConsultGrouping} */
  const grouping = available === "no"
    ? {
      available,
      reason: "Firkin + WNFS is not connected",
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
  const status = await storageStatus();
  if (!status?.connected) return cachedTracks;

  let files;
  try {
    files = (await client.listFiles()).filter((file) => isAudioFile(file.name));
  } catch {
    // The storage became unreachable (tab closed, session lost). Keep the
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

  if (tracks.length > 0) return tracks;

  // Keep the source discoverable while the directory is empty.
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
  const parsed = parseURI(uri);
  if (!parsed) return undefined;

  // Stream directly from Firkin with a presigned, CID-scoped URL: the media
  // element and metadata workers get HTTP range support without WNFS or blob
  // buffering in between.
  try {
    const url = await client.fileURL(parsed.name, FILE_URL_TTL_SECONDS);
    if (url) {
      return {
        url,
        expiresAt: Math.round(Date.now() / 1000) + FILE_URL_TTL_SECONDS,
      };
    }
  } catch {
    // Presigned URLs unavailable — fall back to a blob.
  }

  let url = blobUrls.get(uri);

  if (!url) {
    const bytes = await client.read(parsed.name);
    const blob = new Blob(
      [/** @type {BlobPart} */ (/** @type {unknown} */ (bytes))],
      { type: mimeType(parsed.name) },
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
