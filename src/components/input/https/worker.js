import { ostiary, rpc } from "~/common/worker.js";
import { detach as detachUtil, groupKey } from "~/components/input/common.js";

import {
  consultHostCached,
  groupTracksByHost,
  groupUrisByHost,
  parseURI,
} from "./common.js";
import { SCHEME } from "./constants.js";

/**
 * @import { InputActions as Actions, ConsultGrouping } from "@specs/components/input/types.d.ts";
 */

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
export async function consult(fileUriOrScheme) {
  if (!fileUriOrScheme.includes(":")) {
    return { supported: true, consult: "undetermined" };
  }

  const parsed = parseURI(fileUriOrScheme);
  if (!parsed) {
    return { supported: false, reason: "Invalid HTTPS URL" };
  }

  const consult = await consultHostCached(parsed.url);
  return { supported: true, consult };
}

/**
 * @type {Actions['detach']}
 */
export async function detach(args) {
  return detachUtil({
    ...args,

    inputScheme: SCHEME,
    handleFileUri: ({ fileURI, tracks }) => {
      const result = parseURI(fileURI);
      if (!result) return tracks;

      const did = result.host;
      const groups = groupTracksByHost(tracks);

      delete groups[did];

      return Object.values(groups).map((a) => a.tracks).flat(1);
    },
  });
}

/**
 * @type {Actions['groupConsult']}
 */
export async function groupConsult(uris) {
  const groups = groupUrisByHost(uris);

  const promises = Object.entries(groups).map(
    async ([_domainId, { host, uris }]) => {
      const testUri = uris[0];
      const available = testUri ? await consultHostCached(testUri) : false;

      /** @type {ConsultGrouping} */
      const grouping = available
        ? { available, scheme: SCHEME, uris }
        : { available, reason: "Host unreachable", scheme: SCHEME, uris };

      return {
        key: groupKey(SCHEME, host),
        grouping,
      };
    },
  );

  const entries = (await Promise.all(promises)).map((
    entry,
  ) => [entry.key, entry.grouping]);

  return Object.fromEntries(entries);
}

/**
 * @type {Actions['list']}
 */
export async function list(cachedTracks = []) {
  return cachedTracks.map((track) => {
    const t = { ...track };

    if (t.kind === "placeholder") {
      t.kind = undefined;
    }

    return t;
  });
}

/**
 * @type {Actions['resolve']}
 */
export async function resolve({ method, uri }) {
  if (uri.startsWith("blob:")) {
    const expiresInSeconds = 60 * 60 * 24 * 365;
    const expiresAtSeconds = Math.round(Date.now() / 1000) + expiresInSeconds;
    return { url: uri, expiresAt: expiresAtSeconds };
  }

  const parsed = parseURI(uri);
  if (!parsed) return undefined;

  // HTTPS URLs don't need resolution - they're already accessible.
  // Just return the URL as-is with a far-future expiration.
  const expiresInSeconds = 60 * 60 * 24 * 365; // 1 year
  const expiresAtSeconds = Math.round(Date.now() / 1000) + expiresInSeconds;

  return {
    url: parsed.url,
    expiresAt: expiresAtSeconds,
  };
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  // Setup RPC

  rpc(context, {
    artwork,
    consult,
    detach,
    groupConsult,
    list,
    resolve,
  });
});
