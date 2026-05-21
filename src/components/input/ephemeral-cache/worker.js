import * as IDB from "idb-keyval";

import { ostiary, rpc } from "~/common/worker.js";
import { CACHE_KEY_PREFIX, SCHEME } from "./constants.js";

/**
 * @import { InputActions as Actions } from "@specs/components/input/types.d.ts"
 * @import { Track } from "~/definitions/types.d.ts"
 */

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

/** @type {Map<string, string>} */
const blobUrls = new Map();

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

  const cached = await IDB.get(CACHE_KEY_PREFIX + uriOrScheme);
  return { supported: true, consult: cached !== undefined };
}

/**
 * @type {Actions['detach']}
 */
export async function detach({ fileUriOrScheme, tracks }) {
  if (!fileUriOrScheme.includes("://")) {
    if (fileUriOrScheme === SCHEME) {
      await removeBlobs(tracks.map((t) => t.uri));
      return [];
    }
    return tracks;
  }

  const remaining = tracks.filter((t) => t.uri !== fileUriOrScheme);
  await removeBlobs([fileUriOrScheme]);
  return remaining;
}

/**
 * @type {Actions['groupConsult']}
 */
export async function groupConsult(uris) {
  const cached = await Promise.all(
    uris.map((uri) => IDB.get(CACHE_KEY_PREFIX + uri)),
  );

  return {
    [SCHEME]: {
      available: true,
      scheme: SCHEME,
      uris: uris.filter((_, i) => cached[i] !== undefined),
    },
  };
}

/**
 * @type {Actions['list']}
 */
export async function list(tracks) {
  return tracks;
}

/**
 * @type {Actions['resolve']}
 */
export async function resolve({ uri }) {
  const blob =
    /** @type {Blob | undefined} */ (await IDB.get(CACHE_KEY_PREFIX + uri));
  if (!blob) return undefined;

  let blobUrl = blobUrls.get(uri);

  if (!blobUrl) {
    blobUrl = URL.createObjectURL(blob);
    blobUrls.set(uri, blobUrl);
  }

  return { expiresAt: Infinity, url: blobUrl };
}

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

/**
 * @param {string[]} uris
 */
async function removeBlobs(uris) {
  await Promise.all(uris.map(async (uri) => {
    const blobUrl = blobUrls.get(uri);
    if (blobUrl) {
      URL.revokeObjectURL(blobUrl);
      blobUrls.delete(uri);
    }
    await IDB.del(CACHE_KEY_PREFIX + uri);
  }));
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
