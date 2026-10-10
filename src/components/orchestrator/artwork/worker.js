import * as IDB from "idb-keyval";

import { create as createCid } from "~/common/cid.js";
import { ostiary, rpc, workerProxy } from "~/common/worker.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {ActionsWithTunnel, ProxiedActions} from "~/common/worker.d.ts"
 * @import {Actions} from "@specs/components/artwork/types.d.ts"
 * @import {Artwork} from "@specs/components/orchestrator/artwork/types.d.ts"
 */

/**
 * Cached entry shape. `validated` marks entries whose bytes have been
 * confirmed to decode as an image — entries written before validation
 * existed may be junk (e.g. cached error pages).
 * @typedef {Artwork & { validated?: boolean }} CachedArtwork
 */

// multicodec raw bytes
const RAW = 0x55;

const IDB_PREFIX = "~/components/orchestrator/artwork";
const IDB_ARTWORK_PREFIX = `${IDB_PREFIX}/cache`;

/** @type {Map<string, Promise<Uint8Array | null>>} */
const inFlight = new Map();

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {ActionsWithTunnel<Actions>['get']}
 */
export async function get({ data: track, ports }) {
  const existing = inFlight.get(track.id);
  if (existing) return existing;

  const promise = processRequest(track, ports).finally(() => {
    inFlight.delete(track.id);
  });

  inFlight.set(track.id, promise);
  return promise;
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { get });
});

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

/**
 * @param {Track} track
 * @param {Record<string, MessagePort>} ports
 * @returns {Promise<Uint8Array | null>}
 */
async function processRequest(track, ports) {
  // Check if already processed

  /** @type {string[] | undefined} */
  const cachedCids = await IDB.get(
    `${IDB_ARTWORK_PREFIX}/track/${track.id}`,
  );

  if (cachedCids?.length) {
    /** @type {CachedArtwork[]} */
    const art = await Promise.all(
      cachedCids.map((cid) => IDB.get(`${IDB_ARTWORK_PREFIX}/image/${cid}`)),
    );

    const found = art.filter(Boolean);
    if (found.length) {
      const entry = found[0];
      if (entry.validated || await isImage(entry.bytes)) {
        // Flag older entries as validated so the decode check runs at
        // most once per image
        if (!entry.validated) {
          await IDB.set(`${IDB_ARTWORK_PREFIX}/image/${cachedCids[0]}`, {
            ...entry,
            validated: true,
          });
        }
        return entry.bytes;
      }

      // Undecodable junk (e.g. error pages cached by older versions) —
      // drop the mapping so the providers get another shot
      await IDB.set(`${IDB_ARTWORK_PREFIX}/track/${track.id}`, []);
    }
  }

  // 🚀

  /** @type {ProxiedActions<Actions>} */
  const configurator = workerProxy(() => {
    ports.artwork.start();
    return ports.artwork;
  });

  let bytes;

  try {
    bytes = await configurator.get(track);
  } catch {
    return null;
  }

  if (bytes === null) {
    await IDB.set(`${IDB_ARTWORK_PREFIX}/track/${track.id}`, []);
    return null;
  }

  // Don't cache bytes that aren't actually an image — they'd be served
  // back forever and render as a broken thumbnail in the UI
  if (!await isImage(bytes)) return null;

  const mime = detectMime(bytes);

  /** @type {CachedArtwork} */
  const art = { bytes, mime, validated: true };

  // Save artwork to IDB — store by content CID, map track to that CID
  const cid = await createCid(RAW, bytes);
  const key = `${IDB_ARTWORK_PREFIX}/image/${cid}`;
  if (!await IDB.get(key)) await IDB.set(key, art);

  await IDB.set(`${IDB_ARTWORK_PREFIX}/track/${track.id}`, [cid]);

  return bytes;
}

/**
 * Checks the bytes actually decode as an image, so junk (HTML error
 * pages, corrupt embedded art) never reaches the cache or the UI.
 * @param {Uint8Array} bytes
 * @returns {Promise<boolean>}
 */
async function isImage(bytes) {
  try {
    const bitmap = await createImageBitmap(
      new Blob([/** @type {BlobPart} */ (bytes)], {
        type: "application/octet-stream",
      }),
    );
    bitmap.close();
    return true;
  } catch {
    return false;
  }
}

/**
 * @param {Uint8Array} bytes
 * @returns {string}
 */
function detectMime(bytes) {
  if (bytes[0] === 0xFF && bytes[1] === 0xD8) return "image/jpeg";
  if (bytes[0] === 0x89 && bytes[1] === 0x50) return "image/png";
  if (bytes[0] === 0x47 && bytes[1] === 0x49) return "image/gif";
  if (bytes[0] === 0x52 && bytes[1] === 0x49) return "image/webp";
  return "image/jpeg";
}
