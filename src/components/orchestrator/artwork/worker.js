import * as IDB from "idb-keyval";

import { create as createCid } from "~/common/cid.js";
import { ostiary, rpc, workerProxy } from "~/common/worker.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {ActionsWithTunnel, ProxiedActions} from "~/common/worker.d.ts"
 * @import {Actions} from "@specs/components/artwork/types.d.ts"
 * @import {Artwork} from "@specs/components/orchestrator/artwork/types.d.ts"
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
    /** @type {Artwork[]} */
    const art = await Promise.all(
      cachedCids.map((cid) => IDB.get(`${IDB_ARTWORK_PREFIX}/image/${cid}`)),
    );

    const found = art.filter(Boolean);
    if (found.length) return found[0].bytes;
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

  const mime = detectMime(bytes);

  /** @type {Artwork} */
  const art = { bytes, mime };

  // Save artwork to IDB — store by content CID, map track to that CID
  const cid = await createCid(RAW, bytes);
  const key = `${IDB_ARTWORK_PREFIX}/image/${cid}`;
  if (!await IDB.get(key)) await IDB.set(key, art);

  await IDB.set(`${IDB_ARTWORK_PREFIX}/track/${track.id}`, [cid]);

  return bytes;
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
