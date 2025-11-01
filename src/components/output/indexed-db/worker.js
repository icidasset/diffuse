import * as IDB from "idb-keyval";

import { jsonDecode, jsonEncode } from "@common/index.js";
import { IDB_PREFIX } from "./constants.js";
import { define, ostiary } from "@common/worker.js";

/**
 * @import {OutputActions, Track} from "@components/core/types.d.ts";
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {OutputActions['getTracks']}
 */
export async function getTracks() {
  const encoded = await get({ name: "tracks.json" });
  if (!encoded) return [];

  /** @type {Track[]} */
  const tracks = jsonDecode(encoded);
  return tracks;
}

/**
 * @type {OutputActions['putTracks']}
 */
export async function putTracks(tracks) {
  const data = jsonEncode(tracks);
  await put({ name: "tracks.json", data });
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((port) => {
  // Setup RPC
  define("getTracks", getTracks, port);
  define("putTracks", putTracks, port);
});

////////////////////////////////////////////
// ⛔️
////////////////////////////////////////////

/**
 * @param {{ name: string }} _
 */
async function get({ name }) {
  return await IDB.get(`${IDB_PREFIX}/${name}`);
}

/**
 * @param {{ data: Uint8Array; name: string }} _
 */
async function put({ data, name }) {
  return await IDB.set(`${IDB_PREFIX}/${name}`, data);
}
