import * as IDB from "idb-keyval";

import { IDB_PREFIX } from "./constants.js";
import { define, ostiary } from "@common/worker.js";

/**
 * @import {Track} from "@common/types.d.ts";
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @returns {Promise<Track[]>}
 */
export async function getTracks() {
  /** @type {Track[] | null} */
  const tracks = await get({ name: "tracks.json" });
  return tracks ?? [];
}

/**
 * @param {Track[]} tracks
 */
export async function putTracks(tracks) {
  await put({ name: "tracks.json", data: tracks });
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
 * @param {{ data: any; name: string }} _
 */
async function put({ data, name }) {
  return await IDB.set(`${IDB_PREFIX}/${name}`, data);
}
