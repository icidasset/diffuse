import * as IDB from "idb-keyval";

import { IDB_PREFIX } from "./constants.js";
import { ostiary, rpc } from "@common/worker.js";

/**
 * @import {Track} from "@definitions/types.d.ts";
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

ostiary((context) => {
  rpc(context, {
    getTracks,
    putTracks,
  });
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
