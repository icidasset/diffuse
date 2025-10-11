import * as IDB from "idb-keyval";

import { expose, jsonDecode, jsonEncode, transfer } from "@scripts/common";
import type { Track } from "@applets/core/types";
import { IDB_PREFIX } from "./constants";

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////
const actions = expose({
  getTracks,
  putTracks,
});

export type Actions = typeof actions;

// Actions

async function getTracks() {
  const encoded = await get({ name: "tracks.json" });
  if (!encoded) return [];
  const tracks = jsonDecode<Track[]>(encoded);
  return transfer(tracks);
}

async function putTracks(tracks: Track[]) {
  const data = jsonEncode(tracks);
  await put({ name: "tracks.json", data });
}

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

async function get({ name }: { name: string }) {
  return await IDB.get(`${IDB_PREFIX}/${name}`);
}

async function put({ data, name }: { data: Uint8Array; name: string }) {
  return await IDB.set(`${IDB_PREFIX}/${name}`, data);
}
