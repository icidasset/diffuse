import * as IDB from "idb-keyval";

import { expose, jsonDecode, jsonEncode, transfer } from "@scripts/common";
import type { Track } from "@applets/core/types";
import { IDB_DEVICE_KEY, IDB_PREFIX } from "./constants";

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
  const handle: FileSystemDirectoryHandle | null = (await IDB.get(IDB_DEVICE_KEY)) ?? null;
  if (!handle) throw new Error("Storage not configured properly, handle not found.");

  try {
    const fileHandle = await handle.getFileHandle(name);
    const file = await fileHandle.getFile();
    const data = await file.bytes();
    return data;
  } catch (err) {
    return undefined;
  }
}

async function put({ data, name }: { data: Uint8Array; name: string }) {
  const handle: FileSystemDirectoryHandle | null = (await IDB.get(IDB_DEVICE_KEY)) ?? null;
  if (!handle) throw new Error("Storage not configured properly, handle not found.");
  const fileHandle = await handle.getFileHandle(name, { create: true });
  const stream = await fileHandle.createWritable();
  await stream.write(data);
  await stream.close();
}
