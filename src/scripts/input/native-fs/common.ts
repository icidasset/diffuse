import * as IDB from "idb-keyval";
import * as URI from "uri-js";
import QS from "query-string";

import type { Track } from "@applets/core/types.d.ts";
import type { Handles } from "./types";
import { isAudioFile } from "@scripts/input/common";
import { IDB_HANDLES, SCHEME } from "./constants";

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////
export async function fetchHandles(): Promise<Handles> {
  return (await IDB.get(IDB_HANDLES)) ?? {};
}

export async function fetchHandlesList() {
  return Object.entries(await fetchHandles()).map(([id, handle]) => {
    return { id, handle };
  });
}
export function groupTracksByHandle(tracks: Track[]) {
  const acc: Record<string, { tracks: Track[] }> = {};

  tracks.forEach((track: Track) => {
    const id = trackHandleId(track);
    if (!id) return acc;

    if (acc[id]) {
      acc[id].tracks.push(track);
    } else {
      acc[id] = { tracks: [track] };
    }
  });

  return acc;
}

export function isSupported() {
  return !!(globalThis as any).showDirectoryPicker;
}

export function trackCid(track: Track): string | undefined {
  const a = URI.parse(track.uri);
  const cid = a.query ? QS.parse(a.query).cid || undefined : undefined;
  return Array.isArray(cid) && cid[0] ? cid[0] : typeof cid === "string" ? cid : undefined;
}

export function trackHandleId(track: Track): string | undefined {
  const a = URI.parse(track.uri);
  return a.host;
}

export async function recursiveList(
  dir: FileSystemDirectoryHandle,
  rootHandleId: string,
  path: string[],
): Promise<Track[]> {
  const tracks: Track[] = [];

  for await (const item of dir.values()) {
    if (item.kind === "file" && isAudioFile(item.name)) {
      const uri = URI.serialize({
        scheme: SCHEME,
        host: rootHandleId,
        path: `${path.length ? "/" + path.join("/") : ""}/${item.name}`,
      });

      const track: Track = {
        id: crypto.randomUUID(),
        uri,
      };

      tracks.push(track);
    } else if (item.kind === "directory") {
      const nestedItems = await recursiveList(item as FileSystemDirectoryHandle, rootHandleId, [
        ...path,
        item.name,
      ]);

      tracks.push(...nestedItems);
    }
  }

  return tracks;
}
