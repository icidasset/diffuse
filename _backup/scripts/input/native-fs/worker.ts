import * as URI from "uri-js";

import type { Consult, ConsultGrouping, GroupConsult, Track } from "@applets/core/types.d.ts";
import { SCHEME } from "./constants";
import {
  fetchHandles,
  fetchHandlesList,
  groupTracksByHandle,
  recursiveList,
  trackHandleId,
} from "./common";
import { provide, transfer } from "@scripts/common";

////////////////////////////////////////////
// TASKS
////////////////////////////////////////////
const actions = {
  consult,
  contextualize,
  groupConsult,
  list,
  resolve,
};

const { tasks } = provide({ actions, tasks: actions });

export type Actions = typeof actions;
export type Tasks = typeof tasks;

// Tasks

export async function consult(fileUriOrScheme: string): Promise<Consult> {
  if (!self.FileSystemDirectoryHandle) {
    return { supported: false, reason: "File System Access API is not supported" };
  }

  if (!fileUriOrScheme.includes(":")) {
    if (fileUriOrScheme !== SCHEME) return { supported: false, reason: "Scheme does not match" };
    return { supported: true, consult: "undetermined" };
  }

  const handles = await fetchHandles();
  const uri = URI.parse(fileUriOrScheme);
  if (uri.scheme !== SCHEME) return { supported: false, reason: "Scheme does not match" };
  return { supported: true, consult: uri.host && !!handles[uri.host] ? true : false };
}

export async function contextualize(cachedTracks: Track[]) {}

async function groupConsult(tracks: Track[]): Promise<GroupConsult> {
  const groups = groupTracksByHandle(tracks);
  const handles = await fetchHandles();

  const promises = Object.entries(groups).map(async ([handleId, { tracks }]) => {
    const handle = handles[handleId];
    const grouping: ConsultGrouping = handle
      ? { available: true, tracks }
      : { available: false, reason: "Handle not available", tracks };

    return {
      key: URI.serialize({ scheme: SCHEME, host: handleId }),
      grouping,
    };
  });

  const entries = (await Promise.all(promises)).map((entry) => [entry.key, entry.grouping]);
  const obj = Object.fromEntries(entries);

  return transfer(obj);
}

export async function list(cachedTracks: Track[] = []) {
  const handles = await fetchHandlesList();

  // Recursive listing of all tracks of available handles
  const processed: Track[][] = await Promise.all(
    handles.map(({ id, handle }) => {
      return recursiveList(handle, id, []);
    }),
  );

  // Group tracks by handle id & index by track uri
  const cache: Record<string, Record<string, Track>> = {};

  cachedTracks.forEach((track: Track) => {
    const handleId = trackHandleId(track);
    if (!handleId) return;

    cache[handleId] ??= {};
    cache[handleId][track.uri] = track;
  });

  // Replace indexes in groups of which we have the handle.
  // Keeping around tracks with handles we don't have access to,
  // and removing tracks that are no longer available (for handles we do have access to).

  // TODO: Refactor to not use `reduce`, for performance.
  const groups = processed.flat(1).reduce(
    (acc, track) => {
      const handleId = trackHandleId(track);
      if (!handleId) throw new Error("New tracks are missing a handle id!");

      return { ...acc, [handleId]: { ...acc[handleId], [track.uri]: track } };
    },
    handles.reduce((acc: Record<string, Record<string, Track>>, handle) => {
      return { ...acc, [handle.id]: {} };
    }, cache),
  );

  // Transform in track list and sort by uri
  const data = Object.values(groups)
    .map((tracks) => Object.values(tracks))
    .flat(1)
    .sort((a: any, b: any) => {
      if (a.uri < b.uri) return -1;
      if (a.uri > b.uri) return 1;
      return 0;
    });

  // Fin
  return transfer(data);
}

export async function resolve(args: { uri: string }) {
  const fileUri = args.uri;

  const uri = URI.parse(fileUri);
  if (uri.scheme !== SCHEME) return undefined;
  if (!uri.host || !uri.path) return undefined;

  const handles = await fetchHandles();
  const handle = handles[uri.host];
  if (!handle) return undefined;

  const path = URI.unescapeComponent(uri.path);
  const parts = (path.startsWith("/") ? path.slice(1) : path).split("/");
  const filename = parts[parts.length - 1];

  const dirHandle = await parts
    .slice(0, -1)
    .reduce(
      async (
        acc: Promise<FileSystemDirectoryHandle>,
        part: string,
      ): Promise<FileSystemDirectoryHandle> => {
        const h = await acc;
        return await h.getDirectoryHandle(part);
      },
      Promise.resolve(handle),
    );

  const fileHandle = await dirHandle.getFileHandle(filename);
  const file = await fileHandle.getFile();
  const url = URL.createObjectURL(file);

  return { expiresAt: Infinity, url };
}
