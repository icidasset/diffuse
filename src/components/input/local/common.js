import * as IDB from "idb-keyval";
import * as URI from "fast-uri";

import { isAudioFile } from "~/components/input/common.js";
import { safeDecodeURIComponent } from "~/common/utils.js";
import { IDB_HANDLES, SCHEME } from "./constants.js";

/**
 * @import { Track } from "~/definitions/types.d.ts"
 */

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

/**
 * @param {string} uriString
 * @returns {{ tid: string; path: string } | undefined}
 */
export function parseURI(uriString) {
  try {
    const url = new URL(uriString);
    if (url.protocol !== `${SCHEME}:`) return undefined;
    if (!url.host) return undefined;

    return {
      tid: url.host,
      path: safeDecodeURIComponent(url.pathname),
    };
  } catch {
    return undefined;
  }
}

/**
 * @param {string} tid
 * @param {string} [path]
 */
export function buildURI(tid, path = "/") {
  return URI.serialize({
    scheme: SCHEME,
    host: tid,
    path,
  });
}

/**
 * @param {Track[]} tracks
 * @returns {Record<string, { tid: string; tracks: Track[] }>}
 */
export function groupTracksByTid(tracks) {
  /** @type {Record<string, { tid: string; tracks: Track[] }>} */
  const acc = {};

  tracks.forEach((track) => {
    const parsed = parseURI(track.uri);
    if (!parsed) return;

    const { tid } = parsed;
    if (acc[tid]) {
      acc[tid].tracks.push(track);
    } else {
      acc[tid] = { tid, tracks: [track] };
    }
  });

  return acc;
}

/**
 * @param {string[]} uris
 * @returns {Record<string, { tid: string; uris: string[] }>}
 */
export function groupUrisByTid(uris) {
  /** @type {Record<string, { tid: string; uris: string[] }>} */
  const acc = {};

  uris.forEach((uri) => {
    const parsed = parseURI(uri);
    if (!parsed) return;

    const { tid } = parsed;
    if (acc[tid]) {
      acc[tid].uris.push(uri);
    } else {
      acc[tid] = { tid, uris: [uri] };
    }
  });

  return acc;
}

/**
 * @param {Track[]} tracks
 * @returns {Record<string, string>}
 */
export function tidsFromTracks(tracks) {
  /** @type {Record<string, string>} */
  const acc = {};

  tracks.forEach((track) => {
    const parsed = parseURI(track.uri);
    if (!parsed) return;
    acc[parsed.tid] = parsed.tid;
  });

  return acc;
}

/**
 * @returns {Promise<Record<string, FileSystemHandle>>}
 */
export async function loadHandles() {
  const i = await IDB.get(IDB_HANDLES);
  return i ?? {};
}

/**
 * @param {Record<string, FileSystemHandle>} handles
 */
export async function saveHandles(handles) {
  await IDB.set(IDB_HANDLES, handles);
}

/**
 * @param {FileSystemHandle} handle
 * @param {string} path
 * @returns {Promise<FileSystemFileHandle>}
 */
export async function getHandleFile(handle, path) {
  if (handle.kind === "file") {
    return /** @type {FileSystemFileHandle} */ (handle);
  }

  const parts = path.replace(/^\//, "").split("/").filter(Boolean);
  let current = /** @type {FileSystemDirectoryHandle} */ (handle);

  for (const part of parts.slice(0, -1)) {
    current = await current.getDirectoryHandle(part);
  }

  return current.getFileHandle(/** @type {string} */ (parts.at(-1)));
}

/**
 * @param {FileSystemDirectoryHandle} dirHandle
 * @param {string} [basePath]
 * @returns {Promise<string[]>}
 */
export async function enumerateAudioFiles(dirHandle, basePath = "/") {
  const results = [];

  for await (const [name, handle] of /** @type {any} */ (dirHandle).entries()) {
    const entryPath = basePath + name;

    if (handle.kind === "directory") {
      const sub = await enumerateAudioFiles(
        /** @type {FileSystemDirectoryHandle} */ (handle),
        entryPath + "/",
      );
      results.push(...sub);
    } else if (isAudioFile(name)) {
      results.push(entryPath);
    }
  }

  return results;
}
