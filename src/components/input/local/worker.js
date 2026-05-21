import * as TID from "@atcute/tid";
import { ostiary, rpc } from "~/common/worker.js";
import { groupKey } from "~/components/input/common.js";
import {
  buildURI,
  enumerateAudioFiles,
  getHandleFile,
  groupTracksByTid,
  groupUrisByTid,
  isSupported,
  loadHandles,
  parseURI,
  saveHandles,
} from "./common.js";
import { SCHEME } from "./constants.js";

/**
 * @import { InputActions as Actions, ConsultGrouping } from "@specs/components/input/types.d.ts";
 * @import { Track } from "~/definitions/types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions['artwork']}
 */
export async function artwork(_uri) {
  return null;
}

/**
 * @type {Actions['consult']}
 */
export async function consult(fileUriOrScheme) {
  if (!isSupported()) {
    return { supported: false, reason: "No browser support" };
  }

  if (!fileUriOrScheme.includes(":")) {
    return { supported: true, consult: "undetermined" };
  }

  const parsed = parseURI(fileUriOrScheme);
  if (!parsed) return { supported: false, reason: "Unknown handle" };

  const handles = await loadHandles();
  const handle = handles[parsed.tid];

  if (!handle) return { supported: false, reason: "Unknown handle" };

  const permission = await /** @type {any} */ (handle).queryPermission({
    mode: "read",
  });

  return { supported: true, consult: permission === "granted" };
}

/**
 * @type {Actions['detach']}
 */
export async function detach({ fileUriOrScheme, tracks }) {
  if (!fileUriOrScheme.includes("://")) {
    if (fileUriOrScheme === SCHEME) return [];
    return tracks;
  }

  const parsed = parseURI(fileUriOrScheme);
  if (!parsed) return tracks;

  const { tid } = parsed;
  const groups = groupTracksByTid(tracks);
  delete groups[tid];
  const filteredTracks = Object.values(groups).map((g) => g.tracks).flat(1);

  try {
    const handles = await loadHandles();
    delete handles[tid];
    await saveHandles(handles);
  } catch {
    // IDB cleanup failure must not prevent track removal
  }

  return filteredTracks;
}

/**
 * @type {Actions['groupConsult']}
 */
export async function groupConsult(uris) {
  const groups = groupUrisByTid(uris);
  const handles = await loadHandles();

  const promises = Object.entries(groups).flatMap(async ([tid, { uris }]) => {
    const handle = handles[tid];
    if (!handle) return [];

    const available =
      (await /** @type {any} */ (handle).queryPermission({ mode: "read" })) ===
        "granted";

    /** @type {ConsultGrouping} */
    const grouping = available ? { available, scheme: SCHEME, uris } : {
      available: false,
      reason: "Permission not granted",
      scheme: SCHEME,
      uris,
    };

    return [{ key: groupKey(SCHEME, tid), grouping }];
  });

  const results = (await Promise.all(promises)).flat(1);
  return Object.fromEntries(results.map((e) => [e.key, e.grouping]));
}

/**
 * @type {Actions['list']}
 */
export async function list(cachedTracks = []) {
  const handles = await loadHandles();
  const now = new Date().toISOString();

  /** @type {Record<string, Track>} */
  const cacheByUri = {};

  cachedTracks.forEach((t) => {
    cacheByUri[t.uri] = t;
  });

  const trackGroups = groupTracksByTid(cachedTracks);

  const allTids = new Set(Object.keys(trackGroups));

  const promises = [...allTids].map(async (tid) => {
    const handle = handles[tid];
    if (!handle) return trackGroups[tid]?.tracks ?? /** @type {Track[]} */ ([]);

    const perm = await /** @type {any} */ (handle).queryPermission({
      mode: "read",
    });

    if (perm !== "granted") {
      const cached = trackGroups[tid]?.tracks[0];

      /** @type {Track} */
      const placeholder = {
        $type: "sh.diffuse.output.track",
        id: cached?.id ?? TID.now(),
        createdAt: cached?.createdAt ?? now,
        updatedAt: now,
        kind: "placeholder",
        uri: buildURI(tid),
      };

      return [placeholder];
    }

    if (handle.kind === "file") {
      const uri = buildURI(tid);
      const cached = cacheByUri[uri];

      /** @type {Track} */
      const track = {
        $type: "sh.diffuse.output.track",
        id: cached?.id ?? TID.now(),
        createdAt: cached?.createdAt ?? now,
        updatedAt: cached?.updatedAt ?? now,
        stats: cached?.stats,
        tags: cached?.tags,
        uri,
      };

      return [track];
    }

    const paths = await enumerateAudioFiles(
      /** @type {FileSystemDirectoryHandle} */ (handle),
    );

    if (!paths.length) {
      /** @type {Track} */
      const placeholder = {
        $type: "sh.diffuse.output.track",
        id: TID.now(),
        createdAt: now,
        updatedAt: now,
        kind: "placeholder",
        uri: buildURI(tid),
      };

      return [placeholder];
    }

    return paths.map((path) => {
      const uri = buildURI(tid, path);
      const cached = cacheByUri[uri];

      /** @type {Track} */
      const track = {
        $type: "sh.diffuse.output.track",
        id: cached?.id ?? TID.now(),
        createdAt: cached?.createdAt ?? now,
        updatedAt: cached?.updatedAt ?? now,
        stats: cached?.stats,
        tags: cached?.tags,
        uri,
      };

      return track;
    });
  });

  const tracks = (await Promise.all(promises)).flat(1);
  return tracks;
}

/**
 * @type {Actions['resolve']}
 */
export async function resolve({ uri }) {
  const parsed = parseURI(uri);
  if (!parsed) return undefined;

  const handles = await loadHandles();
  const handle = handles[parsed.tid];
  const path = parsed.path.replace(/^\//, "");

  if (!handle) return undefined;
  if (handle.kind === "directory" && path === "") return undefined;

  const fileHandle = await getHandleFile(handle, path);
  const file = await fileHandle.getFile();

  const url = URL.createObjectURL(file);
  return { url, expiresAt: Infinity };
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, {
    artwork,
    consult,
    detach,
    groupConsult,
    list,
    resolve,
  });
});
