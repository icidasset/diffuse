import * as TID from "@atcute/tid";
import { ostiary, rpc } from "~/common/worker.js";
import {
  bytesFromUrl,
  detach as detachUtil,
  groupKey,
  isAudioFile,
  isImageFile,
  pickCoverArt,
} from "~/components/input/common.js";
import { safeDecodeURIComponent } from "~/common/utils.js";

import {
  buildURI,
  checkAccessCached,
  groupTracksByServer,
  groupUrisByServer,
  listFiles,
  listImageFilesInDir,
  parseURI,
  serverId,
  toHttpUrl,
} from "./common.js";
import { SCHEME } from "./constants.js";

/**
 * @import { InputActions as Actions, ConsultGrouping } from "@specs/components/input/types.d.ts";
 * @import { Track } from "~/definitions/types.d.ts";
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions['artwork']}
 */
export async function artwork(uri) {
  try {
    const parsed = parseURI(uri);
    if (!parsed || !parsed.path) return null;

    // Parent directory of the audio file, relative to server.dir.
    const lastSlash = parsed.path.lastIndexOf("/");
    const dirPath = parsed.server.dir +
      (lastSlash > 0 ? parsed.path.slice(0, lastSlash + 1) : "");

    const fileNames = await listImageFilesInDir(parsed.server, dirPath);
    if (fileNames === null) return null;

    const images = fileNames.filter((name) => isImageFile(name));
    const imageName = pickCoverArt(images, (name) => name);
    if (!imageName) return null;

    // `dirPath` is already percent-encoded (it comes from `parsed.path`);
    // encode just the image filename, mirroring how `walkEntries` builds paths.
    const url = toHttpUrl(
      parsed.server,
      dirPath + encodeURIComponent(imageName),
    );
    return await bytesFromUrl(url);
  } catch {
    // No sibling artwork found / fetch failed — `null` means "none".
    return null;
  }
}

/**
 * @type {Actions['consult']}
 */
export async function consult(fileUriOrScheme) {
  if (!fileUriOrScheme.includes(":")) {
    return { supported: true, consult: "undetermined" };
  }

  const parsed = parseURI(fileUriOrScheme);
  if (!parsed) return { supported: true, consult: "undetermined" };

  const accessible = await checkAccessCached(parsed.server);
  return { supported: true, consult: accessible };
}

/**
 * @type {Actions['detach']}
 */
export async function detach(args) {
  return detachUtil({
    ...args,

    inputScheme: SCHEME,
    handleFileUri: ({ fileURI, tracks }) => {
      const result = parseURI(fileURI);
      if (!result) return tracks;

      const id = serverId(result.server);
      const groups = groupTracksByServer(tracks);

      delete groups[id];

      return Object.values(groups).map((g) => g.tracks).flat(1);
    },
  });
}

/**
 * @type {Actions['groupConsult']}
 */
export async function groupConsult(uris) {
  const groups = groupUrisByServer(uris);

  const promises = Object.entries(groups).map(
    async ([id, { server, uris }]) => {
      const available = await checkAccessCached(server);

      /** @type {ConsultGrouping} */
      const grouping = available === "yes"
        ? { available, scheme: SCHEME, uris }
        : { available, reason: "Server unreachable", scheme: SCHEME, uris };

      return { key: groupKey(SCHEME, id), grouping };
    },
  );

  const entries = (await Promise.all(promises)).map((e) => [e.key, e.grouping]);
  return Object.fromEntries(entries);
}

/**
 * @type {Actions['list']}
 */
export async function list(cachedTracks = []) {
  /** @type {Record<string, Record<string, Track>>} */
  const cache = {};

  const groups = groupTracksByServer(cachedTracks);

  Object.entries(groups).forEach(([id, { tracks }]) => {
    tracks.forEach((track) => {
      const parsed = parseURI(track.uri);
      if (!parsed) return;

      if (!cache[id]) cache[id] = {};
      cache[id][safeDecodeURIComponent(parsed.path)] = track;
    });
  });

  const promises = Object.entries(groups).map(
    async ([id, { server, tracks: cachedServerTracks }]) => {
      const files = await listFiles(server);

      // `listFiles` returns `null` when the root directory listing could
      // not be fetched at all (e.g. the server was briefly unreachable
      // right after a laptop wake). In that case, preserve the previously
      // cached tracks for this server rather than replacing them with a
      // single placeholder — otherwise an interrupted refresh would wipe
      // the user's library and cascade into an empty browser view.
      if (files === null) {
        if (cachedServerTracks.length) return cachedServerTracks;

        const now = new Date().toISOString();
        return [/** @type {Track} */ ({
          $type: "sh.diffuse.output.track",
          id: TID.now(),
          createdAt: now,
          updatedAt: now,
          kind: "placeholder",
          uri: buildURI(server),
        })];
      }

      let tracks = files
        .filter((path) => isAudioFile(path))
        .map((path) => {
          const cachedTrack = cache[id]?.[safeDecodeURIComponent(path)];

          const trackId = cachedTrack?.id || TID.now();
          const stats = cachedTrack?.stats;
          const tags = cachedTrack?.tags;
          const now = new Date().toISOString();

          /** @type {Track} */
          const track = {
            $type: "sh.diffuse.output.track",
            id: trackId,
            createdAt: cachedTrack?.createdAt ?? now,
            updatedAt: cachedTrack?.updatedAt ?? now,
            stats,
            tags,
            uri: buildURI(server, path),
          };

          return track;
        });

      if (!tracks.length) {
        const now = new Date().toISOString();

        tracks = [{
          $type: "sh.diffuse.output.track",
          id: TID.now(),
          createdAt: now,
          updatedAt: now,
          kind: "placeholder",
          uri: buildURI(server),
        }];
      }

      return tracks;
    },
  );

  return (await Promise.all(promises)).flat(1);
}

/**
 * @type {Actions['resolve']}
 */
export async function resolve({ uri }) {
  const parsed = parseURI(uri);
  if (!parsed || !parsed.path) return undefined;

  const url = toHttpUrl(parsed.server, parsed.path);
  const expiresAt = Math.round(Date.now() / 1000) + 60 * 60 * 24 * 365;

  return { url, expiresAt };
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
