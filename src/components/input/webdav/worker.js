import * as TID from "@atcute/tid";
import { ostiary, rpc } from "~/common/worker.js";
import {
  detach as detachUtil,
  groupKey,
  isAudioFile,
} from "~/components/input/common.js";
import { safeDecodeURIComponent } from "~/common/utils.js";

import {
  buildTrackUrl,
  buildURI,
  checkAccessCached,
  groupTracksByServer,
  groupUrisByServer,
  listFiles,
  parseURI,
  serverId,
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
export async function artwork(_uri) {
  return null;
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
        : { available, reason: "WebDAV server unreachable", scheme: SCHEME, uris };

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

  const promises = Object.entries(groups).map(async ([id, { server }]) => {
    const files = await listFiles(server);

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
  });

  return (await Promise.all(promises)).flat(1);
}

/**
 * @type {Actions['resolve']}
 */
export async function resolve({ uri }) {
  const parsed = parseURI(uri);
  if (!parsed || !parsed.path) return undefined;

  const url = buildTrackUrl(parsed.server, parsed.path);
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
