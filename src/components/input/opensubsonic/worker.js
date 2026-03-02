import * as TID from "@atcute/tid";
import { ostiary, rpc } from "~/common/worker.js";

import { SCHEME } from "./constants.js";
import {
  removeUndefinedValuesFromRecord,
  safeDecodeURIComponent,
} from "~/common/utils.js";
import { detach as detachUtil, groupKey } from "../common.js";
import {
  autoTypeToTrackKind,
  buildURI,
  consultServerCached,
  createClient,
  groupTracksByServer,
  groupUrisByServer,
  parseURI,
  serverId,
} from "./common.js";

/**
 * @import {Child, SubsonicAPI} from "subsonic-api"
 * @import {Track} from "~/definitions/types.d.ts";
 * @import {ConsultGrouping, InputActions as Actions} from "~/components/input/types.d.ts";
 * @import {Server} from "./types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions['consult']}
 */
export async function consult(fileUriOrScheme) {
  if (!fileUriOrScheme.includes(":")) {
    return { supported: true, consult: "undetermined" };
  }

  const parsed = parseURI(fileUriOrScheme);
  if (!parsed) return { supported: true, consult: "undetermined" };

  const consult = await consultServerCached(parsed.server);
  return { supported: true, consult };
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

      const sid = serverId(result.server);
      const groups = groupTracksByServer(tracks);

      delete groups[sid];

      return Object.values(groups).map((a) => a.tracks).flat(1);
    },
  });
}

/**
 * @type {Actions['groupConsult']}
 */
export async function groupConsult(uris) {
  const groups = groupUrisByServer(uris);

  const promises = Object.entries(groups).map(
    async ([serverId, { server, uris }]) => {
      const available = await consultServerCached(server);

      /** @type {ConsultGrouping} */
      const grouping = available
        ? { available, scheme: SCHEME, uris }
        : { available, reason: "Server ping failed", scheme: SCHEME, uris };

      return {
        key: groupKey(SCHEME, serverId),
        grouping,
      };
    },
  );

  const entries = (await Promise.all(promises)).map((
    entry,
  ) => [entry.key, entry.grouping]);

  return Object.fromEntries(entries);
}

/**
 * @type {Actions['list']}
 */
export async function list(cachedTracks = []) {
  /** @type {Record<string, Record<string, Track>>} */
  const cache = {};

  /** @type {Record<string, Server>} */
  const servers = {};

  cachedTracks.forEach((t) => {
    const parsed = parseURI(t.uri);
    if (!parsed || parsed.path === undefined) return;

    const sid = serverId(parsed.server);
    servers[sid] = parsed.server;

    const path = safeDecodeURIComponent(parsed.path);
    cache[sid] ??= {};
    cache[sid][path] = t;
  });

  /**
   * @param {SubsonicAPI} client
   * @returns {Promise<Child[]>}
   */
  async function search(client, offset = 0) {
    const result = await client.search3({
      query: "",
      artistCount: 0,
      albumCount: 0,
      songCount: 1000,
      songOffset: offset,
    });

    const songs = result.searchResult3.song || [];

    if (songs.length === 1000) {
      const moreSongs = await search(client, offset + 1000);
      return [...songs, ...moreSongs];
    }

    return songs;
  }

  const promises = Object.values(servers).map(async (server) => {
    const client = createClient(server);
    const sid = serverId(server);
    const list = await search(client, 0);

    let tracks = list
      .filter((song) => !song.isVideo)
      .map((song) => {
        const path = song.path
          ? song.path.startsWith("/") ? song.path : `/${song.path}`
          : undefined;

        const fromCache = path ? cache[sid]?.[path] : undefined;
        if (fromCache) return fromCache;

        const now = new Date().toISOString();

        /** @type {Track} */
        const track = {
          $type: "sh.diffuse.output.track",
          id: TID.now(),
          createdAt: now,
          updatedAt: now,
          kind: autoTypeToTrackKind(song.type),
          uri: buildURI(server, { songId: song.id, path }),

          stats: removeUndefinedValuesFromRecord({
            albumGain: undefined,
            bitrate: song.bitRate ? Math.round(song.bitRate * 1000) : undefined,
            bitsPerSample: undefined,
            codec: undefined,
            container: undefined,
            duration: song.duration != null
              ? Math.round(song.duration * 1000)
              : undefined,
            lossless: undefined,
            numberOfChannels: undefined,
            sampleRate: undefined,
            trackGain: undefined,
          }),
          tags: removeUndefinedValuesFromRecord({
            album: song.album,
            albumartist: song.albumArtists?.[0]?.name,
            albumartists: song.albumArtists?.map((a) => a.name),
            albumartistsort: song.albumArtists?.[0]?.sortName,
            albumsort: undefined,
            arranger: undefined,
            artist: song.artist ?? song.displayArtist,
            artists: undefined,
            artistsort: undefined,
            asin: undefined,
            averageLevel: undefined,
            barcode: undefined,
            bpm: song.bpm,
            catalognumbers: undefined,
            compilation: undefined,
            composers: song.displayComposer
              ? [song.displayComposer]
              : undefined,
            composersort: undefined,
            conductors: undefined,
            date: undefined,
            disc: {
              no: song.discNumber || 1,
            },
            djmixers: undefined,
            engineers: undefined,
            gapless: undefined,
            genres: song.genres,
            isrc: undefined,
            labels: undefined,
            lyricists: undefined,
            media: undefined,
            mixers: undefined,
            moods: song.moods,
            originaldate: undefined,
            originalyear: undefined,
            peakLevel: undefined,
            producers: undefined,
            publishers: undefined,
            releasecountry: undefined,
            releasedate: undefined,
            releasestatus: undefined,
            releasetypes: undefined,
            remixers: undefined,
            technicians: undefined,
            title: song.title ?? "Unknown",
            titlesort: undefined,
            track: {
              no: song.track ?? 1,
              of: song.size,
            },
            work: undefined,
            writers: undefined,
            year: song.year,
          }),
        };

        return track;
      });

    // If a server didn't have any tracks,
    // keep a placeholder track so the server gets
    // picked up as a source.
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

  const tracks = (await Promise.all(promises)).flat(1);
  return tracks;
}

/**
 * @type {Actions['resolve']}
 */
export async function resolve({ uri }) {
  const parsed = parseURI(uri);
  if (!parsed) return undefined;

  const client = createClient(parsed.server);
  const songId = parsed.songId;
  if (!songId) return undefined;

  const url = await client
    .stream({
      id: songId,
      format: "raw",
    })
    .then((a) => a.url);

  return { expiresAt: Infinity, url };
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  // Setup RPC

  rpc(context, {
    consult,
    detach,
    groupConsult,
    list,
    resolve,
  });
});
