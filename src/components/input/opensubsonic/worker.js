import * as URI from "uri-js";

import { effect, signal } from "@common/signal.js";
import { announce, define, ostiary } from "@common/worker.js";

import { SCHEME } from "./constants.js";
import {
  autoTypeToTrackKind,
  buildURI,
  consultServer,
  createClient,
  groupTracksByServer,
  loadServers,
  parseURI,
  saveServers,
  serverId,
  serversFromTracks,
} from "./common.js";

/**
 * @import {Child, SubsonicAPI} from "subsonic-api"
 * @import {ConsultGrouping, InputActions as Actions, Track} from "@components/core/types.d.ts";
 * @import {Server} from "./types.d.ts"
 */

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

const $servers = signal(/** @type {Record<string, Server>} */ ({}));

effect(() => {
  saveServers($servers.value);
});

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

  const consult = await consultServer(parsed.server);
  return { supported: true, consult };
}

/**
 * @type {Actions['contextualize']}
 */
export async function contextualize(tracks) {
  const servers = serversFromTracks(tracks);
  $servers.value = servers;
}

/**
 * @type {Actions['groupConsult']}
 */
export async function groupConsult(tracks) {
  const groups = groupTracksByServer(tracks);

  const promises = Object.entries(groups).map(
    async ([serverId, { server, tracks }]) => {
      const available = await consultServer(server);

      /** @type {ConsultGrouping} */
      const grouping = available
        ? { available, tracks }
        : { available, reason: "Server ping failed", tracks };

      return {
        // key: `${SCHEME}:${serverId}`,
        key: SCHEME,
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

  cachedTracks.forEach((t) => {
    const parsed = parseURI(t.uri);
    if (!parsed || !parsed.path) return;

    const sid = serverId(parsed?.server);

    cache[sid] ??= {};
    cache[sid][URI.unescapeComponent(parsed.path)] = t;
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

  const servers = await loadServers();
  const promises = Object.values(servers).map(async (server) => {
    const client = createClient(server);
    const sid = serverId(server);
    const list = await search(client, 0);

    return list
      .filter((song) => !song.isVideo)
      .map((song) => {
        const path = song.path
          ? song.path.startsWith("/") ? song.path : `/${song.path}`
          : undefined;

        const fromCache = path ? cache[sid]?.[path] : undefined;
        if (fromCache) return fromCache;

        /** @type {Track} */
        const track = {
          id: crypto.randomUUID(),
          kind: autoTypeToTrackKind(song.type),
          uri: buildURI(server, { songId: song.id, path }),

          stats: {
            bitrate: song.bitRate,
            duration: song.duration,
          },
          tags: {
            album: song.album,
            artist: song.artist,
            disc: { no: song.discNumber || 1 },
            genre: song.genre,
            title: song.title,
            track: { no: song.track || 1 },
            year: song.year,
          },
        };

        return track;
      });
  });

  const tracks = (await Promise.all(promises)).flat(1);
  if (tracks.length) return tracks;

  // If a server didn't have any tracks,
  // keep a placeholder track so the server gets
  // picked up whenever it is re-contextualized.
  return Object.values(servers).map((server) => {
    return {
      id: crypto.randomUUID(),
      kind: "placeholder",
      uri: buildURI(server),
    };
  });
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

  // TODO:
  // const expiresInSeconds = 60 * 60 * 24 * 7; // 7 days
  // const expiresAtSeconds = Math.round(Date.now() / 1000) + expiresInSeconds;

  const url = await client
    .download({
      id: songId,
      format: "raw",
    })
    .then((a) => a.url);

  return { expiresAt: Infinity, url };
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((port) => {
  // Setup RPC

  define("servers", $servers.get, port);

  define("consult", consult, port);
  define("contextualize", contextualize, port);
  define("groupConsult", groupConsult, port);
  define("list", list, port);
  define("resolve", resolve, port);

  // Communicate state

  effect(() => announce("servers", $servers.value, port));
});
