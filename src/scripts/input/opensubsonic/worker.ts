import { SubsonicAPI, type Child } from "subsonic-api";
import * as URI from "uri-js";

import type { Consult, ConsultGrouping, GroupConsult, Track } from "@applets/core/types.d.ts";
import { SCHEME } from "./constants.ts";
import {
  autoTypeToTrackKind,
  buildURI,
  consultServer,
  createClient,
  groupTracksByServer,
  loadServers,
  parseURI,
  serverId,
  serversFromTracks,
} from "./common.ts";
import { provide, transfer } from "@scripts/common.ts";

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

async function consult(fileUriOrScheme: string): Promise<Consult> {
  if (!fileUriOrScheme.includes(":")) return { supported: true, consult: "undetermined" };

  const parsed = parseURI(fileUriOrScheme);
  if (!parsed) return { supported: true, consult: "undetermined" };

  const consult = await consultServer(parsed.server);
  return { supported: true, consult };
}

async function contextualize(tracks: Track[]) {
  return serversFromTracks(tracks);
}

async function groupConsult(tracks: Track[]): Promise<GroupConsult> {
  const groups = groupTracksByServer(tracks);

  const promises = Object.entries(groups).map(async ([serverId, { server, tracks }]) => {
    const available = await consultServer(server);
    const grouping: ConsultGrouping = available
      ? { available, tracks }
      : { available, reason: "Server ping failed", tracks };

    return {
      // key: `${SCHEME}:${serverId}`,
      key: SCHEME,
      grouping,
    };
  });

  const entries = (await Promise.all(promises)).map((entry) => [entry.key, entry.grouping]);
  const obj = Object.fromEntries(entries);

  return transfer(obj);
}

async function list(cachedTracks: Track[] = []) {
  const cache: Record<string, Record<string, Track>> = {};

  cachedTracks.forEach((t: Track) => {
    const parsed = parseURI(t.uri);
    if (!parsed || !parsed.path) return;

    const sid = serverId(parsed?.server);

    cache[sid] ??= {};
    cache[sid][URI.unescapeComponent(parsed.path)] = t;
  });

  async function search(client: SubsonicAPI, offset = 0): Promise<Child[]> {
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
          ? song.path.startsWith("/")
            ? song.path
            : `/${song.path}`
          : undefined;

        const fromCache = path ? cache[sid]?.[path] : undefined;
        if (fromCache) return fromCache;

        const track: Track = {
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
  return transfer(tracks);
}

async function resolve({ uri }: { method: string; uri: string }) {
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
