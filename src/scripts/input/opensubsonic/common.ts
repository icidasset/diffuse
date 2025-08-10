import { SubsonicAPI, type Child } from "subsonic-api";
import * as IDB from "idb-keyval";
import * as URI from "uri-js";
import QS from "query-string";

import type { Server } from "./types";
import { IDB_SERVERS, SCHEME } from "./constants";
import type { Track } from "@applets/core/types";

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////
export function autoTypeToTrackKind(type: Child["type"]): Track["kind"] {
  switch (type?.toLowerCase()) {
    case "audiobook":
      return "audiobook";

    case "music":
      return "music";

    case "podcast":
      return "podcast";

    default:
      return "miscellaneous";
  }
}

export function buildURI(server: Server, args: { songId: string; path?: string }) {
  return URI.serialize({
    scheme: SCHEME,
    userinfo: server.apiKey
      ? URI.escapeComponent(server.apiKey)
      : `${URI.escapeComponent(server.username || "")}:${URI.escapeComponent(server.password || "")}`,
    host: server.host.replace(/^https?:\/\//, ""),
    path: args.path,
    query: QS.stringify({
      songId: args.songId,
      tls: server.tls ? "t" : "f",
    }),
  });
}

export async function consultServer(server: Server) {
  const client = createClient(server);
  const resp = await client.ping().catch(() => undefined);

  return resp?.status?.toLowerCase() === "ok";
}

export function createClient(server: Server) {
  return new SubsonicAPI({
    url: `http${server.tls ? "s" : ""}://${server.host}`,
    auth: server.apiKey
      ? { apiKey: URI.unescapeComponent(server.apiKey) }
      : {
          username: URI.unescapeComponent(server.username || ""),
          password: URI.unescapeComponent(server.password || ""),
        },
  });
}

export function groupTracksByServer(tracks: Track[]) {
  const acc: Record<string, { server: Server; tracks: Track[] }> = {};

  tracks.forEach((track: Track) => {
    const parsed = parseURI(track.uri);
    if (!parsed) return;

    const id = serverId(parsed.server);

    if (acc[id]) {
      acc[id].tracks.push(track);
    } else {
      acc[id] = { server: parsed.server, tracks: [track] };
    }
  });

  return acc;
}

export async function loadServers(): Promise<Record<string, Server>> {
  const i = await IDB.get(IDB_SERVERS);
  return i ? i : {};
}

export function parseURI(
  uriString: string,
): { path: string | undefined; server: Server; songId: string | undefined } | undefined {
  const uri = URI.parse(uriString);
  if (uri.scheme !== SCHEME) return undefined;
  if (!uri.host) return undefined;

  let apiKey: string | undefined = undefined;
  let username: string | undefined = undefined;
  let password: string | undefined = undefined;

  if (uri.userinfo?.includes(":")) {
    // Username + Password
    const [u, p] = uri.userinfo.split(":");
    username = u;
    password = p;
    if (!username || !password) return undefined;
  } else {
    // API key
    apiKey = uri.userinfo;
    if (!apiKey) return undefined;
  }

  const qs = QS.parse(uri.query || "");

  const server = {
    apiKey,
    host: uri.port ? `${uri.host}:${uri.port}` : uri.host,
    password,
    tls: qs.tls === "f" ? false : true,
    username,
  };

  const path = uri.path;
  const songId = typeof qs.songId === "string" ? qs.songId : undefined;

  return { path, server, songId };
}

export async function saveServers(items: Record<string, Server>) {
  await IDB.set(IDB_SERVERS, items);
}

export function serversFromTracks(tracks: Track[]) {
  const acc: Record<string, Server> = {};

  tracks.forEach((track: Track) => {
    const parsed = parseURI(track.uri);
    if (!parsed) return;

    const id = serverId(parsed.server);
    if (acc[id]) return;

    acc[id] = parsed.server;
  });

  return acc;
}

export function serverId(server: Server) {
  const parts = {
    host: server.host,
    query: `tls=${server.tls ? "t" : "f"}`,
  };

  const uri = server.apiKey
    ? URI.serialize({ ...parts, userinfo: server.apiKey })
    : URI.serialize({ ...parts, userinfo: `${server.username}:${server.password}` });

  return btoa(uri);
}
