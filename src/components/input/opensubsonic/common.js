import { SubsonicAPI } from "subsonic-api";
import * as IDB from "idb-keyval";
import * as URI from "uri-js";
import QS from "query-string";

import { IDB_SERVERS, SCHEME } from "./constants.js";

/**
 * @import {Child} from "subsonic-api"
 * @import {Server} from "./types.d.ts";
 * @import {Track} from "@common/types.d.ts";
 */

/**
 * @param {Child["type"]} type
 * @returns {Track["kind"]}
 */
export function autoTypeToTrackKind(type) {
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

/**
 * @param {Server} server
 * @param {{ songId: string; path?: string }} [args]
 */
export function buildURI(server, args) {
  return URI.serialize({
    scheme: SCHEME,
    userinfo: server.apiKey
      ? URI.escapeComponent(server.apiKey)
      : `${URI.escapeComponent(server.username || "")}:${
        URI.escapeComponent(server.password || "")
      }`,
    host: server.host.replace(/^https?:\/\//, ""),
    path: args?.path,
    query: QS.stringify({
      songId: args?.songId,
      tls: server.tls ? "t" : "f",
    }),
  });
}

/**
 * @param {Server} server
 */
export async function consultServer(server) {
  const client = createClient(server);
  const resp = await client.ping().catch(() => undefined);

  return resp?.status?.toLowerCase() === "ok";
}

/**
 * @param {Server} server
 */
export function createClient(server) {
  return new SubsonicAPI({
    url: `http${server.tls ? "s" : ""}://${server.host}`,
    auth: server.apiKey ? { apiKey: URI.unescapeComponent(server.apiKey) } : {
      username: URI.unescapeComponent(server.username || ""),
      password: URI.unescapeComponent(server.password || ""),
    },
  });
}

/**
 * @param {Track[]} tracks
 */
export function groupTracksByServer(tracks) {
  /** @type {Record<string, { server: Server; tracks: Track[] }>} */
  const acc = {};

  tracks.forEach((track) => {
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

/**
 * @returns {Promise<Record<string, Server>>}
 */
export async function loadServers() {
  const i = await IDB.get(IDB_SERVERS);
  return i ? i : {};
}

/**
 * Parse an opensubsonic URI.
 *
 * ```
 * opensubsonic://username:password@server-host:port/path?tls=f
 * ```
 *
 * @param {string} uriString
 * @returns {{ path: string | undefined; server: Server; songId: string | undefined } | undefined}
 */
export function parseURI(uriString) {
  const uri = URI.parse(uriString);
  if (uri.scheme !== SCHEME) return undefined;
  if (!uri.host) return undefined;

  let apiKey = undefined;
  let username = undefined;
  let password = undefined;

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

/**
 * @param {Record<string, Server>} items
 */
export async function saveServers(items) {
  await IDB.set(IDB_SERVERS, items);
}

/**
 * @param {Track[]} tracks
 */
export function serversFromTracks(tracks) {
  /** @type {Record<string, Server>} */
  const acc = {};

  tracks.forEach((track) => {
    const parsed = parseURI(track.uri);
    if (!parsed) return;

    const id = serverId(parsed.server);
    if (acc[id]) return;

    acc[id] = parsed.server;
  });

  return acc;
}

/**
 * @param {Server} server
 */
export function serverId(server) {
  const parts = {
    host: server.host,
    query: `tls=${server.tls ? "t" : "f"}`,
  };

  const uri = server.apiKey
    ? URI.serialize({ ...parts, userinfo: server.apiKey })
    : URI.serialize({
      ...parts,
      userinfo: `${server.username}:${server.password}`,
    });

  return btoa(uri);
}
