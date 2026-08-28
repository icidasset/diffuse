import * as URI from "fast-uri";
import QS from "query-string";

import { cachedConsult } from "~/components/input/common.js";
import { SCHEME } from "./constants.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 */

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

/**
 * @typedef {{ host: string; dir: string; exclude?: string[] }} Server
 */

/**
 * @param {Server} server
 * @returns {string}
 */
export function serverId(server) {
  return `${server.host}${server.dir}`;
}

/**
 * Build a https-json:// URI.
 * Protocol can be embedded in host (eg. http://localhost:8080) and is stored
 * as a query param so the URI authority stays valid.
 *
 * @param {Server} server
 * @param {string} [path]
 * @returns {string}
 */
export function buildURI(server, path = "") {
  let host = server.host;
  let protocol;

  if (host.includes("://")) {
    [protocol, host] = host.split("://");
  }

  const exclude = server.exclude?.length ? server.exclude.join(",") : undefined;
  const query = QS.stringify({ dir: server.dir, exclude, protocol });
  return `${SCHEME}://${host}${path}${query ? `?${query}` : ""}`;
}

/**
 * @param {string} uriString
 * @returns {{ server: Server; path: string } | undefined}
 */
export function parseURI(uriString) {
  const uri = URI.parse(uriString);
  if (uri.scheme !== SCHEME) return undefined;
  if (!uri.host) return undefined;

  const qs = QS.parse(uri.query || "");
  const dir = typeof qs.dir === "string" ? qs.dir : "/";
  const protocol = typeof qs.protocol === "string" ? qs.protocol : undefined;
  const exclude = typeof qs.exclude === "string"
    ? qs.exclude.split(",").filter(Boolean)
    : undefined;

  const rawHost = uri.port ? `${uri.host}:${uri.port}` : uri.host;
  const host = protocol ? `${protocol}://${rawHost}` : rawHost;
  const server = { host, dir, exclude };
  const path = uri.path || "";

  return { server, path };
}

/**
 * @param {Server} server
 * @param {string} [path]
 * @returns {string}
 */
export function toHttpUrl(server, path = "") {
  const base = server.host.includes("://")
    ? server.host
    : `${
      server.host.split(":")[0] === "localhost" ||
        server.host.split(":")[0] === "127.0.0.1"
        ? "http"
        : "https"
    }://${server.host}`;

  return base.replace(/\/$/, "") + (path ? "/" + path.replace(/^\//, "") : "");
}

/**
 * @param {Track[]} tracks
 * @returns {Record<string, Server>}
 */
export function serversFromTracks(tracks) {
  /** @type {Record<string, Server>} */
  const acc = {};

  tracks.forEach((track) => {
    const parsed = parseURI(track.uri);
    if (!parsed) return;

    const id = serverId(parsed.server);
    if (!acc[id]) acc[id] = parsed.server;
  });

  return acc;
}

/**
 * @param {Track[]} tracks
 * @returns {Record<string, { server: Server; tracks: Track[] }>}
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
 * @param {string[]} uris
 * @returns {Record<string, { server: Server; uris: string[] }>}
 */
export function groupUrisByServer(uris) {
  /** @type {Record<string, { server: Server; uris: string[] }>} */
  const acc = {};

  uris.forEach((uri) => {
    const parsed = parseURI(uri);
    if (!parsed) return;

    const id = serverId(parsed.server);

    if (acc[id]) {
      acc[id].uris.push(uri);
    } else {
      acc[id] = { server: parsed.server, uris: [uri] };
    }
  });

  return acc;
}

/**
 * @param {Server} server
 * @returns {Promise<import("@specs/components/input/types.d.ts").ConsultResult>}
 */
async function checkAccess(server) {
  const url = toHttpUrl(server, server.dir);
  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), 5000);

  try {
    const response = await fetch(url, {
      headers: { "Accept": "application/json" },
      signal: controller.signal,
    });

    clearTimeout(timeoutId);
    return response.ok ? "yes" : "no";
  } catch {
    // Network error or timeout: inconclusive — let `cachedConsult` fall
    // back to the last known availability instead of caching a sticky
    // "no" for the full TTL (which would hide every track from this
    // server for 5 minutes after a laptop wake).
    clearTimeout(timeoutId);
    return "unsure";
  }
}

export const checkAccessCached = cachedConsult(checkAccess, serverId);

/**
 * List all files on the server under server.dir using JSON directory listing.
 * Fetches each directory with `Accept: application/json` and recurses into subdirs.
 *
 * Returns `null` if the root directory listing could not be fetched at all
 * (network error, non-2xx, or non-JSON response) — this signals a transient
 * outage rather than "directory is genuinely empty" so the caller can
 * preserve previously-cached tracks instead of wiping them with a placeholder.
 *
 * @param {Server} server
 * @returns {Promise<string[] | null>}
 */
export async function listFiles(server) {
  const paths = /** @type {string[]} */ ([]);
  const exclude = new Set(server.exclude ?? []);

  let rootEntries;
  try {
    rootEntries = await fetchDirEntries(server, server.dir);
  } catch {
    // Root fetch failed — server unreachable or not a JSON listing.
    // The caller can use this to preserve cached tracks rather than
    // replacing them with a placeholder.
    return null;
  }

  await walkEntries(server, server.dir, rootEntries, paths, exclude);
  return paths;
}

/**
 * @param {Server} server
 * @param {string} dir
 * @returns {Promise<unknown[]>}
 */
async function fetchDirEntries(server, dir) {
  const url = toHttpUrl(server, dir);
  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), 10000);

  let response;
  try {
    response = await fetch(url, {
      headers: { "Accept": "application/json" },
      signal: controller.signal,
    });
  } finally {
    clearTimeout(timeoutId);
  }

  if (!response.ok) throw new Error(`dir fetch failed: ${response.status}`);

  /** @type {unknown} */
  const data = await response.json();
  if (!Array.isArray(data)) throw new Error("dir listing is not an array");
  return /** @type {unknown[]} */ (data);
}

/**
 * @param {Server} server
 * @param {string} dir
 * @param {unknown[]} entries
 * @param {string[]} paths
 * @param {Set<string>} exclude
 */
async function walkEntries(server, dir, entries, paths, exclude) {
  const basePath = dir.endsWith("/") ? dir : dir + "/";

  for (const entry of entries) {
    if (!entry || typeof /** @type {any} */ (entry).name !== "string" ||
      !/** @type {any} */ (entry).type) {
      continue;
    }

    // Encode each path segment so URIs stay valid for non-ASCII filenames.
    const encodedName = encodeURIComponent(/** @type {any} */ (entry).name);
    const entryPath = basePath + encodedName;

    if (/** @type {any} */ (entry).type === "directory") {
      if (exclude.has(/** @type {any} */ (entry).name)) continue;
      // A failing subdir shouldn't fail the whole walk — skip it and
      // keep going, matching the previous tolerant behaviour.
      try {
        const subEntries = await fetchDirEntries(server, entryPath);
        await walkEntries(server, entryPath, subEntries, paths, exclude);
      } catch {
        // Subdir unavailable: skip.
      }
    } else if (/** @type {any} */ (entry).type === "file") {
      paths.push(entryPath);
    }
  }
}
