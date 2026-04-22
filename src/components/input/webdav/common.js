import { parse as parseXml } from "@std/xml";
import * as URI from "fast-uri";
import QS from "query-string";

import { cachedConsult } from "~/components/input/common.js";
import { SCHEME } from "./constants.js";

/**
 * @import { Track } from "~/definitions/types.d.ts";
 * @import { Server } from "./types.d.ts";
 */

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

/**
 * Build an HTTP(S) URL with credentials in a query param for the service worker to intercept.
 * Credentials go in `?diffuse:basic-auth=<base64>` rather than `user:pass@host` because browsers
 * block `new Request()` with credentials in the URL authority (which music-metadata uses).
 *
 * @param {Server} server
 * @param {string} [filePath]
 */
export function buildTrackUrl(server, filePath = "") {
  const url = new URL(toHttpUrl(server, filePath));
  url.searchParams.set(
    "diffuse:basic-auth",
    btoa(unescape(encodeURIComponent(`${server.username}:${server.password}`))),
  );
  return url.href;
}

/**
 * @param {Server} server
 */
export function serverId(server) {
  return `${server.username}:${server.password}@${server.host}${server.dir}`;
}

/**
 * @param {Server} server
 * @param {string} [filePath]
 */
export function buildURI(server, filePath = "") {
  let host = server.host;
  let protocol;

  if (host.includes("://")) {
    [protocol, host] = host.split("://");
  }

  return URI.serialize({
    scheme: SCHEME,
    userinfo: `${encodeURIComponent(server.username)}:${
      encodeURIComponent(server.password)
    }`,
    host,
    path: filePath,
    query: QS.stringify({ dir: server.dir, protocol }),
  });
}

/**
 * @param {string} uriString
 * @returns {{ server: Server; path: string } | undefined}
 */
export function parseURI(uriString) {
  const uri = URI.parse(uriString);
  if (uri.scheme !== SCHEME) return undefined;
  if (!uri.host) return undefined;

  const userinfo = uri.userinfo ?? "";
  const colonIdx = userinfo.indexOf(":");
  const username = decodeURIComponent(
    colonIdx >= 0 ? userinfo.slice(0, colonIdx) : userinfo,
  );
  const password = decodeURIComponent(
    colonIdx >= 0 ? userinfo.slice(colonIdx + 1) : "",
  );

  const qs = QS.parse(uri.query || "");
  const dir = typeof qs.dir === "string" ? qs.dir : "/";
  const protocol = typeof qs.protocol === "string" ? qs.protocol : undefined;

  const rawHost = uri.port ? `${uri.host}:${uri.port}` : uri.host;
  const host = protocol ? `${protocol}://${rawHost}` : rawHost;
  const server = { username, password, host, dir };
  const path = uri.path || "";

  return { server, path };
}

/**
 * @param {Server} server
 * @param {string} [path]
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
 * @param {Server} server
 */
export function authHeader(server) {
  return `Basic ${
    btoa(unescape(encodeURIComponent(`${server.username}:${server.password}`)))
  }`;
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
 */
async function checkAccess(server) {
  try {
    const url = toHttpUrl(server, server.dir);
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), 5000);

    const response = await fetch(url, {
      method: "PROPFIND",
      headers: {
        "Authorization": authHeader(server),
        "Depth": "0",
      },
      signal: controller.signal,
    });

    clearTimeout(timeoutId);
    return response.status === 207 || response.ok;
  } catch {
    return false;
  }
}

export const checkAccessCached = cachedConsult(checkAccess, serverId);

/**
 * List all files on a WebDAV server under server.dir.
 * Uses Depth:1 and recurses into subdirectories to avoid loading the
 * entire tree in one response.
 *
 * @param {Server} server
 * @returns {Promise<string[]>}
 */
export async function listFiles(server) {
  const paths = /** @type {string[]} */ ([]);
  await propfindDir(server, server.dir, paths);
  return paths;
}

/**
 * @param {Server} server
 * @param {string} dir
 * @param {string[]} paths
 */
async function propfindDir(server, dir, paths) {
  const url = toHttpUrl(server, dir);

  const response = await fetch(url, {
    method: "PROPFIND",
    headers: {
      "Authorization": authHeader(server),
      "Depth": "1",
    },
  });

  if (response.status !== 207 && !response.ok) return;

  const xml = await response.text();
  const subdirs = /** @type {string[]} */ ([]);

  const doc = parseXml(xml);
  const multistatus = doc.root;
  if (!multistatus) return;

  for (const node of multistatus.children ?? []) {
    if (node.type !== "element" || node.name.local !== "response") continue;

    let href = "";
    let isCollection = false;

    for (const child of node.children ?? []) {
      if (child.type !== "element") continue;

      if (child.name.local === "href") {
        href = (child.children?.find((n) => n.type === "text")?.text ?? "")
          .trim();
      } else if (child.name.local === "propstat") {
        if (propstatHasCollection(child)) isCollection = true;
      }
    }

    if (!href) continue;

    // Trailing slash is the most reliable collection indicator in WebDAV
    isCollection = isCollection || href.endsWith("/");

    // Keep the raw (percent-encoded) pathname for recursion so that
    // toHttpUrl produces a valid URL; decode only for the final paths list.
    let rawPath;
    try {
      rawPath = new URL(href).pathname;
    } catch {
      rawPath = href;
    }
    const path = decodeURIComponent(rawPath);

    // Skip the directory entry itself.
    // Normalise both sides to have a leading slash — server hrefs always do,
    // but `dir` may not when the user omitted the leading slash in the form.
    const normPath = path.replace(/\/$/, "");
    const normDir = ("/" + decodeURIComponent(dir).replace(/^\//, "")).replace(
      /\/$/,
      "",
    );
    if (normPath === normDir) continue;

    // Skip Synology extended-attribute metadata folders
    if (path.split("/").includes("@eaDir")) continue;

    if (isCollection) {
      subdirs.push(rawPath);
    } else {
      paths.push(path);
    }
  }

  for (const subdir of subdirs) {
    await propfindDir(server, subdir, paths);
  }
}

/**
 * Check propstat > prop > resourcetype > collection (DAV spec path).
 * Using `||=` in the caller means multiple propstat elements don't overwrite a true result.
 *
 * @param {{ children?: ReadonlyArray<{ type: string; name?: { local: string }; children?: ReadonlyArray<any> }> }} propstat
 * @returns {boolean}
 */
function propstatHasCollection(propstat) {
  for (const prop of propstat.children ?? []) {
    if (prop.type !== "element" || prop.name?.local !== "prop") continue;
    for (const child of prop.children ?? []) {
      if (child.type !== "element" || child.name?.local !== "resourcetype") {
        continue;
      }
      for (const rt of child.children ?? []) {
        if (rt.type === "element" && rt.name?.local === "collection") {
          return true;
        }
      }
    }
  }
  return false;
}
