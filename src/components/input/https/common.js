import { cachedConsult } from "~/components/input/common.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 */

/**
 * Group tracks by host.
 *
 * @param {Track[]} tracks
 * @returns {Record<string, { host: string; tracks: Track[] }>}
 *
 * @example Group tracks by domain
 * ```ts
 * import { expect } from "@std/expect";
 * import { groupTracksByHost } from "./common.js";
 * import type { Track } from "~/definitions/types.d.ts";
 *
 * const tracks: Track[] = [
 *   {
 *     $type: "sh.diffuse.output.track",
 *     id: "1",
 *     uri: "https://example.com/a.mp3",
 *   },
 *   {
 *     $type: "sh.diffuse.output.track",
 *     id: "2",
 *     uri: "https://cdn.example.com/b.mp3",
 *   },
 *   {
 *     $type: "sh.diffuse.output.track",
 *     id: "3",
 *     uri: "https://example.com/c.mp3",
 *   },
 * ];
 *
 * const groups = groupTracksByHost(tracks);
 * expect(Object.keys(groups).length).toBe(2);
 * expect(groups["example.com"].tracks.length).toBe(2);
 * expect(groups["cdn.example.com"].tracks.length).toBe(1);
 * ```
 *
 * @example Group tracks by host including port
 * ```ts
 * import { expect } from "@std/expect";
 * import { groupTracksByHost } from "./common.js";
 * import type { Track } from "~/definitions/types.d.ts";
 *
 * const tracks: Track[] = [
 *   {
 *     $type: "sh.diffuse.output.track",
 *     id: "1",
 *     uri: "https://example.com/a.mp3",
 *   },
 *   {
 *     $type: "sh.diffuse.output.track",
 *     id: "2",
 *     uri: "https://example.com:8443/b.mp3",
 *   },
 * ];
 *
 * const groups = groupTracksByHost(tracks);
 * expect(Object.keys(groups).length).toBe(2);
 * expect(groups["example.com"].tracks.length).toBe(1);
 * expect(groups["example.com:8443"].tracks.length).toBe(1);
 * ```
 */
export function groupTracksByHost(tracks) {
  /** @type {Record<string, { host: string; tracks: Track[] }>} */
  const acc = {};

  tracks.forEach((track) => {
    const parsed = parseURI(track.uri);
    if (!parsed) return;

    const host = parsed.host;

    if (acc[host]) {
      acc[host].tracks.push(track);
    } else {
      acc[host] = { host, tracks: [track] };
    }
  });

  return acc;
}

/**
 * Group URIs by host.
 *
 * @param {string[]} uris
 * @returns {Record<string, { host: string; uris: string[] }>}
 */
export function groupUrisByHost(uris) {
  /** @type {Record<string, { host: string; uris: string[] }>} */
  const acc = {};

  uris.forEach((uri) => {
    const parsed = parseURI(uri);
    if (!parsed) return;

    const host = parsed.host;

    if (acc[host]) {
      acc[host].uris.push(uri);
    } else {
      acc[host] = { host, uris: [uri] };
    }
  });

  return acc;
}

/**
 * Extract unique hosts from tracks.
 *
 * @param {Track[]} tracks
 * @returns {Record<string, string>}
 *
 * @example Extract unique hosts
 * ```ts
 * import { expect } from "@std/expect";
 * import { hostsFromTracks } from "./common.js";
 * import type { Track } from "~/definitions/types.d.ts";
 *
 * const tracks: Track[] = [
 *   {
 *     $type: "sh.diffuse.output.track",
 *     id: "1",
 *     uri: "https://example.com/a.mp3",
 *   },
 *   {
 *     $type: "sh.diffuse.output.track",
 *     id: "2",
 *     uri: "https://example.com/b.mp3",
 *   },
 *   {
 *     $type: "sh.diffuse.output.track",
 *     id: "3",
 *     uri: "https://cdn.example.com/c.mp3",
 *   },
 * ];
 *
 * const hosts = hostsFromTracks(tracks);
 * expect(Object.keys(hosts).length).toBe(2);
 * expect(hosts["example.com"]).toBe("example.com");
 * expect(hosts["cdn.example.com"]).toBe("cdn.example.com");
 * ```
 */
export function hostsFromTracks(tracks) {
  /** @type {Record<string, string>} */
  const acc = {};

  tracks.forEach((track) => {
    const parsed = parseURI(track.uri);
    if (!parsed) return;

    const host = parsed.host;
    if (acc[host]) return;

    acc[host] = host;
  });

  return acc;
}

/**
 * Parse an HTTPS URI.
 * Validates and extracts components from a standard HTTPS URL.
 *
 * @param {string} uriString
 * @returns {{ url: string; domain: string; path: string; host: string } | undefined}
 *
 * @example Parse a valid HTTPS URI
 * ```ts
 * import { expect } from "@std/expect";
 * import { parseURI } from "./common.js";
 *
 * const result = parseURI("https://example.com/song.mp3");
 * expect(result?.domain).toBe("example.com");
 * expect(result?.host).toBe("example.com");
 * expect(result?.path).toBe("/song.mp3");
 * expect(result?.url).toBe("https://example.com/song.mp3");
 * ```
 *
 * @example Parse HTTPS URI with port
 * ```ts
 * import { expect } from "@std/expect";
 * import { parseURI } from "./common.js";
 *
 * const result = parseURI("https://example.com:8443/audio.mp3");
 * expect(result?.domain).toBe("example.com");
 * expect(result?.host).toBe("example.com:8443");
 * expect(result?.path).toBe("/audio.mp3");
 * ```
 *
 * @example Parse HTTPS URI with query parameters
 * ```ts
 * import { expect } from "@std/expect";
 * import { parseURI } from "./common.js";
 *
 * const result = parseURI("https://example.com/song.mp3?token=abc123");
 * expect(result?.domain).toBe("example.com");
 * expect(result?.path).toBe("/song.mp3");
 * expect(result?.url).toContain("token=abc123");
 * ```
 *
 * @example Reject non-HTTPS URI
 * ```ts
 * import { expect } from "@std/expect";
 * import { parseURI } from "./common.js";
 *
 * const result = parseURI("http://example.com/song.mp3");
 * expect(result).toBeUndefined();
 * ```
 *
 * @example Reject invalid URI
 * ```ts
 * import { expect } from "@std/expect";
 * import { parseURI } from "./common.js";
 *
 * const result = parseURI("not-a-url");
 * expect(result).toBeUndefined();
 * ```
 */
export function parseURI(uriString) {
  try {
    const url = new URL(uriString);
    if (url.protocol !== "https:") return undefined;

    return {
      url: url.href,
      domain: url.hostname,
      host: url.host, // includes port if present
      path: url.pathname,
    };
  } catch {
    return undefined;
  }
}

/** @param {string} uri */
async function consultHost(uri) {
  try {
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), 5000);
    const response = await fetch(uri, {
      method: "HEAD",
      signal: controller.signal,
    });
    clearTimeout(timeoutId);
    return response.ok;
  } catch {
    return false;
  }
}

export const consultHostCached = cachedConsult(
  consultHost,
  (uri) => new URL(uri).host,
);
