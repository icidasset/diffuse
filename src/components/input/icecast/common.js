import { IcyParser } from "@cloudradio/icy-parser";
import { cachedConsult } from "~/components/input/common.js";

import { SCHEME } from "./constants.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 */

/**
 * Build an icecast:// URI from an HTTPS URL.
 *
 * @param {string} httpsUrl
 * @returns {string}
 *
 * @example Build URI from HTTPS URL
 * ```ts
 * import { expect } from "@std/expect";
 * import { buildURI } from "./common.js";
 *
 * const uri = buildURI("https://radio.example.com/stream.mp3");
 * expect(uri).toBe("icecast://radio.example.com/stream.mp3");
 * ```
 *
 * @example Build URI with port
 * ```ts
 * import { expect } from "@std/expect";
 * import { buildURI } from "./common.js";
 *
 * const uri = buildURI("https://radio.example.com:8000/live");
 * expect(uri).toBe("icecast://radio.example.com:8000/live");
 * ```
 */
export function buildURI(httpsUrl) {
  const url = new URL(httpsUrl);
  return `${SCHEME}://${url.host}${url.pathname}${url.search}`;
}

/**
 * Parse an icecast:// URI.
 *
 * @param {string} uriString
 * @returns {{ host: string; path: string; httpsUrl: string } | undefined}
 *
 * @example Parse a valid icecast URI
 * ```ts
 * import { expect } from "@std/expect";
 * import { parseURI } from "./common.js";
 *
 * const result = parseURI("icecast://radio.example.com/stream.mp3");
 * expect(result?.host).toBe("radio.example.com");
 * expect(result?.path).toBe("/stream.mp3");
 * expect(result?.httpsUrl).toBe("https://radio.example.com/stream.mp3");
 * ```
 *
 * @example Parse icecast URI with port
 * ```ts
 * import { expect } from "@std/expect";
 * import { parseURI } from "./common.js";
 *
 * const result = parseURI("icecast://radio.example.com:8000/live");
 * expect(result?.host).toBe("radio.example.com:8000");
 * expect(result?.httpsUrl).toBe("https://radio.example.com:8000/live");
 * ```
 *
 * @example Reject non-icecast URI
 * ```ts
 * import { expect } from "@std/expect";
 * import { parseURI } from "./common.js";
 *
 * const result = parseURI("https://radio.example.com/stream.mp3");
 * expect(result).toBeUndefined();
 * ```
 */
export function parseURI(uriString) {
  try {
    const url = new URL(uriString);
    if (url.protocol !== `${SCHEME}:`) return undefined;

    return {
      host: url.host,
      path: url.pathname,
      httpsUrl: `https://${url.host}${url.pathname}${url.search}`,
    };
  } catch {
    return undefined;
  }
}

/**
 * Group tracks by host.
 *
 * @param {Track[]} tracks
 * @returns {Record<string, { host: string; tracks: Track[] }>}
 */
export function groupTracksByHost(tracks) {
  /** @type {Record<string, { host: string; tracks: Track[] }>} */
  const acc = {};

  tracks.forEach((track) => {
    const parsed = parseURI(track.uri);
    if (!parsed) return;

    const { host } = parsed;
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

    const { host } = parsed;
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
 */
export function hostsFromTracks(tracks) {
  /** @type {Record<string, string>} */
  const acc = {};

  tracks.forEach((track) => {
    const parsed = parseURI(track.uri);
    if (!parsed) return;

    const { host } = parsed;
    if (acc[host]) return;
    acc[host] = host;
  });

  return acc;
}

/**
 * Fetch ICY metadata from an Icecast stream.
 * Returns undefined if the stream is unreachable or does not support ICY metadata.
 *
 * @param {string} httpsUrl
 * @returns {Promise<import("@cloudradio/icy-parser").IcyMetadata | undefined>}
 */
export async function fetchMetadata(httpsUrl) {
  try {
    const parser = new IcyParser(httpsUrl);
    return await parser.parseOnce();
  } catch {
    return undefined;
  }
}

/** @param {string} uri */
async function consultStream(uri) {
  const parsed = parseURI(uri);
  if (!parsed) return false;
  const metadata = await fetchMetadata(parsed.httpsUrl);
  return metadata !== undefined;
}

export const consultStreamCached = cachedConsult(
  consultStream,
  (uri) => new URL(uri.replace(/^icecast:/, "https:")).host,
);
