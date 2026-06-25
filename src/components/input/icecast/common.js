import { IcyParser } from "@cloudradio/icy-parser";
import { cachedConsult } from "~/components/input/common.js";

import { SCHEME } from "./constants.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 */

/**
 * Build an icecast:// URI from an HTTP or HTTPS URL.
 * HTTP streams are encoded with a `tls=0` query parameter; HTTPS is the default.
 *
 * @param {string} streamUrl
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
 * @example Build URI from HTTP URL
 * ```ts
 * import { expect } from "@std/expect";
 * import { buildURI } from "./common.js";
 *
 * const uri = buildURI("http://radio.example.com:8000/live");
 * expect(uri).toBe("icecast://radio.example.com:8000/live?tls=0");
 * ```
 */
export function buildURI(streamUrl) {
  const url = new URL(streamUrl);
  const tls = url.protocol === "https:";
  const query = tls ? url.search : `${url.search ? url.search + "&" : "?"}tls=0`;
  return `${SCHEME}://${url.host}${url.pathname}${query}`;
}

/**
 * Parse an icecast:// URI.
 * Returns the resolved HTTP or HTTPS stream URL based on the `tls` query param
 * (absent or `tls=1` → HTTPS; `tls=0` → HTTP).
 *
 * @param {string} uriString
 * @returns {{ host: string; path: string; streamUrl: string } | undefined}
 *
 * @example Parse a valid icecast URI (defaults to HTTPS)
 * ```ts
 * import { expect } from "@std/expect";
 * import { parseURI } from "./common.js";
 *
 * const result = parseURI("icecast://radio.example.com/stream.mp3");
 * expect(result?.host).toBe("radio.example.com");
 * expect(result?.path).toBe("/stream.mp3");
 * expect(result?.streamUrl).toBe("https://radio.example.com/stream.mp3");
 * ```
 *
 * @example Parse icecast URI for an HTTP stream
 * ```ts
 * import { expect } from "@std/expect";
 * import { parseURI } from "./common.js";
 *
 * const result = parseURI("icecast://radio.example.com:8000/live?tls=0");
 * expect(result?.host).toBe("radio.example.com:8000");
 * expect(result?.streamUrl).toBe("http://radio.example.com:8000/live");
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

    const tls = url.searchParams.get("tls") !== "0";
    const protocol = tls ? "https" : "http";

    // Strip the tls param from the forwarded URL's search string
    const params = new URLSearchParams(url.search);
    params.delete("tls");
    const search = params.size > 0 ? `?${params}` : "";

    return {
      host: url.host,
      path: url.pathname,
      streamUrl: `${protocol}://${url.host}${url.pathname}${search}`,
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
 * @param {string} streamUrl
 * @returns {Promise<import("@cloudradio/icy-parser").IcyMetadata | undefined>}
 */
export async function fetchMetadata(streamUrl) {
  try {
    const parser = new IcyParser(streamUrl);
    return await parser.parseOnce();
  } catch {
    return undefined;
  }
}

/**
 * @param {string} uri
 * @returns {Promise<import("@specs/components/input/types.d.ts").ConsultResult>}
 */
async function consultStream(uri) {
  const parsed = parseURI(uri);
  if (!parsed) return "no";
  const metadata = await fetchMetadata(parsed.streamUrl);
  // `fetchMetadata` swallows transport/parse errors as `undefined`;
  // treat that as inconclusive so a network blip doesn't hide the
  // stream for the full consult TTL.
  return metadata === undefined ? "unsure" : "yes";
}

export const consultStreamCached = cachedConsult(
  consultStream,
  (uri) => parseURI(uri)?.host ?? uri,
);
