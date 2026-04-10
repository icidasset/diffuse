import * as URI from "fast-uri";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 */

/**
 * @param {string} uri
 *
 * @example Strips path and query from a URI
 * ```js
 * import { trackURIBase } from "~/common/track.js";
 *
 * const r1 = trackURIBase("https://example.com/music/track.mp3");
 * if (r1 !== "https://example.com/") throw new Error(`expected "https://example.com/", got "${r1}"`);
 *
 * const r2 = trackURIBase("https://example.com/track.mp3?token=abc");
 * if (r2 !== "https://example.com/") throw new Error(`expected "https://example.com/", got "${r2}"`);
 *
 * const r3 = trackURIBase("https://example.com");
 * if (r3 !== "https://example.com/") throw new Error(`expected "https://example.com/", got "${r3}"`);
 *
 * const r4 = trackURIBase("s3://my-bucket/path/to/track.flac");
 * if (r4 !== "s3://my-bucket") throw new Error(`expected "s3://my-bucket", got "${r4}"`);
 * ```
 */
export function trackURIBase(uri) {
  const p = URI.parse(uri);
  p.path = undefined;
  p.query = undefined;
  return URI.serialize(p);
}

/**
 * @param {Track[]} tracks
 *
 * @example Returns a deduplicated set of base URIs
 * ```js
 * import { uniqueTrackURIs } from "~/common/track.js";
 *
 * const tracks = [
 *   { $type: "sh.diffuse.output.track", id: "1", uri: "https://example.com/a.mp3" },
 *   { $type: "sh.diffuse.output.track", id: "2", uri: "https://example.com/b.mp3" },
 * ];
 * const set = uniqueTrackURIs(tracks);
 * if (!set.has("https://example.com/")) throw new Error("expected base URI in set");
 * if (set.size !== 1) throw new Error("expected deduplication to one entry");
 *
 * const multi = uniqueTrackURIs([
 *   { $type: "sh.diffuse.output.track", id: "1", uri: "https://a.com/1.mp3" },
 *   { $type: "sh.diffuse.output.track", id: "2", uri: "https://b.com/2.mp3" },
 * ]);
 * if (multi.size !== 2) throw new Error("expected 2 distinct base URIs");
 *
 * if (uniqueTrackURIs([]).size !== 0) throw new Error("expected empty set for empty input");
 * ```
 */
export function uniqueTrackURIs(tracks) {
  const set = new Set();

  tracks.forEach((t) => {
    set.add(trackURIBase(t.uri));
  });

  return set;
}
