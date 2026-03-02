import * as URI from "fast-uri";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 */

/**
 * @param {string} uri
 */
export function trackURIBase(uri) {
  const p = URI.parse(uri);
  p.path = undefined;
  p.query = undefined;
  return URI.serialize(p);
}

/**
 * @param {Track[]} tracks
 */
export function uniqueTrackURIs(tracks) {
  const set = new Set();

  tracks.forEach((t) => {
    set.add(trackURIBase(t.uri));
  });

  return set;
}
