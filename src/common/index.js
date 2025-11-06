// import * as Uint8 from "uint8arrays";

/**
 * @import {Track} from "@common/types.d.ts"
 */

/**
 * @template T
 * @param {Array<T>} array
 * @returns Array<T>
 */
export function arrayShuffle(array) {
  if (array.length === 0) {
    return [];
  }

  array = [...array];

  for (let index = array.length - 1; index > 0; index--) {
    const randArr = crypto.getRandomValues(new Uint32Array(1));
    const randVal = randArr[0] / 2 ** 32;
    const newIndex = Math.floor(randVal * (index + 1));
    [array[index], array[newIndex]] = [array[newIndex], array[index]];
  }

  return array;
}

/**
 * @param {Track[]} tracks
 * @returns {Track[]}
 */
export function cleanUndefinedValuesForTracks(tracks) {
  return tracks.map((track) => {
    const t = { ...track };

    if (t.tags) {
      if ("album" in t.tags && t.tags.album === undefined) delete t.tags.album;
      if ("artist" in t.tags && t.tags.artist === undefined) {
        delete t.tags.artist;
      }
      if ("genre" in t.tags && t.tags.genre === undefined) delete t.tags.genre;
      if ("year" in t.tags && t.tags.year === undefined) delete t.tags.year;

      if ("of" in t.tags.disc && t.tags.disc.of === undefined) {
        delete t.tags.disc.of;
      }
      if ("of" in t.tags.track && t.tags.track.of === undefined) {
        delete t.tags.track.of;
      }
    }

    return t;
  });
}

/**
 * @param {Track[]} tracks
 * @param {Record<string, Track[]>} initial
 * @returns {Record<string, Track[]>}
 */
export function groupTracksPerScheme(
  tracks,
  initial = {},
) {
  /** @type {Record<string, Track[]>} */
  const acc = initial;

  tracks.forEach((track) => {
    const scheme = track.uri.substring(0, track.uri.indexOf(":"));
    acc[scheme] ??= [];
    acc[scheme].push(track);
  });

  return acc;
}

/**
 * @param {unknown} test
 */
export function isPrimitive(test) {
  return test !== Object(test);
}

/**
 * @template T
 * @param {any} a
 * @returns {T}
 */
export function jsonDecode(a) {
  return JSON.parse(new TextDecoder().decode(a));
}

/**
 * @template T
 * @param {T} a
 * @returns Uint8Array
 */
export function jsonEncode(a) {
  return new TextEncoder().encode(JSON.stringify(a));
}

/**
 * @param {Track} track
 * @returns {Promise<string>}
 */
export async function trackArtworkCacheId(track) {
  return "";
  // return await crypto.subtle
  //   .digest("SHA-256", new TextEncoder().encode(track.uri))
  //   .then((a) => Uint8.toString(new Uint8Array(a), "base64url"));
}
