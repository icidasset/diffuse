import { base64url } from "iso-base/rfc4648";
import { xxh32r } from "xxh32/dist/raw.js";

/**
 * @import {Track} from "@definitions/types.d.ts"
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
 * @param {string | undefined | null} value
 */
export function boolAttr(value) {
  return value === "";
}

/**
 * @param {any} object
 */
export function hash(object) {
  return xxh32r(jsonEncode(object)).toString();
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
 * @template {Record<string, any>} T
 * @param {T} rec
 */
export function removeUndefinedValuesFromRecord(rec) {
  const recClone = { ...rec };

  Object.entries(recClone).forEach(([key, value]) => {
    if (value === undefined) {
      delete recClone[key];
    }
  });

  return recClone;
}

/**
 * @template {Record<string, any>} T
 * @param {T} rec
 */
export function recursivelyCloneRecords(rec) {
  const recClone = { ...rec };

  Object.entries(recClone).forEach(([key, value]) => {
    if (typeof value === "object") {
      /** @ts-ignore */
      recClone[key] = recursivelyCloneRecords(value);
    }
  });

  return recClone;
}

/**
 * @param {Track} track
 * @returns {Promise<string>}
 */
export async function trackArtworkCacheId(track) {
  return await crypto.subtle
    .digest("SHA-256", new TextEncoder().encode(track.uri))
    .then((a) => base64url.encode(new Uint8Array(a)));
}
