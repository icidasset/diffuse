import { xxh32r } from "xxh32/dist/raw.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 */

/**
 * @template T
 * @param {Array<T>} array
 * @returns Array<T>
 *
 * @example Returns same elements in (possibly different) order without mutating the original
 * ```js
 * import { arrayShuffle } from "~/common/utils.js";
 *
 * if (arrayShuffle([]).length !== 0) throw new Error("empty array should return empty");
 *
 * const sorted = arrayShuffle([1, 2, 3, 4, 5]).sort((a, b) => a - b);
 * if (sorted.join(",") !== "1,2,3,4,5") throw new Error("shuffled array should contain same elements");
 *
 * const original = [1, 2, 3];
 * arrayShuffle(original);
 * if (original.join(",") !== "1,2,3") throw new Error("original array should not be mutated");
 *
 * if (arrayShuffle([42]).join(",") !== "42") throw new Error("single-element array should be unchanged");
 * ```
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
 *
 * @example Returns true only for empty string (present attribute)
 * ```js
 * import { boolAttr } from "~/common/utils.js";
 *
 * if (!boolAttr("")) throw new Error("empty string should return true");
 * if (boolAttr(null)) throw new Error("null should return false");
 * if (boolAttr(undefined)) throw new Error("undefined should return false");
 * if (boolAttr("true")) throw new Error("non-empty string should return false");
 * ```
 */
export function boolAttr(value) {
  return value === "";
}


/**
 * @param {any} object
 *
 * @example Returns a consistent string hash for the same object
 * ```js
 * import { hash } from "~/common/utils.js";
 *
 * if (typeof hash({ a: 1 }) !== "string") throw new Error("hash should return a string");
 * if (hash({ a: 1, b: 2 }) !== hash({ a: 1, b: 2 })) throw new Error("same objects should produce same hash");
 * if (hash({ a: 1 }) === hash({ a: 2 })) throw new Error("different objects should produce different hashes");
 * ```
 */
export function hash(object) {
  return xxh32r(jsonEncode(object)).toString();
}

/**
 * @param {Track[]} tracks
 * @param {Record<string, Track[]>} initial
 * @returns {Record<string, Track[]>}
 *
 * @example Groups tracks by URI scheme
 * ```js
 * import { groupTracksPerScheme } from "~/common/utils.js";
 *
 * const tracks = [
 *   { $type: "sh.diffuse.output.track", id: "1", uri: "http://example.com/a.mp3" },
 *   { $type: "sh.diffuse.output.track", id: "2", uri: "s3://bucket/b.mp3" },
 *   { $type: "sh.diffuse.output.track", id: "3", uri: "http://example.com/c.mp3" },
 * ];
 * // @ts-ignore
 * const groups = groupTracksPerScheme(tracks);
 * if (groups["http"].length !== 2) throw new Error("expected 2 http tracks");
 * if (groups["s3"].length !== 1) throw new Error("expected 1 s3 track");
 * ```
 *
 * @example Merges into a provided initial object
 * ```js
 * import { groupTracksPerScheme } from "~/common/utils.js";
 *
 * const initial = { http: [{ $type: "sh.diffuse.output.track", id: "0", uri: "http://existing.com/x.mp3" }] };
 * const tracks = [{ $type: "sh.diffuse.output.track", id: "1", uri: "http://example.com/a.mp3" }];
 * // @ts-ignore
 * const merged = groupTracksPerScheme(tracks, initial);
 * if (merged["http"].length !== 2) throw new Error("expected 2 http tracks after merge");
 * ```
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
 * @param {string[]} uris
 * @returns {Record<string, string[]>}
 *
 * @example Groups URIs by scheme
 * ```js
 * import { groupUrisPerScheme } from "~/common/utils.js";
 *
 * const groups = groupUrisPerScheme(["http://a.com/t.mp3", "s3://b/t.flac", "http://c.com/t.mp3"]);
 * if (groups["http"].length !== 2) throw new Error("expected 2 http URIs");
 * if (groups["s3"].length !== 1) throw new Error("expected 1 s3 URI");
 *
 * if (Object.keys(groupUrisPerScheme([])).length !== 0) throw new Error("expected empty object for empty input");
 * ```
 */
export function groupUrisPerScheme(uris) {
  /** @type {Record<string, string[]>} */
  const acc = {};

  uris.forEach((uri) => {
    const scheme = uri.substring(0, uri.indexOf(":"));
    acc[scheme] ??= [];
    acc[scheme].push(uri);
  });

  return acc;
}

/**
 * @param {unknown} test
 *
 * @example Returns true for primitives and false for objects/arrays
 * ```js
 * import { isPrimitive } from "~/common/utils.js";
 *
 * if (!isPrimitive(42)) throw new Error("number should be primitive");
 * if (!isPrimitive("hello")) throw new Error("string should be primitive");
 * if (!isPrimitive(true)) throw new Error("boolean should be primitive");
 * if (!isPrimitive(null)) throw new Error("null should be primitive");
 * if (isPrimitive({ a: 1 })) throw new Error("object should not be primitive");
 * if (isPrimitive([1, 2])) throw new Error("array should not be primitive");
 * ```
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
 *
 * @example jsonEncode returns a Uint8Array that round-trips through jsonDecode
 * ```js
 * import { jsonEncode, jsonDecode } from "~/common/utils.js";
 *
 * const original = { a: 1, b: "hello", c: [1, 2, 3] };
 * if (!(jsonEncode(original) instanceof Uint8Array)) throw new Error("jsonEncode should return a Uint8Array");
 *
 * const roundTripped = jsonDecode(jsonEncode(original));
 * if (JSON.stringify(roundTripped) !== JSON.stringify(original)) throw new Error("round-trip should preserve values");
 * ```
 */
export function jsonEncode(a) {
  return new TextEncoder().encode(JSON.stringify(a));
}

/**
 * @template {Record<string, any>} T
 * @param {T} rec
 *
 * @example Removes keys with undefined values without mutating the original
 * ```js
 * import { removeUndefinedValuesFromRecord } from "~/common/utils.js";
 *
 * const result = removeUndefinedValuesFromRecord({ a: 1, b: undefined, c: "x" });
 * if ("b" in result) throw new Error("undefined key should be removed");
 * if (result["a"] !== 1 || result["c"] !== "x") throw new Error("defined keys should be preserved");
 *
 * const original = { a: 1, b: undefined };
 * removeUndefinedValuesFromRecord(original);
 * if (!("b" in original)) throw new Error("original should not be mutated");
 *
 * const noUndef = removeUndefinedValuesFromRecord({ a: 1, b: 2 });
 * if (noUndef["a"] !== 1 || noUndef["b"] !== 2) throw new Error("record without undefined values should be unchanged");
 * ```
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
 *
 * @example Deep-clones nested records without sharing references
 * ```js
 * import { recursivelyCloneRecords } from "~/common/utils.js";
 *
 * const original = { a: 1, b: "x" };
 * const clone = recursivelyCloneRecords(original);
 * if (clone === original) throw new Error("clone should be a different object");
 * if (clone.a !== 1 || clone.b !== "x") throw new Error("values should be preserved");
 *
 * const nested = { outer: { inner: 42 } };
 * const nestedClone = recursivelyCloneRecords(nested);
 * if (nestedClone.outer === nested.outer) throw new Error("nested objects should not share references");
 * if (nestedClone.outer.inner !== 42) throw new Error("nested values should be preserved");
 * ```
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
 * @param {string} str
 * @returns {string}
 *
 * @example Decodes percent-encoded and %u unicode escapes, leaves plain strings unchanged
 * ```js
 * import { safeDecodeURIComponent } from "~/common/utils.js";
 *
 * if (safeDecodeURIComponent("hello%20world") !== "hello world") throw new Error("should decode %20 as space");
 * if (safeDecodeURIComponent("%u0041") !== "A") throw new Error("should decode %u unicode escape");
 * if (safeDecodeURIComponent("plain-string") !== "plain-string") throw new Error("plain string should be unchanged");
 * if (safeDecodeURIComponent("hello%2Fworld") !== "hello/world") throw new Error("should decode %2F as slash");
 * ```
 */
export function safeDecodeURIComponent(str) {
  return str.replace(
    /%u([0-9A-Fa-f]{4})|%([0-9A-Fa-f]{2})/g,
    (_, unicode, byte) =>
      unicode
        ? String.fromCharCode(parseInt(unicode, 16))
        : String.fromCharCode(parseInt(byte, 16)),
  );
}
