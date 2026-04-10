/**
 * @import {Track} from "~/definitions/types.d.ts"
 */

/**
 * Creates a time-cached version of an async consult function.
 * Results are cached per key for the given TTL.
 *
 * @template T
 * @param {(arg: T) => Promise<boolean>} fn
 * @param {(arg: T) => string} keyFn
 * @param {number} ttl - Cache TTL in milliseconds
 * @returns {(arg: T) => Promise<boolean>}
 *
 * @example Caches results and avoids calling fn more than once per key
 * ```js
 * import { cachedConsult } from "~/components/input/common.js";
 *
 * let callCount = 0;
 * const cached = cachedConsult(async (uri) => { callCount++; return true; }, (uri) => uri);
 *
 * const r1 = await cached("https://example.com/stream");
 * const r2 = await cached("https://example.com/stream");
 *
 * if (r1 !== true || r2 !== true) throw new Error("should return cached value");
 * if (callCount !== 1) throw new Error("fn should only be called once per key");
 * ```
 */
export function cachedConsult(fn, keyFn, ttl = 60_000 * 5) {
  /** @type {Map<string, { value: boolean; expiry: number }>} */
  const cache = new Map();

  return async (arg) => {
    const key = keyFn(arg);
    const now = Date.now();
    const cached = cache.get(key);

    if (cached && cached.expiry > now) {
      return cached.value;
    }

    const value = await fn(arg);
    cache.set(key, { value, expiry: now + ttl });
    return value;
  };
}

/**
 * @param {{ fileUriOrScheme: string; handleFileUri: (args: { fileURI: string; tracks: Track[] }) => Track[]; inputScheme: string; tracks: Track[] }} _
 *
 * @example Removes all tracks when given a matching scheme, returns all when scheme doesn't match
 * ```js
 * import { detach } from "~/components/input/common.js";
 *
 * const tracks = JSON.parse('[{"$type":"sh.diffuse.output.track","id":"1","uri":"https://a.com/1.mp3"},{"$type":"sh.diffuse.output.track","id":"2","uri":"https://b.com/2.mp3"}]');
 *
 * // @ts-ignore
 * const removed = detach({ fileUriOrScheme: "https", inputScheme: "https", handleFileUri: () => [], tracks });
 * if (removed.length !== 0) throw new Error("matching scheme should remove all tracks");
 *
 * // @ts-ignore
 * const kept = detach({ fileUriOrScheme: "ftp", inputScheme: "https", handleFileUri: () => [], tracks });
 * if (kept.length !== 2) throw new Error("non-matching scheme should keep all tracks");
 * ```
 *
 * @example Delegates to handleFileUri when a full URI is given
 * ```js
 * import { detach } from "~/components/input/common.js";
 *
 * const tracks = JSON.parse('[{"$type":"sh.diffuse.output.track","id":"1","uri":"https://a.com/1.mp3"},{"$type":"sh.diffuse.output.track","id":"2","uri":"https://b.com/2.mp3"}]');
 *
 * // @ts-ignore
 * const result = detach({ fileUriOrScheme: "https://a.com/1.mp3", inputScheme: "https", handleFileUri: ({ tracks }) => tracks.filter((t) => t.id !== "1"), tracks });
 * if (result.length !== 1 || result[0].id !== "2") throw new Error("handleFileUri should filter by URI");
 * ```
 */
export function detach(
  { fileUriOrScheme, handleFileUri, inputScheme, tracks },
) {
  if (!fileUriOrScheme.includes("://")) {
    // Delete everything if scheme matches
    if (fileUriOrScheme === inputScheme) return [];
    return tracks;
  }

  return handleFileUri({ fileURI: fileUriOrScheme, tracks });
}

/**
 * @param {string} scheme
 * @param {string} groupId
 *
 * @example Returns scheme://groupId
 * ```js
 * import { groupKey } from "~/components/input/common.js";
 *
 * if (groupKey("https", "example.com") !== "https://example.com") throw new Error(`expected "https://example.com"`);
 * ```
 */
export function groupKey(scheme, groupId) {
  return `${scheme}://${groupId}`;
}

/**
 * @param {string} filename
 *
 * @example Returns truthy for audio extensions and falsy for non-audio ones
 * ```js
 * import { isAudioFile } from "~/components/input/common.js";
 *
 * const audioExts = ["track.mp3", "track.flac", "track.ogg", "track.opus", "track.wav", "track.m4a", "track.webm"];
 * for (const f of audioExts) {
 *   if (!isAudioFile(f)) throw new Error(`${f} should be recognised as audio`);
 * }
 *
 * const nonAudio = ["track.txt", "track.jpg", "track.pdf", "track"];
 * for (const f of nonAudio) {
 *   if (isAudioFile(f)) throw new Error(`${f} should not be recognised as audio`);
 * }
 * ```
 */
export function isAudioFile(filename) {
  return filename.match(/\.(flac|m4a|mp3|mp4|ogg|opus|wav|webm)$/);
}
