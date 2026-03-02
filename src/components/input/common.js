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
 */
export function groupKey(scheme, groupId) {
  return `${scheme}://${groupId}`;
}

/**
 * @param {string} filename
 */
export function isAudioFile(filename) {
  return filename.match(/\.(flac|m4a|mp3|mp4|ogg|opus|wav|webm)$/);
}
