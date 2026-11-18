import { encode } from "@atcute/cbor";
import { xxh32r } from "xxh32/dist/raw.js";

/**
 * Small, pure helpers shared by the AT Protocol publish element.
 */

/**
 * Serialise a JSON value with object keys sorted, so equal records written in a
 * different order produce the same string (and therefore the same hash).
 *
 * `undefined` members are skipped like `JSON.stringify` would, and values JSON
 * cannot represent (functions, `NaN`, …) fall back to `null`.
 *
 * @param {unknown} value
 * @returns {string}
 *
 * @example Object key order does not matter
 * ```js
 * import { stableStringify } from "~/components/supplement/atproto-publish/common.js";
 *
 * const a = stableStringify({ b: 1, a: 2 });
 * const b = stableStringify({ a: 2, b: 1 });
 *
 * if (a !== b) throw new Error("expected the same string for reordered keys");
 * if (a !== '{"a":2,"b":1}') throw new Error(`unexpected output: ${a}`);
 * ```
 *
 * @example Nested values and arrays are handled
 * ```js
 * import { stableStringify } from "~/components/supplement/atproto-publish/common.js";
 *
 * const value = stableStringify({ z: [{ b: true, a: null }], y: "x" });
 *
 * if (value !== '{"y":"x","z":[{"a":null,"b":true}]}') {
 *   throw new Error(`unexpected output: ${value}`);
 * }
 * ```
 *
 * @example Undefined members are skipped
 * ```js
 * import { stableStringify } from "~/components/supplement/atproto-publish/common.js";
 *
 * const value = stableStringify({ a: 1, b: undefined });
 *
 * if (value !== '{"a":1}') throw new Error(`unexpected output: ${value}`);
 * ```
 */
export function stableStringify(value) {
  if (value === null || typeof value !== "object") {
    return JSON.stringify(value) ?? "null";
  }

  if (Array.isArray(value)) {
    return `[${value.map((item) => stableStringify(item)).join(",")}]`;
  }

  /** @type {string[]} */
  const members = [];

  for (const key of Object.keys(value).sort()) {
    const member = /** @type {Record<string, unknown>} */ (value)[key];
    if (member === undefined) continue;
    members.push(`${JSON.stringify(key)}:${stableStringify(member)}`);
  }

  return `{${members.join(",")}}`;
}

/**
 * Group playlist items by the playlist they belong to.
 *
 * Ephemeral items (temporary playlists such as queues) are skipped: they are
 * device-local and should never be published to an account.
 *
 * @template {{ playlist?: string; ephemeral?: boolean }} T
 * @param {T[]} items
 * @returns {Map<string, T[]>}
 *
 * @example Groups by playlist and skips ephemeral items
 * ```js
 * import { groupPlaylistItems } from "~/components/supplement/atproto-publish/common.js";
 *
 * const groups = groupPlaylistItems([
 *   { id: "a", playlist: "Mix" },
 *   { id: "b", playlist: "Mix" },
 *   { id: "c", playlist: "Other" },
 *   { id: "d", playlist: "Mix", ephemeral: true },
 *   { id: "e" },
 * ]);
 *
 * if (groups.get("Mix")?.length !== 2) throw new Error("expected two Mix items");
 * if (groups.get("Other")?.length !== 1) throw new Error("expected one Other item");
 * if (groups.size !== 2) throw new Error("expected exactly two playlists");
 * ```
 *
 * @example Returns an empty map for an empty list
 * ```js
 * import { groupPlaylistItems } from "~/components/supplement/atproto-publish/common.js";
 *
 * if (groupPlaylistItems([]).size !== 0) throw new Error("expected no groups");
 * ```
 */
export function groupPlaylistItems(items) {
  /** @type {Map<string, T[]>} */
  const groups = new Map();

  for (const item of items) {
    if (item.ephemeral) continue;

    const playlist = item.playlist;
    if (typeof playlist !== "string") continue;

    const group = groups.get(playlist);
    if (group) group.push(item);
    else groups.set(playlist, [item]);
  }

  return groups;
}

/**
 * The record key used for a playlist's bundle record.
 *
 * This mirrors the AT Protocol output so both elements address the same record
 * for the same playlist: the key is the hash of the playlist name encoded as
 * CBOR (a plain CBOR text string).
 *
 * @param {string} playlist
 * @returns {string}
 *
 * @example Is stable and distinguishes playlist names
 * ```js
 * import { playlistRkey } from "~/components/supplement/atproto-publish/common.js";
 *
 * if (playlistRkey("Mix") !== playlistRkey("Mix")) throw new Error("expected a stable key");
 * if (playlistRkey("Mix") === playlistRkey("Other")) throw new Error("expected distinct keys");
 * ```
 */
export function playlistRkey(playlist) {
  return xxh32r(encode(playlist)).toString(16);
}

/**
 * Hash arbitrary bytes as the lowercase hexadecimal string used by record keys
 * and change detection.
 *
 * @param {Uint8Array} bytes
 * @returns {string}
 *
 * @example Hashes equal bytes equally
 * ```js
 * import { bytesHash } from "~/components/supplement/atproto-publish/common.js";
 *
 * const bytes = new TextEncoder().encode("hello");
 * if (bytesHash(bytes) !== bytesHash(bytes)) throw new Error("expected equal hashes");
 * if (bytesHash(bytes) === bytesHash(new Uint8Array())) {
 *   throw new Error("expected different hashes");
 * }
 * ```
 */
export function bytesHash(bytes) {
  return xxh32r(bytes).toString(16);
}

/**
 * A short, order-independent hash of a JSON value.
 *
 * Used to remember which records were already published without keeping a copy
 * of their contents in `localStorage`.
 *
 * @param {unknown} value
 * @returns {string}
 *
 * @example Equal records with reordered keys hash equally
 * ```js
 * import { stableHash } from "~/components/supplement/atproto-publish/common.js";
 *
 * if (stableHash({ b: 1, a: 2 }) !== stableHash({ a: 2, b: 1 })) {
 *   throw new Error("expected equal hashes");
 * }
 * if (stableHash({ a: 1 }) === stableHash({ a: 2 })) {
 *   throw new Error("expected different hashes");
 * }
 * ```
 */
export function stableHash(value) {
  return bytesHash(new TextEncoder().encode(stableStringify(value)));
}
