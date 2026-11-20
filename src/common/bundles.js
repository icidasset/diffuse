import { encode } from "@atcute/cbor";
import { xxh32r } from "xxh32/dist/raw.js";

/**
 * @import {Bundle, BundleOptions} from "./bundles.d.ts"
 */

/**
 * Partition a collection into smaller, stable bundles.
 *
 * Bundling turns one flat list into groups keyed by some property of each item:
 * a playlist name, a URI scheme, a field value, and so on. The bundle's `id` is
 * derived from that key the same way Diffuse derives AT Protocol record keys, so
 * each bundle maps naturally onto one record (or one blob) on an account.
 *
 * Items keep their relative order inside a bundle, and bundles are ordered by
 * the first time their key appears. An item may be placed in more than one
 * bundle by returning several keys. Items without a key are skipped.
 *
 * This function is pure: it does not touch the DOM, signals or storage, which
 * makes it usable by both output transformers and supplements such as the AT
 * Protocol publisher.
 *
 * @template T
 * @param {T[]} items
 * @param {BundleOptions<T>} options
 * @returns {Bundle<T>[]}
 *
 * @example Groups items by a field
 * ```js
 * import { bundle, byField } from "~/common/bundles.js";
 *
 * const bundles = bundle([
 *   { id: "a", playlist: "Mix" },
 *   { id: "b", playlist: "Other" },
 *   { id: "c", playlist: "Mix" },
 * ], { by: byField("playlist") });
 *
 * if (bundles.length !== 2) throw new Error("expected two bundles");
 * if (bundles[0].key !== "Mix") throw new Error("expected Mix first");
 * if (bundles[0].items.length !== 2) throw new Error("expected two Mix items");
 * if (bundles[1].items[0].id !== "b") throw new Error("expected b in Other");
 * ```
 *
 * @example Skips items without a key and items rejected by `skip`
 * ```js
 * import { bundle } from "~/common/bundles.js";
 *
 * const bundles = bundle([
 *   { id: "a", playlist: "Mix" },
 *   { id: "b", playlist: "Mix", ephemeral: true },
 *   { id: "c" },
 * ], {
 *   by: (item) => item.playlist,
 *   skip: (item) => Boolean(item.ephemeral),
 * });
 *
 * if (bundles.length !== 1) throw new Error("expected one bundle");
 * if (bundles[0].items.length !== 1) throw new Error("expected the ephemeral item to be skipped");
 * if (bundles[0].items[0].id !== "a") throw new Error("expected a to remain");
 * ```
 *
 * @example An item can belong to several bundles
 * ```js
 * import { bundle } from "~/common/bundles.js";
 *
 * const bundles = bundle(
 *   [{ id: "a", genres: ["rock", "pop"] }],
 *   { by: (item) => item.genres },
 * );
 *
 * if (bundles.map((b) => b.key).join(",") !== "rock,pop") {
 *   throw new Error("expected one bundle per genre");
 * }
 * ```
 */
export function bundle(items, { by, id = bundleId, skip }) {
  /** @type {Map<string, Bundle<T>>} */
  const bundles = new Map();

  for (const item of items) {
    if (skip?.(item)) continue;

    const keys = by(item);
    if (!keys) continue;

    const seen = new Set();

    for (const key of typeof keys === "string" ? [keys] : keys) {
      if (typeof key !== "string" || seen.has(key)) continue;
      seen.add(key);

      let bucket = bundles.get(key);
      if (!bucket) {
        bucket = { id: id(key), key, items: [] };
        bundles.set(key, bucket);
      }

      bucket.items.push(item);
    }
  }

  return [...bundles.values()];
}

/**
 * A stable bundle id derived from its key.
 *
 * The key is encoded as CBOR and hashed with xxhash32, which is the same
 * derivation Diffuse uses for its AT Protocol record keys. Equal keys therefore
 * always address the same bundle, and therefore the same record.
 *
 * @param {string} key
 * @returns {string}
 *
 * @example Is stable and distinguishes keys
 * ```js
 * import { bundleId } from "~/common/bundles.js";
 *
 * if (bundleId("Mix") !== bundleId("Mix")) throw new Error("expected a stable id");
 * if (bundleId("Mix") === bundleId("Other")) throw new Error("expected distinct ids");
 * if (bundleId("Mix") !== bundleId("Mix").toLowerCase()) {
 *   throw new Error("expected a lowercase id");
 * }
 * ```
 */
export function bundleId(key) {
  return xxh32r(encode(key)).toString(16);
}

/**
 * Flatten bundles back into a single list of their items.
 *
 * The result follows the bundle order (and each bundle's item order), not the
 * original order of the collection. That is intentional: a bundle only records
 * which items belong together, and different consumers may order the bundles
 * differently.
 *
 * @template T
 * @param {Bundle<T>[]} bundles
 * @returns {T[]}
 *
 * @example Flattens bundles back into their items
 * ```js
 * import { bundle, flattenBundles, byField } from "~/common/bundles.js";
 *
 * const flat = flattenBundles(bundle([
 *   { id: "a", playlist: "Mix" },
 *   { id: "b", playlist: "Other" },
 * ], { by: byField("playlist") }));
 *
 * if (flat.map((item) => item.id).join(",") !== "a,b") {
 *   throw new Error("expected the original items");
 * }
 * ```
 */
export function flattenBundles(bundles) {
  /** @type {T[]} */
  const items = [];

  for (const bucket of bundles) {
    items.push(...bucket.items);
  }

  return items;
}

/**
 * Whether a value looks like a bundle produced by {@link bundle}.
 *
 * @param {unknown} value
 * @returns {value is Bundle<any>}
 *
 * @example Recognises bundles but not plain items
 * ```js
 * import { isBundle } from "~/common/bundles.js";
 *
 * if (!isBundle({ id: "1", key: "Mix", items: [] })) {
 *   throw new Error("expected a bundle");
 * }
 * if (isBundle({ id: "1", playlist: "Mix" })) {
 *   throw new Error("expected a plain item not to be a bundle");
 * }
 * ```
 */
export function isBundle(value) {
  if (!value || typeof value !== "object") return false;

  const bundle = /** @type {Record<string, unknown>} */ (value);

  return typeof bundle.id === "string" &&
    typeof bundle.key === "string" &&
    Array.isArray(bundle.items);
}

/**
 * Whether a value is a list of bundles rather than a list of items.
 *
 * An empty list counts as bundles: it flattens to an empty item list either
 * way, and lets callers treat "no data" uniformly.
 *
 * @param {unknown} value
 * @returns {value is Bundle<any>[]}
 *
 * @example Distinguishes bundle lists from item lists
 * ```js
 * import { isBundleArray } from "~/common/bundles.js";
 *
 * if (!isBundleArray([])) throw new Error("expected an empty list to be a bundle list");
 * if (!isBundleArray([{ id: "1", key: "Mix", items: [] }])) {
 *   throw new Error("expected a bundle list");
 * }
 * if (isBundleArray([{ id: "1", playlist: "Mix" }])) {
 *   throw new Error("expected an item list not to be a bundle list");
 * }
 * ```
 */
export function isBundleArray(value) {
  return Array.isArray(value) && value.every(isBundle);
}

/**
 * Group items by the value of one of their string fields.
 *
 * Items where the field is missing or not a string are skipped.
 *
 * @template T
 * @param {string} field
 * @returns {(item: T) => string | undefined}
 *
 * @example Groups by the `playlist` field
 * ```js
 * import { byField } from "~/common/bundles.js";
 *
 * const select = byField("playlist");
 *
 * if (select({ playlist: "Mix" }) !== "Mix") throw new Error("expected the field value");
 * if (select({ playlist: 42 }) !== undefined) throw new Error("expected non-strings to be skipped");
 * if (select({}) !== undefined) throw new Error("expected missing fields to be skipped");
 * ```
 */
export function byField(field) {
  return (item) => {
    if (!item || typeof item !== "object") return undefined;

    const value = /** @type {Record<string, unknown>} */ (item)[field];
    return typeof value === "string" ? value : undefined;
  };
}

/**
 * Group items by the scheme of their `uri` (for example `https`, `s3`,
 * `opensubsonic` or `local`).
 *
 * This is a coarse, portable fallback. Callers that can ask an input for its
 * richer `sources(tracks)` should prefer those, which distinguish individual
 * buckets, servers and directories that share a scheme.
 *
 * @template T
 * @returns {(item: T) => string | undefined}
 *
 * @example Groups URLs by scheme
 * ```js
 * import { byUriScheme } from "~/common/bundles.js";
 *
 * const select = byUriScheme();
 *
 * if (select({ uri: "https://example.com/a.mp3" }) !== "https") {
 *   throw new Error("expected https");
 * }
 * if (select({ uri: "s3://bucket/b.flac" }) !== "s3") throw new Error("expected s3");
 * if (select({ uri: "nonsense" }) !== undefined) throw new Error("expected scheme-less URIs to be skipped");
 * ```
 */
export function byUriScheme() {
  return (item) => {
    if (!item || typeof item !== "object") return undefined;

    const uri = /** @type {Record<string, unknown>} */ (item).uri;
    if (typeof uri !== "string") return undefined;

    const colon = uri.indexOf(":");
    return colon > 0 ? uri.slice(0, colon) : undefined;
  };
}
