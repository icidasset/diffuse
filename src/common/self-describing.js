/**
 * Self-describing envelopes for saved output data.
 *
 * Saved payloads carry their own schema so a reader can interpret and migrate
 * them without external knowledge. The envelope wraps a collection of records
 * (`Facet[]`, `Track[]`, …) with the atproto lexicon NSID that produced them plus
 * an ordered history of schema transitions (`$schemaHistory`). Each history entry
 * carries a portable lens document (authored, schema-independent `steps`) and,
 * when a write-back into a previous shape is needed, the complement produced by
 * the forward projection.
 *
 * In keeping with the atproto lexicon model (see "Lexicon Evolution" in the
 * atproto spec), the NSID (`$schema`) is the schema's identity. Compatible schema
 * evolution keeps the same NSID — new optional fields simply appear, and old data
 * remains valid. A breaking change (renaming or removing a field, changing a type)
 * is expressed as a NEW NSID, with an authored lens from the old NSID to the new
 * one. There is no integer "schema version"; the migration chain is the history.
 *
 * The lens documents embedded in `$schemaHistory` are the correctness guarantee
 * that older apps can read newer data. A given app build may also resolve a
 * segment's lens from its own bundled registry (cheaper, for well-known
 * transitions); resolution order is: bundled registry first, then the embedded
 * document.
 *
 * @import {Facet, PlaylistItem, Setting, Track} from "~/definitions/types.d.ts"
 */

/**
 * A schema transition recorded in `$schemaHistory`, from one lexicon NSID to
 * another.
 *
 * The `lens` is an authored, schema-independent document (the same shape
 * `@panproto/core`'s `compileLensDocument` accepts: an object with `steps`). It is
 * stored here so any reader — including one older than the lens' authoring build —
 * can recompile it and traverse the transition.
 *
 * @template {unknown} L
 * @typedef {{
 *   from: string;
 *   to: string;
 *   lens: L | null;
 *   complement?: Uint8Array | string | null;
 * }} HistoryEntry
 */

/**
 * A self-describing envelope around a collection of records.
 *
 * @template {unknown[]} T
 * @template {unknown} L
 * @typedef {{
 *   $schema: string;
 *   $schemaHistory: HistoryEntry<L>[];
 *   data: T;
 * }} SelfDescribing<T, L>
 */

/**
 * Wrap a collection into a self-describing envelope.
 *
 * @template {unknown[]} T
 * @template {unknown} L
 * @param {T} data
 * @param {{ schema: string; history?: HistoryEntry<L>[] }} opts
 * @returns {SelfDescribing<T, L>}
 *
 * @example Wraps records with the lexicon that produced them
 * ```js
 * import { wrap } from "~/common/self-describing.js";
 *
 * const envelope = wrap([{ id: "a" }], { schema: "sh.diffuse.output.facet" });
 * if (envelope.$schema !== "sh.diffuse.output.facet") throw new Error("expected schema");
 * if (envelope.data[0].id !== "a") throw new Error("expected data");
 * ```
 *
 * @example Stores the supplied history entries
 * ```js
 * import { wrap } from "~/common/self-describing.js";
 *
 * const envelope = wrap([], {
 *   schema: "sh.diffuse.output.track2",
 *   history: [{
 *     from: "sh.diffuse.output.track",
 *     to: "sh.diffuse.output.track2",
 *     lens: { steps: [] },
 *     complement: null,
 *   }],
 * });
 * if (envelope.$schemaHistory.length !== 1) throw new Error("expected 1 history entry");
 * ```
 */
export function wrap(data, { schema, history = [] }) {
  return {
    $schema: schema,
    $schemaHistory: history,
    data,
  };
}

/**
 * Read the collection out of a stored value, tolerating legacy payloads.
 *
 * Legacy payloads — a bare array, or an object that is not an envelope — are
 * treated as `data` written against the supplied schema (a `v0` with no recorded
 * migration). This keeps existing users unbroken when the envelope is introduced.
 *
 * @template {unknown[]} T
 * @template {unknown} L
 * @param {unknown} value
 * @param {{ $schema: string }} opts
 * @returns {{ data: T; envelope: SelfDescribing<T, L> | null }}
 *
 * @example Reads a self-describing envelope
 * ```ts
 * import { wrap, unwrap } from "~/common/self-describing.js";
 *
 * const wrapped = wrap([{ id: "a" }], { schema: "sh.diffuse.output.facet" });
 * const out = unwrap(wrapped, {
 *   $schema: "sh.diffuse.output.facet",
 * }) as { data: Array<{ id: string }>; envelope: { $schema: string } | null };
 * if (out.data[0].id !== "a") throw new Error("expected data");
 * if (out.envelope === null) throw new Error("expected envelope");
 * ```
 *
 * @example Tolerates a legacy bare array
 * ```ts
 * import { unwrap } from "~/common/self-describing.js";
 *
 * const out = unwrap([{ id: "a" }], {
 *   $schema: "sh.diffuse.output.facet",
 * }) as { data: Array<{ id: string }>; envelope: unknown };
 * if (out.data[0].id !== "a") throw new Error("expected data");
 * if (out.envelope !== null) throw new Error("expected no envelope for legacy data");
 * ```
 */
export function unwrap(value, { $schema }) {
  void $schema;
  if (isSelfDescribing(value)) {
    const envelope = /** @type {SelfDescribing<T, L>} */ (value);
    return { data: /** @type {T} */ (envelope.data), envelope };
  }
  return { data: /** @type {T} */ (value), envelope: null };
}

/**
 * Whether a stored value is a self-describing envelope.
 *
 * @template {unknown} T
 * @param {unknown} value
 * @returns {value is SelfDescribing<T, any>}
 *
 * @example Identifies enveloped data
 * ```js
 * import { isSelfDescribing } from "~/common/self-describing.js";
 *
 * if (isSelfDescribing({ $schema: "s", $schemaHistory: [], data: [] })) {
 *   // is a self-describing envelope
 * } else {
 *   throw new Error("expected to be self-describing");
 * }
 * ```
 *
 * @example Rejects a bare array
 * ```js
 * import { isSelfDescribing } from "~/common/self-describing.js";
 *
 * if (isSelfDescribing([1, 2, 3])) throw new Error("bare array is not an envelope");
 * ```
 */
export function isSelfDescribing(value) {
  return Boolean(
    value &&
      typeof value === "object" &&
      !Array.isArray(value) &&
      typeof /** @type {any} */ (value).$schema === "string" &&
      Array.isArray(/** @type {any} */ (value).$schemaHistory) &&
      Array.isArray(/** @type {any} */ (value).data),
  );
}

/**
 * The name of one of the collections an output persists.
 *
 * @typedef {"facets" | "playlistItems" | "settings" | "tracks"} CollectionName
 */

/** @type {Record<CollectionName, string>} */
const COLLECTION_SCHEMAS = {
  facets: "sh.diffuse.output.facet",
  playlistItems: "sh.diffuse.output.playlistItem",
  settings: "sh.diffuse.output.setting",
  tracks: "sh.diffuse.output.track",
};

/**
 * The current lexicon NSID for a collection.
 *
 * @param {CollectionName} name
 * @returns {string}
 *
 * @example Returns the schema for tracks
 * ```js
 * import { collectionSchema } from "~/common/self-describing.js";
 *
 * const s = collectionSchema("tracks");
 * if (s !== "sh.diffuse.output.track") throw new Error("expected track schema");
 * ```
 */
export function collectionSchema(name) {
  return COLLECTION_SCHEMAS[name];
}