/**
 * Migration projection engine: applying authored lens documents to records.
 *
 * Lens documents (see `~/common/lens-registry.js`) describe transitions between
 * lexicon NSIDs as schema-independent steps. In keeping with atproto's lexicon
 * model, the NSID is the schema identity: compatible evolution keeps the same
 * NSID, while a breaking change is a NEW NSID with an authored lens from the old
 * NSID to the new one. This engine projects records from one NSID's shape to
 * another without the panproto WASM engine for the common, reversible cases
 * (renames, additive fields). Use `migrate()` for that projection.
 *
 * Wild/disruptive changes (e.g. dropping a field where the discarded value must
 * be written back) are handled losslessly via the panproto complement machinery
 * (see the design doc, open item #1); the pure-step projection here covers the
 * additive/rename migrations diffuse actually ships.
 *
 * @import {LensDocument} from "~/common/lens-registry.js"
 */

import { collectionSchema, unwrap, wrap } from "./self-describing.js";
import { resolve } from "./lens-registry.js";
import { put as panprotoPut, parseLexicon } from "./panproto.js";

/**
 * Project a collection of records from one NSID's shape to another using a lens
 * document's steps.
 *
 * Supports the DSL document steps diffuse authors: `rename_field`, `add_field`,
 * `remove_field`. Records are plain objects; unknown steps are left as-is.
 *
 * @param {unknown[]} records
 * @param {LensDocument} lens
 * @returns {unknown[]}
 *
 * @example Renames a field and adds a defaulted field
 * ```ts
 * import { project } from "~/common/lens.js";
 *
 * const out = project(
 *   [{ $type: "sh.diffuse.output.facet", id: "a", name: "x", favourite: true }],
 *   {
 *     id: "f", source: "sh.diffuse.output.facet", target: "sh.diffuse.output.facet2",
 *     steps: [
 *       { rename_field: { old: "favourite", new: "starred" } },
 *       { add_field: { parent: "sh.diffuse.output.facet2:body", name: "description", kind: "string" } },
 *     ],
 *   },
 * );
 * const rec = out[0] as Record<string, unknown>;
 * if (!("starred" in rec)) throw new Error("expected renamed field");
 * if ("favourite" in rec) throw new Error("old field should be gone");
 * if (!("description" in rec)) throw new Error("expected added field");
 * ```
 *
 * @example Remove a field
 * ```ts
 * import { project } from "~/common/lens.js";
 *
 * const out = project([{ id: "a", dropped: true }], {
 *   id: "f", source: "s", target: "t",
 *   steps: [{ remove_field: { name: "dropped" } }],
 * });
 * if ("dropped" in (out[0] as Record<string, unknown>)) throw new Error("expected removed");
 * ```
 */
export function project(records, lens) {
  return records.map((record) => {
    if (typeof record !== "object" || record === null || Array.isArray(record)) {
      return record;
    }
    /** @type {Record<string, unknown>} */
    const rec = { .../** @type {Record<string, unknown>} */ (record) };

    for (const step of lens.steps) {
      const s = /** @type {any} */ (step);
      if (s?.rename_field) {
        const oldName = /** @type {string} */ (s.rename_field.old);
        const newName = /** @type {string} */ (s.rename_field.new);
        if (oldName in rec) {
          rec[newName] = rec[oldName];
          delete rec[oldName];
        }
      } else if (s?.remove_field) {
        const name = /** @type {string} */ (s.remove_field.name);
        delete rec[name];
      } else if (s?.add_field) {
        const name = /** @type {string} */ (s.add_field.name);
        if (!(name in rec)) rec[name] = defaultValue(/** @type {string} */(s.add_field.kind));
      }
    }

    return rec;
  });
}

/**
 * Migrate a stored envelope to the current lexicon NSID if it is stale, returning
 * the migrated data and the updated `$schemaHistory`. When the stored schema
 * already matches, it is returned unchanged.
 *
 * @template {unknown[]} T
 * @template {unknown} L
 * @param {{ data: T; envelope: import("./self-describing.js").SelfDescribing<T, L> | null }} stored
 * @param {string} current - The current lexicon NSID
 * @param {import("./lens-registry.js").resolve} resolveLens
 * @returns {{ data: T; history: import("./self-describing.js").HistoryEntry<LensDocument | null>[] }}
 *
 * @example Migrates a stale envelope (old NSID) to the current NSID
 * ```ts
 * import { wrap } from "~/common/self-describing.js";
 * import { register, resolve } from "~/common/lens-registry.js";
 * import { migrate } from "~/common/lens.js";
 *
 * register({
 *   id: "f-old-new", source: "sh.diffuse.output.facet", target: "sh.diffuse.output.facet2",
 *   steps: [{ rename_field: { old: "favourite", new: "starred" } }],
 * });
 * const envelope = wrap([{ id: "a", favourite: true }], { schema: "sh.diffuse.output.facet" });
 * const out = migrate(
 *   { data: envelope.data, envelope },
 *   "sh.diffuse.output.facet2",
 *   resolve,
 * );
 * if (out.history.length !== 1) throw new Error("expected one history entry");
 * if (!("starred" in (out.data[0] as object))) throw new Error("expected migrated record");
 * ```
 */
export function migrate(stored, current, resolveLens) {
  const env = stored.envelope;
  /** @type {import("./self-describing.js").HistoryEntry<LensDocument | null>[]} */
  const storedHistory = /** @type {any} */ (env?.$schemaHistory ?? []);
  if (!env || env.$schema === current) {
    return { data: stored.data, history: storedHistory };
  }

  const lens = resolveLens(env.$schema, current, { history: storedHistory });
  if (!lens) {
    // No lens available for the transition; leave data as-is rather than guess.
    return { data: stored.data, history: storedHistory };
  }

  const projected = /** @type {T} */ (project(envelopeToArray(env), lens));
  return {
    data: projected,
    history: [
      ...storedHistory,
      {
        from: env.$schema,
        to: current,
        lens,
        complement: null,
      },
    ],
  };
}

/**
 * Read a stored value for a collection, migrating it to the collection's current
 * lexicon NSID if it is stale. Returns the data to expose and the (possibly
 * updated) envelope.
 *
 * @template {unknown[]} T
 * @param {unknown} value
 * @param {import("./self-describing.js").CollectionName} name
 * @param {import("./lens-registry.js").resolve} resolveLens
 * @returns {{ data: T; envelope: import("./self-describing.js").SelfDescribing<T, LensDocument | null> | null }}
 *
 * @example Reads a stale envelope and migrates it to the current NSID
 * ```ts
 * import { wrap } from "~/common/self-describing.js";
 * import { register, resolve } from "~/common/lens-registry.js";
 * import { migrateEnvelope } from "~/common/lens.js";
 *
 * register({
 *   id: "f-old-current", source: "sh.diffuse.output.facetOld", target: "sh.diffuse.output.facet",
 *   steps: [{ rename_field: { old: "favourite", new: "starred" } }],
 * });
 * // A payload stored under an OLDER lexicon NSID; the current collection NSID is
 * // sh.diffuse.output.facet, so this migrates.
 * const envelope = wrap([{ id: "a", favourite: true }], { schema: "sh.diffuse.output.facetOld" });
 * const out = migrateEnvelope(envelope, "facets", resolve);
 * if (!("starred" in (out.data[0] as object))) throw new Error("expected migrated record");
 * if (!out.envelope || out.envelope.$schema !== "sh.diffuse.output.facet") throw new Error("expected migrated envelope NSID");
 * ```
 */
export function migrateEnvelope(value, name, resolveLens) {
  const current = collectionSchema(name);
  const { data, envelope } = unwrap(value, { $schema: current });

  /** @type {import("./self-describing.js").SelfDescribing<T, LensDocument | null> | null} */
  const env = /** @type {any} */ (envelope);

  if (!env || env.$schema === current) {
    return { data: /** @type {T} */ (data), envelope: env };
  }

  const migrated = migrate({ data: /** @type {T} */ (data), envelope: env }, current, resolveLens);

  /** @type {import("./self-describing.js").SelfDescribing<T, LensDocument | null>} */
  const migratedEnvelope = {
    ...env,
    $schema: current,
    $schemaHistory: migrated.history,
    data: /** @type {T} */ (migrated.data),
  };
  return { data: /** @type {T} */ (migrated.data), envelope: migratedEnvelope };
}

/**
 * Wrap a collection into a self-describing envelope stamped with the collection's
 * current NSID, ready to be serialized by an encoder.
 *
 * @template {unknown[]} T
 * @param {T} data
 * @param {import("./self-describing.js").CollectionName} name
 * @returns {import("./self-describing.js").SelfDescribing<T, LensDocument | null>}
 *
 * @example Wraps records for a collection
 * ```ts
 * import { encodeCollection } from "~/common/lens.js";
 *
 * const envelope = encodeCollection([{ id: "a" }], "facets");
 * if (envelope.$schema !== "sh.diffuse.output.facet") throw new Error("expected facet NSID");
 * if (envelope.data[0].id !== "a") throw new Error("expected data");
 * ```
 */
export function encodeCollection(data, name) {
  return wrap(data, { schema: collectionSchema(name) });
}

/**
 * Decode a stored value (envelope or legacy) into a collection, migrating it to
 * the collection's current NSID if stale. Returns `null` when `value` is `null`
 * or `undefined`, so encoders can map that to an empty collection.
 *
 * @template {unknown[]} T
 * @param {unknown} value
 * @param {import("./self-describing.js").CollectionName} name
 * @returns {T | null}
 */
export function decodeCollection(value, name) {
  if (value === null || value === undefined) return null;
  const { data } = migrateEnvelope(value, name, resolve);
  return /** @type {T} */ (data);
}

/**
 * Encode a collection as self-describing JSON, either as a string or as bytes.
 *
 * @param {unknown[]} data
 * @param {import("./self-describing.js").CollectionName} name
 * @param {boolean} [asBytes]
 * @returns {string | Uint8Array}
 *
 * @example Encodes a collection as a JSON string
 * ```ts
 * import { encodeJsonCollection } from "~/common/lens.js";
 *
 * const out = encodeJsonCollection([{ id: "a" }], "tracks") as string;
 * const parsed = JSON.parse(out) as { $schema: string };
 * if (parsed.$schema !== "sh.diffuse.output.track") throw new Error("expected track NSID");
 * ```
 *
 * @example Encodes a collection as JSON bytes
 * ```ts
 * import { encodeJsonCollection } from "~/common/lens.js";
 *
 * const out = encodeJsonCollection([{ id: "a" }], "tracks", true);
 * if (!(out instanceof Uint8Array)) throw new Error("expected bytes");
 * ```
 */
export function encodeJsonCollection(data, name, asBytes = false) {
  const json = JSON.stringify(encodeCollection(data, name));
  return asBytes ? new TextEncoder().encode(json) : json;
}

/**
 * Decode a JSON-encoded collection (string, bytes, or an already-parsed object —
 * an envelope or legacy array), migrating stale payloads. `undefined`/`null`
 * yields an empty collection.
 *
 * @template {unknown[]} T
 * @param {Uint8Array | string | object | null | undefined} raw
 * @param {import("./self-describing.js").CollectionName} name
 * @returns {T}
 *
 * @example Round-trips through encodeJsonCollection
 * ```ts
 * import { encodeJsonCollection, decodeJsonCollection } from "~/common/lens.js";
 *
 * const bytes = encodeJsonCollection([{ id: "a" }], "tracks", true) as Uint8Array;
 * const out = decodeJsonCollection(bytes, "tracks") as Array<{ id: string }>;
 * if (out[0].id !== "a") throw new Error("expected record");
 * ```
 *
 * @example Empty for undefined input
 * ```ts
 * import { decodeJsonCollection } from "~/common/lens.js";
 *
 * if (decodeJsonCollection(undefined, "tracks").length !== 0) throw new Error("expected empty");
 * ```
 */
export function decodeJsonCollection(raw, name) {
  try {
    let parsed;
    if (raw instanceof Uint8Array) {
      parsed = JSON.parse(new TextDecoder().decode(raw));
    } else if (raw === undefined || raw === null) {
      return /** @type {T} */ (/** @type {unknown} */ ([]));
    } else if (typeof raw === "string") {
      parsed = JSON.parse(raw);
    } else {
      // Already-parsed value (e.g. a stored envelope object).
      parsed = raw;
    }
    return normalizeCollection(decodeCollection(parsed, name));
  } catch (err) {
    console.error(err);
    return /** @type {T} */ (/** @type {unknown} */ ([]));
  }
}

/**
 * Guarantee a collection is always returned as an array.
 *
 * @template {unknown[]} T
 * @param {T | null | undefined | unknown} value
 * @returns {T}
 */
function normalizeCollection(value) {
  if (Array.isArray(value)) return /** @type {T} */ (value);
  return /** @type {T} */ (/** @type {unknown} */ ([]));
}

/**
 * Write back an edited record into the collection's current shape, losslessly.
 *
 * This is the panproto write-back path and the ONLY place the migration flow
 * loads `@panproto/core` (its WASM). It is never called on a plain read — it runs
 * only when an app edits a record in an older shape and needs to store it back in
 * the current shape, using the lens + complement recorded in `$schemaHistory`.
 *
 * When a complement is available (captured when the data was migrated forward), it
 * is used to reconstruct the discarded fields so the old app's edit is preserved
 * losslessly; otherwise the forward projection is pure-JS and panproto is not
 * loaded.
 *
 * @param {unknown} editedRecord - the record as the (older) app edited it
 * @param {{ lens: LensDocument; toLexicon?: object; complement?: Uint8Array | string | null }} opts
 * @returns {Promise<unknown>} the record in the current shape, ready to store
 *
 * @example Runs the write-back for an empty lens (loads panproto lazily)
 * ```ts
 * import { writeBack } from "~/common/lens.js";
 *
 * const out = await writeBack({ $type: "sh.diffuse.output.facet", id: "a", name: "x", favourite: true }, {
 *   lens: { id: "l", source: "sh.diffuse.output.facet", target: "sh.diffuse.output.facet2", steps: [] },
 *   toLexicon: {
 *     lexicon: 1, id: "sh.diffuse.output.facet2",
 *     defs: { main: { type: "record", record: { type: "object", properties: {
 *       $type: { type: "string" }, id: { type: "string" }, name: { type: "string" },
 *     } } } },
 *   },
 * });
 * if (typeof out !== "object") throw new Error("expected a record back");
 * ```
 */
export async function writeBack(editedRecord, { lens, toLexicon, complement }) {
  // Only load panproto (WASM) when a complement is actually used for lossless
  // write-back and we have the target lexicon to instantiate it; otherwise the
  // common additive/rename path stays pure-JS via `project`.
  if (!complement || !toLexicon) {
    return project([editedRecord], lens)[0];
  }

  const schema = await parseLexicon(toLexicon);
  const comp = typeof complement === "string"
    ? base64ToBytes(complement)
    : /** @type {Uint8Array} */ (complement);
  return panprotoPut(lens, schema, editedRecord, comp);
}

/**
 * Write a collection back into its stored (possibly newer) shape on save.
 *
 * Called by encoders before wrapping a save into the envelope. If the stored
 * envelope's `$schemaHistory` ends at a non-null `complement` for the transition
 * to the current NSID, each record is written back (loading panproto lazily to
 * preserve discarded fields); otherwise the records pass through unchanged and no
 * WASM is loaded.
 *
 * @template {unknown[]} T
 * @param {T} items
 * @param {import("./self-describing.js").CollectionName} name
 * @param {import("./self-describing.js").SelfDescribing<T, LensDocument | null> | null} storedEnvelope
 * @param {object} [toLexicon]
 * @returns {Promise<T>}
 *
 * @example Passes records through when the stored envelope already matches (no panproto)
 * ```ts
 * import { writeBackCollection, encodeCollection } from "~/common/lens.js";
 *
 * const items = [{ id: "a" }];
 * const envelope = encodeCollection(items, "tracks");
 * const out = await writeBackCollection(items, "tracks", envelope);
 * if (out[0].id !== "a") throw new Error("expected unchanged records");
 * ```
 *
 * @example Passes records through when no lexicon is available (no panproto)
 * ```ts
 * import { writeBackCollection } from "~/common/lens.js";
 * import { wrap } from "~/common/self-describing.js";
 *
 * const items = [{ id: "a" }];
 * const envelope = wrap([{ id: "a" }], { schema: "sh.diffuse.output.track" }) as {
 *   $schema: string; $schemaHistory: any[]; data: unknown[];
 * };
 * // stored under an older NSID with a complement-carrying history, but no lexicon:
 * const out = await writeBackCollection(items, "tracks", {
 *   ...envelope, $schema: "sh.diffuse.output.trackOld",
 *   $schemaHistory: [{ from: "sh.diffuse.output.trackOld", to: "sh.diffuse.output.track", lens: { id: "l", source: "s", target: "t", steps: [] }, complement: new Uint8Array(1) }],
 * }) as Array<{ id: string }>;
 * if (out[0].id !== "a") throw new Error("expected unchanged records without a lexicon");
 * ```
 */
export async function writeBackCollection(items, name, storedEnvelope, toLexicon) {
  if (!storedEnvelope || storedEnvelope.$schema === collectionSchema(name)) {
    // Nothing stale to write back — the stored envelope is already this build's
    // shape, or there is no history. Pass through (no panproto load).
    return items;
  }

  const entry = storedEnvelope.$schemaHistory[storedEnvelope.$schemaHistory.length - 1];
  const lens = entry?.lens;
  const complement = entry?.complement;
  if (!lens || !complement || !toLexicon) {
    // No complement recorded, or no lexicon to instantiate against — nothing
    // panproto could losslessly write back; keep the records as-is.
    return items;
  }

  const written = await Promise.all(
    /** @type {unknown[]} */ (items).map((item) =>
      writeBack(item, { lens, toLexicon, complement }),
    ),
  );
  return /** @type {T} */ (written);
}

/**
 * Reconstruct the stored self-describing envelope of a collection from its raw
 * encoded value (a JSON string/bytes, or a stored envelope object). Returns
 * `null` when the value is absent/legacy (no envelope).
 *
 * @template {unknown[]} T
 * @param {Uint8Array | string | unknown} raw - the raw stored payload for the collection
 * @param {import("./self-describing.js").CollectionName} name
 * @returns {import("./self-describing.js").SelfDescribing<T, LensDocument | null> | null}
 *
 * @example Reads the envelope out of a stored JSON string
 * ```ts
 * import { readStoredEnvelope, encodeJsonCollection } from "~/common/lens.js";
 *
 * const stored = encodeJsonCollection([{ id: "a" }], "tracks", true);
 * const env = readStoredEnvelope(stored, "tracks");
 * if (!env || env.$schema !== "sh.diffuse.output.track") throw new Error("expected stored envelope");
 * ```
 *
 * @example Returns null for absent/legacy (bare array) data
 * ```ts
 * import { readStoredEnvelope } from "~/common/lens.js";
 *
 * if (readStoredEnvelope(null, "tracks") !== null) throw new Error("expected null for absent");
 * if (readStoredEnvelope([{ id: "a" }], "tracks") !== null) throw new Error("expected null for legacy array");
 * ```
 */
export function readStoredEnvelope(raw, name) {
  if (raw === null || raw === undefined) return null;
  let parsed = raw;
  if (raw instanceof Uint8Array) {
    parsed = JSON.parse(new TextDecoder().decode(raw));
  } else if (typeof raw === "string") {
    parsed = JSON.parse(raw);
  }
  const { envelope } = unwrap(parsed, { $schema: collectionSchema(name) });
  return /** @type {import("./self-describing.js").SelfDescribing<T, LensDocument | null> | null} */ (
    /** @type {any} */ (envelope)
  );
}

/**
 * The write path for a JSON-encoded collection: write back any stale records to
 * the stored shape (guarded no-op), then encode. Used by the JSON encoders' save
 * so the write-back path is wired on save without loading panproto unless a
 * cross-NSID complement + lexicon make it necessary.
 *
 * @param {unknown[]} items
 * @param {import("./self-describing.js").CollectionName} name
 * @param {Uint8Array | string | null | undefined} stored - the raw stored payload
 * @param {boolean} [asBytes]
 * @returns {Promise<string | Uint8Array>}
 *
 * @example Encodes a collection through the wired save path (guarded no-op)
 * ```ts
 * import { saveJsonCollection, decodeJsonCollection } from "~/common/lens.js";
 *
 * const out = await saveJsonCollection([{ id: "a" }], "tracks", null) as string;
 * const back = decodeJsonCollection(out, "tracks") as Array<{ id: string }>;
 * if (back[0].id !== "a") throw new Error("expected encoded records");
 * ```
 *
 * @example Produces bytes when asBytes is set
 * ```ts
 * import { saveJsonCollection } from "~/common/lens.js";
 *
 * const out = await saveJsonCollection([{ id: "a" }], "tracks", null, true);
 * if (!(out instanceof Uint8Array)) throw new Error("expected bytes");
 * ```
 */
export async function saveJsonCollection(items, name, stored, asBytes = false) {
  const storedEnvelope = readStoredEnvelope(stored, name);
  const written = await writeBackCollection(items, name, storedEnvelope, undefined);
  return encodeJsonCollection(written, name, asBytes);
}

/**
 * @param {string} base64
 * @returns {Uint8Array}
 */
function base64ToBytes(base64) {
  const bin = atob(base64);
  const bytes = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
  return bytes;
}

/**
 * @template {unknown[]} T
 * @template {unknown} L
 * @param {import("./self-describing.js").SelfDescribing<T, L>} envelope
 */
function envelopeToArray(envelope) {
  return /** @type {unknown[]} */ (envelope.data);
}

/**
 * A sensible default for an `add_field` kind.
 *
 * @param {string} kind
 * @returns {unknown}
 */
function defaultValue(kind) {
  switch (kind) {
    case "boolean":
      return false;
    case "integer":
    case "float":
    case "number":
      return 0;
    case "array":
      return [];
    case "object":
      return {};
    case "null":
      return null;
    default:
      return "";
  }
}