import * as Automerge from "@automerge/automerge";
import { isUint8Array } from "iso-base/utils";

import {
  recursivelyCloneRecords,
  removeUndefinedValuesFromRecord,
} from "~/common/utils.js";

/**
 * @import {Heads} from "@automerge/automerge"
 */

/**
 * The document shape used for every collection.
 *
 * Records live in a map keyed by `id` (not in an array) so that concurrent
 * edits to different records merge on their own, and `order` keeps the array
 * order the output interface expects without making a reorder rewrite every
 * record.
 *
 * @typedef {Object} CollectionDocument
 * @property {string} [$schema] - Lexicon NSID the records conform to.
 * @property {string[]} order
 * @property {Record<string, any>} records
 */

/** @returns {CollectionDocument} */
export function emptyDocument() {
  return { order: [], records: {} };
}

/**
 * A fresh, empty document. Every collection starts from one of these when
 * there is nothing cached locally.
 *
 * @param {string} [actor] - This device's actor id; omit only for throwaway docs.
 * @returns {AutomergeDoc}
 */
export function createDocument(actor) {
  return actor
    ? Automerge.from(emptyDocument(), { actor })
    : Automerge.from(emptyDocument());
}

/**
 * @typedef {Automerge.Doc<CollectionDocument>} AutomergeDoc
 *
 * @param {unknown} value
 * @returns {AutomergeDoc | undefined}
 */
export function loadDocument(value) {
  if (value === undefined || value === null) return undefined;
  if (!isUint8Array(value)) throw new Error("Invalid data type");

  return normalize(Automerge.load(value));
}

/**
 * @param {Automerge.Doc<CollectionDocument> | CollectionDocument} doc
 * @returns {Automerge.Doc<CollectionDocument>}
 */
export function normalize(doc) {
  // Documents written by the old array-shaped transformer. Kept so a stale
  // local cache is upgraded instead of silently reporting an empty collection.
  const legacy = /** @type {any} */ (doc);
  if (!legacy.records && Array.isArray(legacy.collection)) {
    const records = Object.fromEntries(
      legacy.collection.map((/** @type {any} */ item) => [item.id, item]),
    );

    return Automerge.change(
      Automerge.from(emptyDocument()),
      (d) => {
        if (legacy.$schema) d.$schema = legacy.$schema;
        for (const [id, item] of Object.entries(records)) {
          d.records[id] = item;
        }
        d.order = Object.keys(records);
      },
    );
  }

  return /** @type {Automerge.Doc<CollectionDocument>} */ (doc);
}

/**
 * @param {Automerge.Doc<CollectionDocument>} doc
 * @returns {any[]}
 */
export function recordsOf(doc) {
  const ordered = doc.order
    .map((id) => normalizeLists(doc.records[id]))
    .filter((record) => record !== undefined);

  // `order` is a plain list, so two devices adding different records at the
  // same time conflict over it and one list wins. The records themselves are
  // unaffected, so surface any that the winning order doesn't mention.
  const seen = new Set(doc.order);
  for (const [id, record] of Object.entries(doc.records)) {
    if (seen.has(id) || record === undefined) continue;

    ordered.push(normalizeLists(record));
  }

  return ordered;
}

/**
 * Turn objects that are really arrays back into arrays.
 *
 * An earlier version of `recursivelyCloneRecords` spread arrays into objects
 * with numeric keys, so list fields in documents written before that fix are
 * stored as `{ 0: …, 1: … }`. Normalizing on read repairs them, and the next
 * save writes real arrays again.
 *
 * @param {any} value
 * @returns {any}
 */
function normalizeLists(value) {
  if (Array.isArray(value)) return value.map(normalizeLists);
  if (value === null || typeof value !== "object") return value;

  const entries = Object.entries(value);
  const normalized = entries.map(([key, item]) => [key, normalizeLists(item)]);

  const isList = normalized.length > 0 &&
    normalized.every(([key], index) => key === String(index));

  return isList
    ? normalized.map(([, item]) => item)
    : Object.fromEntries(normalized);
}

/**
 * Apply a whole collection (as the output interface hands it over) to a
 * document, touching only the records that actually changed.
 *
 * @param {Automerge.Doc<CollectionDocument>} doc
 * @param {any[]} items
 * @param {{ schema?: string; stripUndefined?: boolean }} [options]
 * @returns {{ doc: Automerge.Doc<CollectionDocument>; changed: boolean }}
 */
export function changeDocument(doc, items, options = {}) {
  /** @type {Map<string, any>} */
  const next = new Map();

  for (const item of items) {
    const cloned = recursivelyCloneRecords(item);
    next.set(
      item.id,
      options.stripUndefined ? removeUndefinedValuesFromRecord(cloned) : cloned,
    );
  }

  const order = [...next.keys()];

  let changed = !sameArray(doc.order, order) || doc.$schema !== options.schema;
  if (!changed) {
    for (const [id, item] of next) {
      if (!sameRecord(doc.records[id], item)) {
        changed = true;
        break;
      }
    }
  }
  if (!changed && Object.keys(doc.records).length !== next.size) changed = true;
  if (!changed) return { doc, changed: false };

  const nextDoc = Automerge.change(doc, (d) => {
    if (options.schema) d.$schema = options.schema;

    for (const id of Object.keys(d.records)) {
      if (!next.has(id)) delete d.records[id];
    }

    for (const [id, item] of next) {
      if (sameRecord(d.records[id], item)) continue;
      d.records[id] = item;
    }

    if (!sameArray(d.order, order)) d.order = order;
  });

  return { doc: nextDoc, changed: true };
}

/**
 * The bytes to append to a log for everything added since `heads`.
 *
 * Falls back to a full document when the heads are unknown (a fresh device, a
 * wiped log, or a log compacted by someone else) — `saveSince` throws on heads
 * the document no longer contains.
 *
 * @param {Automerge.Doc<CollectionDocument>} doc
 * @param {Heads | undefined} heads
 * @returns {Uint8Array}
 */
export function changesSince(doc, heads) {
  const bytes = (() => {
    if (!heads || heads.length === 0) return Automerge.save(doc);

    try {
      return Automerge.saveSince(doc, heads);
    } catch {
      return Automerge.save(doc);
    }
  })();

  return bytes;
}

/** @param {Automerge.Doc<CollectionDocument>} doc */
export function encodeDocument(doc) {
  return Automerge.save(doc);
}

/** @param {AutomergeDoc} doc */
export function headsOf(doc) {
  return Automerge.getHeads(doc);
}

/**
 * Apply one log entry to a document.
 *
 * Entries are usually a change (what an incremental push appends), but a
 * bootstrap or a compaction writes a whole document — and `loadIncremental`
 * silently ignores those. So every entry is also merged in as a document when
 * it parses on its own; a change chunk that doesn't (its dependencies are
 * missing) is already applied.
 *
 * Throws when the entry can't be applied *yet* (missing dependencies), so the
 * caller can retry it after the rest of the log.
 *
 * @param {AutomergeDoc} doc
 * @param {Uint8Array} bytes
 * @returns {AutomergeDoc}
 */
export function applyPack(doc, bytes) {
  const next = Automerge.loadIncremental(doc, bytes);

  try {
    return mergeDocuments(next, Automerge.load(bytes));
  } catch {
    return next;
  }
}

/**
 * Apply a whole log — every entry of a log-shaped output concatenated — to a
 * collection.
 *
 * The log is replayed entry by entry rather than in one pass: entries from
 * several devices interleave, a device's first entry is a whole document while
 * its later entries are deltas against it, and `keep` trims each device's
 * window independently — so a single-pass load either throws ("change's deps
 * should already be in the document") or silently keeps only one root's
 * records.
 *
 * A device with no records of its own adopts the first entry that loads as a
 * document, which also gives it this device's actor id. Change entries apply
 * incrementally; an entry that parses as a document on its own is merged in —
 * `mergeDocuments` keeps the records a plain merge of two roots would drop.
 * Entries that still can't apply (dependencies trimmed away) are retried once
 * after the rest, then dropped — the writer's next whole-document entry
 * supersedes them.
 *
 * @param {AutomergeDoc} doc
 * @param {Uint8Array} bytes
 * @param {string} [actor]
 * @returns {AutomergeDoc}
 */
export function applyLog(doc, bytes, actor) {
  const entries = splitLogEntries(bytes);

  if (recordsOf(doc).length === 0) {
    for (let index = 0; index < entries.length; index++) {
      let adopted;

      try {
        adopted = adoptDocument(entries[index], actor);
      } catch {
        continue; // not a document (an orphan delta); keep looking
      }

      return applyLogEntries(adopted, entries.slice(index + 1));
    }

    // Nothing loads on its own (a log of orphaned deltas): leave the document
    // as it is; the writer's next whole-document entry supersedes them.
    return doc;
  }

  return applyLogEntries(doc, entries);
}

////////////////////////////////////////////
// LOG FRAMING
////////////////////////////////////////////

/** Bytes of the u32be length prefix in front of each decrypted log entry. */
const LENGTH_PREFIX = 4;

/**
 * Split a decrypted log value into its entries.
 *
 * The encrypt layer length-prefixes each decrypted entry (u32be) precisely so
 * this is a walk over our own framing instead of a parse of automerge's
 * container format.
 *
 * A log written before entries were framed starts with the payload itself —
 * its first length header is then implausibly large — and is kept as one
 * chunk so it still applies, best effort. A truncated tail is dropped.
 *
 * @param {Uint8Array} bytes
 * @returns {Uint8Array[]}
 */
function splitLogEntries(bytes) {
  /** @type {Uint8Array[]} */
  const entries = [];

  let offset = 0;

  while (offset < bytes.length) {
    if (offset + LENGTH_PREFIX > bytes.length) break; // trailing fragment

    const length = new DataView(
      bytes.buffer,
      bytes.byteOffset + offset,
      LENGTH_PREFIX,
    ).getUint32(0);

    const start = offset + LENGTH_PREFIX;
    const end = start + length;

    if (end > bytes.length) {
      // An implausible header means this log predates the framing: keep it
      // whole so it still applies, best effort.
      if (offset === 0) entries.push(bytes);

      break;
    }

    entries.push(bytes.subarray(start, end));
    offset = end;
  }

  return entries;
}

/**
 * Replay log entries in order, tolerating entries that cannot apply.
 *
 * An entry that parses as a document on its own (a bootstrap or snapshot of
 * any writer) is merged in — a plain merge keeps one root, so
 * `mergeDocuments` re-adds the records the merge would drop. Change entries
 * apply incrementally. Anything unapplyable (a delta whose dependencies were
 * trimmed away, or rooted in another writer's document) is retried once, then
 * dropped — the writer's next whole-document entry supersedes it.
 *
 * @param {AutomergeDoc} doc
 * @param {Uint8Array[]} entries
 * @returns {AutomergeDoc}
 */
function applyLogEntries(doc, entries) {
  let current = doc;

  let deferred = entries;

  for (let pass = 0; pass < 2 && deferred.length > 0; pass++) {
    const pending = deferred;
    deferred = [];

    for (const entry of pending) {
      // A whole document parses on its own; a change entry does not.
      try {
        current = mergeDocuments(current, normalize(Automerge.load(entry)));
        continue;
      } catch {
        // Not a standalone document; apply it as changes below.
      }

      try {
        current = Automerge.loadIncremental(current, entry);
        continue;
      } catch (err) {
        if (pass > 0) {
          console.warn("Dropping a log entry that could not be applied:", err);
        }

        deferred.push(entry);
      }
    }
  }

  return current;
}

/**
 * Union two documents, including documents that were created independently.
 *
 * Automerge documents each grow from their own root object, and merging two
 * roots keeps one of them — so two devices that started offline (each with an
 * empty document of their own) would otherwise lose one side's records
 * entirely. Re-add whatever the merge dropped.
 *
 * @param {AutomergeDoc} a
 * @param {AutomergeDoc} b
 * @returns {AutomergeDoc}
 */
export function mergeDocuments(a, b) {
  const aRecords = Object.entries(a.records ?? {});
  const bRecords = Object.entries(b.records ?? {});

  const merged = Automerge.merge(a, b);

  const fallback = new Map([...aRecords, ...bRecords]);
  const missing = [...fallback.keys()].filter((id) => merged.records?.[id] === undefined);
  if (missing.length === 0) return merged;

  const order = [...merged.order ?? []];
  for (const id of missing) {
    if (!order.includes(id)) order.push(id);
  }

  return Automerge.change(merged, (d) => {
    for (const id of missing) {
      // Records from the other document are Automerge handles tied to it; a
      // plain copy has to be written into this one.
      d.records[id] = recursivelyCloneRecords(fallback.get(id));
    }

    d.order = order;
  });
}

/**
 * Start from a log entry instead of merging into our own empty document.
 *
 * Every document grows from its own root object, and merging two roots keeps
 * both lineages: records written afterwards land in one of them and stay
 * invisible to the other. A device with nothing of its own should therefore
 * adopt the shared document rather than merge with it.
 *
 * The document is cloned with this device's actor id. Without that, a device
 * that adopted a document would keep writing changes under the previous
 * writer's actor and collide with it on sequence numbers.
 *
 * Throws when the bytes aren't a document (a change chunk whose dependencies
 * are missing).
 *
 * @param {Uint8Array} bytes
 * @param {string} [actor]
 * @returns {AutomergeDoc}
 */
export function adoptDocument(bytes, actor) {
  const loaded = normalize(Automerge.load(bytes));

  return actor ? Automerge.clone(loaded, { actor }) : loaded;
}

/**
 * @param {AutomergeDoc} doc
 * @param {Uint8Array} bytes
 */
export function mergeDocument(doc, bytes) {
  return mergeDocuments(doc, Automerge.load(bytes));
}

/**
 * @param {string[]} a
 * @param {string[]} b
 */
function sameArray(a, b) {
  if (a.length !== b.length) return false;

  return a.every((value, index) => value === b[index]);
}

/**
 * @param {unknown} a
 * @param {unknown} b
 */
function sameRecord(a, b) {
  if (a === b) return true;
  if (a === undefined || b === undefined) return false;

  // Records are plain JSON values, so a stable stringify is an adequate
  // structural comparison — and much cheaper than walking them by hand.
  return JSON.stringify(a) === JSON.stringify(b);
}
