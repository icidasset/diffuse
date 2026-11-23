import { LoroDoc, VersionVector } from "loro-crdt/base64";

/**
 * A stable fingerprint of a document version: the encoded Loro version vector
 * as a one-element list, so cursors and log bookkeeping can keep comparing
 * plain string arrays.
 *
 * @typedef {string[]} Heads
 */

/**
 * The document layer of the Firkin output, backed by Loro.
 *
 * A collection is one Loro document with two root map containers:
 *
 * - `records`: record id → the record's JSON text. Storing the record as its
 *   JSON text makes a whole record one value: last-writer-wins per record, which
 *   is what every other output does, and far cheaper than nesting one operation
 *   per field.
 * - `meta`: the collection's lexicon NSID and the record order.
 *
 * Loro keeps the history that makes the append-only log work, and a snapshot or
 * an update is an opaque byte array, so the log protocol doesn't change.
 *
 * @typedef {LoroDoc} CollectionDocument
 */

const RECORDS = "records";
const META = "meta";

////////////////////////////////////////////
// RECORD SHAPE
////////////////////////////////////////////

/**
 * @param {any} item
 * @returns {string}
 */
const encodeRecord = (item) => JSON.stringify(item);

/**
 * @param {any} value
 * @returns {any}
 */
const decodeRecord = (value) => JSON.parse(value);

/**
 * Whether a stored record matches the incoming one.
 *
 * Tracks bump `updatedAt` on every edit, so their version is enough to decide.
 * The other collections don't guarantee that, so they compare the encoded text.
 *
 * @param {string | undefined} a - The stored record.
 * @param {any} b
 * @param {boolean} [versioned]
 */
function sameRecord(a, b, versioned = false) {
  if (a === undefined) return false;

  const encoded = typeof b === "string" ? b : encodeRecord(b);
  if (a === encoded) return true;
  if (!versioned) return false;

  return sameUpdatedAt(parseRecord(a), b);
}

/**
 * Whether heads fingerprint `a` includes everything in `b`.
 *
 * The push cursor must only move forwards. A pull and a push can finish in the
 * opposite order from the one they started in, and a cursor that moves
 * backwards makes the next pull believe the local document has unpushed
 * changes — which it appends, hinting the other device and looping.
 *
 * @param {Heads} a
 * @param {Heads} b
 */
export function headsAtLeast(a, b) {
  const left = versionVectorFromHeads(a);
  const right = versionVectorFromHeads(b);

  if (!left || !right) return false;

  const entries = right.toJSON();
  const pairs = entries instanceof Map
    ? entries
    : Object.entries(entries ?? {});

  for (const [peer, counter] of pairs) {
    // Peer ids are decimal strings; `get` wants a number or a bigint.
    const id = typeof peer === "string" ? BigInt(peer) : peer;

    if ((left.get(id) ?? 0) < counter) return false;
  }

  return true;
}

/**
 * Whether two heads fingerprints describe the same version.
 *
 * Loro's encoding preserves peer insertion order, so the same version vector
 * can encode to different strings depending on how a document learned about its
 * peers. Compare what the versions include, not their encodings.
 *
 * @param {Heads} a
 * @param {Heads} b
 */
export function headsEqual(a, b) {
  return headsAtLeast(a, b) && headsAtLeast(b, a);
}

/**
 * @param {any} a
 * @param {any} b
 */
function sameUpdatedAt(a, b) {
  const aUpdatedAt = a?.updatedAt;
  const bUpdatedAt = b?.updatedAt;

  return typeof aUpdatedAt === "string" && typeof bUpdatedAt === "string" &&
    aUpdatedAt === bUpdatedAt;
}

/**
 * @param {string} value
 */
function parseRecord(value) {
  try {
    return JSON.parse(value);
  } catch {
    return undefined;
  }
}

////////////////////////////////////////////
// PEER IDENTITY
////////////////////////////////////////////

/**
 * Loro identifies a writer by a numeric peer id. The output already persists a
 * random per-document id (an "actor" in the previous engine), which is reused
 * here as the peer id.
 *
 * @param {string} actor - Hex.
 * @returns {bigint}
 */
function peerIdOf(actor) {
  if (typeof actor !== "string" || !/^[0-9a-f]+$/i.test(actor)) {
    return BigInt(Math.floor(Math.random() * 2 ** 52)) + 1n;
  }

  const value = BigInt(`0x${actor.slice(0, 16)}`);

  return value === 0n ? 1n : value;
}

/**
 * The document's peer id as hex, for tests and diagnostics.
 *
 * @param {CollectionDocument} doc
 */
export function actorOf(doc) {
  return doc.peerId.toString(16);
}

////////////////////////////////////////////
// DOCUMENT
////////////////////////////////////////////

/**
 * A fresh, empty document.
 *
 * @param {string} [actor] - This device's id; omit only for throwaway docs.
 * @returns {CollectionDocument}
 */
export function createDocument(actor) {
  const doc = new LoroDoc();

  if (actor) doc.setPeerId(peerIdOf(actor));

  // Register both root containers up front so a snapshot taken later does not
  // change just because a read touched one.
  doc.getMap(RECORDS);
  doc.getMap(META);

  return doc;
}

/**
 * Read a document from its snapshot bytes.
 *
 * Returns `undefined` when the bytes are not a Loro snapshot or update — which
 * is how a cache or a log entry written by the previous engine is recognised.
 *
 * @param {unknown} value
 * @returns {CollectionDocument | undefined}
 */
export function loadDocument(value) {
  if (value === undefined || value === null) return undefined;
  if (!(value instanceof Uint8Array)) {
    throw new TypeError("Invalid data type");
  }

  const doc = new LoroDoc();

  try {
    doc.import(value);
  } catch {
    return undefined;
  }

  doc.commit();

  return doc;
}

/**
 * Whether a document has any records.
 *
 * @param {CollectionDocument} doc
 */
export function hasRecords(doc) {
  return doc.getMap(RECORDS).size > 0;
}

////////////////////////////////////////////
// READING
////////////////////////////////////////////

/**
 * Turn objects that are really arrays back into arrays.
 *
 * An earlier version of the record encoder spread arrays into objects with
 * numeric keys, so documents written before that fix hold `{ 0: …, 1: … }`.
 * Those records still arrive here through the legacy conversion, and
 * normalising on read repairs them.
 *
 * @param {any} value
 * @returns {any}
 */
function normalizeLists(value) {
  if (Array.isArray(value)) {
    let changed = false;

    for (const item of value) {
      if (item === null || typeof item !== "object") continue;
      if (normalizeLists(item) !== item) {
        changed = true;
        break;
      }
    }

    return changed ? value.map(normalizeLists) : value;
  }

  if (value === null || typeof value !== "object") return value;

  const entries = Object.entries(value);
  const normalized = entries.map(([key, item]) => [key, normalizeLists(item)]);
  const isList = entries.length > 0 &&
    normalized.every(([key], index) => key === String(index));

  if (isList) return normalized.map(([, item]) => item);

  return normalized.every(([, item], index) => item === entries[index][1])
    ? value
    : Object.fromEntries(normalized);
}

/**
 * The stored order, if any.
 *
 * @param {CollectionDocument} doc
 * @returns {string[]}
 */
function readOrder(doc) {
  const raw = doc.getMap(META).get("order");
  if (typeof raw !== "string") return [];

  try {
    const parsed = JSON.parse(raw);
    return Array.isArray(parsed)
      ? parsed.filter((id) => typeof id === "string")
      : [];
  } catch {
    return [];
  }
}

/**
 * The collection, in display order, with any record the order doesn't mention
 * appended.
 *
 * @param {CollectionDocument} doc
 * @returns {any[]}
 */
export function recordsOf(doc) {
  const records = doc.getMap(RECORDS);
  /** @type {any[]} */
  const items = [];
  const seen = new Set();

  for (const id of readOrder(doc)) {
    const value = records.get(id);
    if (value === undefined) continue;

    seen.add(id);
    items.push(normalizeLists(decodeRecord(value)));
  }

  // Order is last-writer-wins, so a concurrent add from another device may not
  // be in it. The record itself is there; surface it instead of losing it.
  for (const id of records.keys()) {
    if (seen.has(id)) continue;

    const value = records.get(id);
    if (value === undefined) continue;

    items.push(normalizeLists(decodeRecord(value)));
  }

  return items;
}

////////////////////////////////////////////
// WRITING
////////////////////////////////////////////

/**
 * The stored record text, or `undefined` when the id isn't in the document.
 *
 * @param {any} records - The `records` map.
 * @param {string} id
 * @returns {string | undefined}
 */
function recordAt(records, id) {
  const value = records.get(id);

  return typeof value === "string" ? value : undefined;
}

/**
 * @param {CollectionDocument} doc
 * @param {any[]} items
 * @param {boolean} versioned
 */
function diff(doc, items, versioned) {
  const records = doc.getMap(RECORDS);
  const ids = new Set();
  /** @type {string[]} */
  const order = [];
  /** @type {Array<[string, string]>} */
  const changed = [];

  for (const item of items) {
    if (ids.has(item.id)) continue;

    ids.add(item.id);
    order.push(item.id);

    if (!sameRecord(recordAt(records, item.id), item, versioned)) {
      changed.push([item.id, encodeRecord(item)]);
    }
  }

  /** @type {string[]} */
  const removed = [];
  for (const id of records.keys()) {
    if (!ids.has(id)) removed.push(id);
  }

  return { order, changed, removed };
}

/**
 * Count how many records a save would add, update, or remove.
 *
 * @param {CollectionDocument} doc
 * @param {any[]} items
 * @param {boolean} [versioned]
 * @returns {{ changed: number; removed: number }}
 */
export function countChanges(doc, items, versioned = false) {
  const { changed, removed } = diff(doc, items, versioned);

  return { changed: changed.length, removed: removed.length };
}

/**
 * Apply a whole collection (as the output interface hands it over).
 *
 * Mutates the document and returns it. Loro commits are cheap, so a save is one
 * transaction regardless of collection size.
 *
 * @param {CollectionDocument} doc
 * @param {any[]} items
 * @param {{ schema?: string; versioned?: boolean }} [options]
 * @returns {{ doc: CollectionDocument; changed: boolean }}
 */
export function changeDocument(doc, items, options = {}) {
  const records = doc.getMap(RECORDS);
  const meta = doc.getMap(META);
  const { order, changed, removed } = diff(doc, items, options.versioned ?? false);

  const encodedOrder = JSON.stringify(order);
  const orderChanged = meta.get("order") !== encodedOrder;
  const schemaChanged = Boolean(options.schema) &&
    meta.get("$schema") !== options.schema;

  if (
    changed.length === 0 && removed.length === 0 && !orderChanged &&
    !schemaChanged
  ) {
    return { doc, changed: false };
  }

  for (const [id, value] of changed) records.set(id, value);
  for (const id of removed) records.delete(id);
  if (orderChanged) meta.set("order", encodedOrder);
  if (schemaChanged) meta.set("$schema", options.schema);
  doc.commit();

  return { doc, changed: true };
}

/**
 * `changeDocument` with a batching interface.
 *
 * Loro applies a collection in one transaction, so the batches are purely a
 * compatibility shape: the collection is written once and `onBatch` is called
 * with the result.
 *
 * @param {CollectionDocument} doc
 * @param {any[]} items
 * @param {{ schema?: string; versioned?: boolean }} [options]
 * @param {() => Promise<void>} [yieldToMain]
 * @param {(doc: CollectionDocument) => void} [onBatch]
 * @returns {Promise<{ doc: CollectionDocument; changed: boolean }>}
 */
export async function changeDocumentBatched(
  doc,
  items,
  options = {},
  yieldToMain = async () => {},
  onBatch,
) {
  const result = changeDocument(doc, items, options);

  if (result.changed) onBatch?.(result.doc);

  return result;
}

////////////////////////////////////////////
// ENCODING
////////////////////////////////////////////

/**
 * The document's snapshot bytes.
 *
 * @param {CollectionDocument} doc
 * @returns {Uint8Array}
 */
export function encodeDocument(doc) {
  doc.commit();

  return doc.export({ mode: "snapshot" });
}

/**
 * Longest update sent as-is. Past this a snapshot is measured too, because a
 * Loro update is the raw operation log: after a bulk insert it can be tens of
 * megabytes while the columnar snapshot of the same document is a fraction of
 * that.
 */
const MAX_PLAIN_UPDATE_BYTES = 64 * 1024;

/**
 * The bytes to append to a log for everything added since `heads`.
 *
 * Falls back to a whole snapshot when the heads are unknown (a first push, a
 * wiped log, or heads written by the previous engine).
 *
 * @param {CollectionDocument} doc
 * @param {Heads | undefined} heads
 * @returns {Uint8Array}
 */
export function changesSince(doc, heads) {
  doc.commit();

  const version = versionVectorFromHeads(heads);
  if (!version) return encodeDocument(doc);

  let update;
  try {
    update = doc.export({ mode: "update", from: version });
  } catch {
    return encodeDocument(doc);
  }

  // Small patches are cheapest as updates. After a bulk change the operation
  // log dwarfs the snapshot, and a snapshot also stays replayable without it.
  if (update.length <= MAX_PLAIN_UPDATE_BYTES) return update;

  const snapshot = encodeDocument(doc);

  return update.length <= snapshot.length ? update : snapshot;
}

/**
 * A stable fingerprint of the document's version, persisted in push cursors and
 * used to tell "already applied" from "something new".
 *
 * Loro's version vector is small and canonical, so its encoding is the
 * fingerprint.
 *
 * @param {CollectionDocument} doc
 * @returns {string[]}
 */
export function headsOf(doc) {
  doc.commit();

  const version = doc.oplogVersion();
  const json = version.toJSON();
  /** @type {Array<[`${number}`, number]>} */
  const entries = /** @type {any} */ (
    json instanceof Map ? [...json] : Object.entries(json ?? {})
  );

  // Loro's encoding preserves the order peers were inserted into the version
  // vector, so the same version can encode differently depending on the order
  // the document learned about its peers. Cursors and applied records compare
  // these strings, so encode in a canonical (peer-sorted) order.
  entries.sort(([left], [right]) => {
    const a = BigInt(left);
    const b = BigInt(right);

    return a < b ? -1 : a > b ? 1 : 0;
  });

  return [hexOf(new VersionVector(new Map(entries)).encode())];
}

/**
 * @param {Heads | undefined} heads
 * @returns {VersionVector | undefined}
 */
function versionVectorFromHeads(heads) {
  if (!Array.isArray(heads) || heads.length !== 1) return undefined;

  const value = heads[0];
  if (typeof value !== "string") return undefined;

  const bytes = bytesOfHex(value);
  if (!bytes) return undefined;

  try {
    return VersionVector.decode(bytes);
  } catch {
    return undefined;
  }
}

/** @param {Uint8Array} bytes */
function hexOf(bytes) {
  let out = "";

  for (const byte of bytes) out += byte.toString(16).padStart(2, "0");

  return out;
}

/**
 * @param {unknown} value
 * @returns {Uint8Array | undefined}
 */
function bytesOfHex(value) {
  if (typeof value !== "string" || value.length === 0 || value.length % 2) {
    return undefined;
  }
  if (!/^[0-9a-f]+$/i.test(value)) return undefined;

  const bytes = new Uint8Array(value.length / 2);

  for (let index = 0; index < bytes.length; index++) {
    bytes[index] = parseInt(value.slice(index * 2, index * 2 + 2), 16);
  }

  return bytes;
}

/**
 * Import a snapshot or an update into a document.
 *
 * @param {CollectionDocument} doc
 * @param {Uint8Array} bytes
 * @returns {CollectionDocument}
 */
export function applyIncremental(doc, bytes) {
  doc.import(bytes);
  doc.commit();

  return doc;
}

/**
 * Apply one log entry, which may be a snapshot or an update.
 *
 * @param {CollectionDocument} doc
 * @param {Uint8Array} bytes
 * @returns {CollectionDocument}
 */
export function applyPack(doc, bytes) {
  return applyIncremental(doc, bytes);
}

////////////////////////////////////////////
// MERGING
////////////////////////////////////////////

/**
 * Merge `b` into `a` and return `a`.
 *
 * Loro documents share one root, so merging is a matter of importing the
 * operations `a` is missing.
 *
 * @param {CollectionDocument} a
 * @param {CollectionDocument} b
 * @returns {CollectionDocument}
 */
export function mergeDocuments(a, b) {
  b.commit();

  const update = b.export({ mode: "update", from: a.oplogVersion() });

  if (update.length > 0) {
    a.import(update);
    a.commit();
  }

  return a;
}

/**
 * Merge a remote document's snapshot bytes into `doc`.
 *
 * @param {CollectionDocument} doc
 * @param {Uint8Array} bytes
 * @returns {CollectionDocument}
 */
export function mergeDocument(doc, bytes) {
  const remote = loadDocument(bytes);
  if (!remote) throw new Error("Not a Loro document");

  return mergeDocuments(doc, remote);
}

/**
 * Load a shared snapshot under this device's peer id.
 *
 * Unlike the previous engine there is no re-rooting to do: imported operations
 * keep the peer id that produced them, and only our future operations use ours.
 *
 * @param {Uint8Array} bytes
 * @param {string} [actor]
 * @returns {CollectionDocument}
 */
export function adoptDocument(bytes, actor) {
  const doc = loadDocument(bytes);
  if (!doc) throw new Error("Not a Loro document");

  if (actor) doc.setPeerId(peerIdOf(actor));

  return doc;
}

/**
 * Copy a document so two branches can be edited independently.
 *
 * @param {CollectionDocument} doc
 * @param {string} [actor]
 * @returns {CollectionDocument}
 */
export function cloneDocument(doc, actor) {
  doc.commit();

  const clone = doc.fork();
  if (actor) clone.setPeerId(peerIdOf(actor));

  return clone;
}

////////////////////////////////////////////
// LOG REPLAY
////////////////////////////////////////////

const LENGTH_PREFIX = 4;

/**
 * Split a decrypted, framed log into its entries.
 *
 * The decrypt layer length-prefixes each entry (u32be) — the framing this
 * splits on. A header that can't fit means the value predates the framing and
 * is kept whole, best effort.
 *
 * @param {Uint8Array} bytes
 * @returns {Uint8Array[]}
 */
export function splitLogEntries(bytes) {
  /** @type {Uint8Array[]} */
  const entries = [];
  let offset = 0;

  while (offset + LENGTH_PREFIX <= bytes.length) {
    const length = new DataView(
      bytes.buffer,
      bytes.byteOffset + offset,
      LENGTH_PREFIX,
    ).getUint32(0);

    const start = offset + LENGTH_PREFIX;
    const end = start + length;

    if (end > bytes.length) {
      if (offset === 0) entries.push(bytes);

      break;
    }

    entries.push(bytes.subarray(start, end));
    offset = end;
  }

  return entries;
}

/**
 * Replay log entries into a document.
 *
 * Loro applies snapshots and updates alike, and parks an update whose causal
 * dependencies haven't arrived yet, so entries are simply imported in order.
 * An entry this engine cannot read is skipped: a log may still hold entries
 * written by a different document format.
 *
 * @param {CollectionDocument} doc
 * @param {Uint8Array} bytes
 * @param {string} [actor]
 * @returns {CollectionDocument}
 */
export function applyLog(doc, bytes, actor) {
  if (actor) doc.setPeerId(peerIdOf(actor));

  let skipped = 0;

  for (const entry of splitLogEntries(bytes)) {
    try {
      doc.import(entry);
    } catch {
      skipped++;
    }
  }

  doc.commit();

  if (skipped > 0) {
    console.warn(`Skipped ${skipped} log entry(ies) this document cannot read`);
  }

  return doc;
}
