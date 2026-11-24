import * as DagCBOR from "@ipld/dag-cbor";

/**
 * The Record Sync state and patch codec.
 *
 * A collection is a map of record id → the record and the version that last
 * wrote it, plus tombstones and one LWW order. Patches carry a single version
 * for the batch; snapshots carry per-entry versions so they merge by LWW like
 * any other patch.
 *
 * Versions are `(at, device)`: an RFC 3339 millisecond timestamp and the
 * writer's public key. Comparing them is lexicographic, so clock skew only
 * decides the winner between writes that actually raced.
 *
 * @typedef {{ at: string; device: string }} Version
 * @typedef {{ record: any; at: string; device: string }} VersionedRecord
 * @typedef {{ value: string[]; at: string; device: string }} VersionedOrder
 *
 * @typedef {Object} State
 * @property {1} v
 * @property {Record<string, VersionedRecord>} records
 * @property {Record<string, Version>} tombstones
 * @property {VersionedOrder} order
 *
 * @typedef {Object} DeltaPatch
 * @property {1} v
 * @property {string} at
 * @property {string} device
 * @property {Record<string, any>} [put]
 * @property {string[]} [del]
 * @property {string[]} [order]
 *
 * @typedef {Object} SnapshotPatch
 * @property {1} v
 * @property {string} at
 * @property {string} device
 * @property {{ records: Record<string, VersionedRecord>; tombstones: Record<string, Version>; order: VersionedOrder }} snapshot
 *
 * @typedef {DeltaPatch | SnapshotPatch} Patch
 */

/** @returns {State} */
export function emptyState() {
  return {
    v: 1,
    records: {},
    tombstones: {},
    order: { value: [], at: "", device: "" },
  };
}

/**
 * @param {Version} a
 * @param {Version} b
 */
export function compareVersions(a, b) {
  if (a.at !== b.at) return a.at < b.at ? -1 : 1;
  if (a.device !== b.device) return a.device < b.device ? -1 : 1;

  return 0;
}

/** @param {any} value */
const isObject = (value) =>
  typeof value === "object" && value !== null && !Array.isArray(value);

/** @param {State} state */
export function hasState(state) {
  return Object.keys(state.records).length > 0 ||
    Object.keys(state.tombstones).length > 0 ||
    state.order.value.length > 0;
}

/** @param {State} state @returns {any[]} */
export function stateItems(state) {
  const ids = state.order.value.filter((id) => state.records[id]);
  const seen = new Set(ids);

  for (const id of Object.keys(state.records)) {
    if (!seen.has(id)) ids.push(id);
  }

  return ids.map((id) => state.records[id].record);
}

/** @param {State} state */
export function encodeState(state) {
  return new Uint8Array(DagCBOR.encode(state));
}

/**
 * @param {Uint8Array} bytes
 * @returns {State}
 */
export function decodeState(bytes) {
  const value = /** @type {any} */ (DagCBOR.decode(bytes));

  if (!isObject(value) || value.v !== 1) {
    throw new TypeError("Not a Record Sync state");
  }

  const state = emptyState();

  if (isObject(value.records)) state.records = value.records;
  if (isObject(value.tombstones)) state.tombstones = value.tombstones;
  if (isObject(value.order) && Array.isArray(value.order.value)) {
    state.order = {
      value: value.order.value.map(String),
      at: typeof value.order.at === "string" ? value.order.at : "",
      device: typeof value.order.device === "string" ? value.order.device : "",
    };
  }

  return state;
}

/** @param {Patch} patch */
export function encodePatch(patch) {
  return new Uint8Array(DagCBOR.encode(patch));
}

/**
 * @param {Uint8Array} bytes
 * @returns {Patch}
 */
export function decodePatch(bytes) {
  const value = /** @type {any} */ (DagCBOR.decode(bytes));

  if (!isObject(value) || value.v !== 1) {
    throw new TypeError("Not a Record Sync patch");
  }
  if (typeof value.at !== "string" || typeof value.device !== "string") {
    throw new TypeError("Record Sync patch has no version");
  }

  return value;
}

/**
 * @param {State} state
 * @param {string} id
 * @param {Version} version
 */
function isNewer(state, id, version) {
  const current = state.records[id] ?? state.tombstones[id];

  return !current || compareVersions(version, current) > 0;
}

/**
 * Apply one patch. LWW per record, tombstone and order; idempotent and
 * independent of the order patches arrive in.
 *
 * @param {State} state
 * @param {Patch} patch
 * @returns {boolean} Whether anything changed.
 */
export function applyPatch(state, patch) {
  /** @type {any} */
  const value = patch;

  if (isObject(value.snapshot)) {
    return applySnapshot(state, {
      at: patch.at,
      device: patch.device,
      ...value.snapshot,
    });
  }

  const version = { at: patch.at, device: patch.device };
  let changed = false;

  for (const [id, record] of Object.entries(value.put ?? {})) {
    if (!isNewer(state, id, version)) continue;

    state.records[id] = { record, ...version };
    delete state.tombstones[id];
    changed = true;
  }

  for (const id of value.del ?? []) {
    if (!isNewer(state, id, version)) continue;

    delete state.records[id];
    state.tombstones[id] = { ...version };
    changed = true;
  }

  if (Array.isArray(value.order) && compareVersions(version, state.order) > 0) {
    state.order = { value: value.order.map(String), ...version };
    changed = true;
  }

  return changed;
}

/**
 * Merge a snapshot as a set of LWW entries, not as a replacement: a local
 * record newer than the snapshot's copy survives.
 *
 * @param {State} state
 * @param {{ at: string; device: string; records?: Record<string, VersionedRecord>; tombstones?: Record<string, Version>; order?: VersionedOrder }} snapshot
 * @returns {boolean}
 */
function applySnapshot(state, snapshot) {
  let changed = false;

  for (const [id, entry] of Object.entries(snapshot.records ?? {})) {
    if (!isObject(entry) || !isObject(entry.record)) continue;
    if (!isNewer(state, id, entry)) continue;

    state.records[id] = {
      record: entry.record,
      at: entry.at,
      device: entry.device,
    };
    delete state.tombstones[id];
    changed = true;
  }

  for (const [id, entry] of Object.entries(snapshot.tombstones ?? {})) {
    if (!isObject(entry)) continue;
    if (!isNewer(state, id, entry)) continue;

    delete state.records[id];
    state.tombstones[id] = { at: entry.at, device: entry.device };
    changed = true;
  }

  if (snapshot.order && compareVersions(snapshot.order, state.order) > 0) {
    state.order = {
      value: snapshot.order.value.map(String),
      at: snapshot.order.at,
      device: snapshot.order.device,
    };
    changed = true;
  }

  return changed;
}

/** @param {any} a @param {any} b */
function sameRecord(a, b) {
  try {
    const left = DagCBOR.encode(a);
    const right = DagCBOR.encode(b);
    if (left.length !== right.length) return false;

    for (let index = 0; index < left.length; index++) {
      if (left[index] !== right[index]) return false;
    }

    return true;
  } catch {
    return false;
  }
}

/** @param {string[]} a @param {string[]} b */
function sameOrder(a, b) {
  return a.length === b.length && a.every((id, index) => id === b[index]);
}

/**
 * Diff a new sealed collection against the current state.
 *
 * @param {State} state
 * @param {Record<string, any>} sealed - sealed records by id
 * @param {string[]} order
 * @param {Version} version
 * @returns {DeltaPatch | undefined}
 */
export function deltaPatch(state, sealed, order, version) {
  /** @type {Record<string, any>} */
  const put = {};

  for (const [id, record] of Object.entries(sealed)) {
    if (!sameRecord(state.records[id]?.record, record)) put[id] = record;
  }

  const del = Object.keys(state.records).filter((id) => !(id in sealed));
  const orderChanged = !sameOrder(state.order.value, order);

  if (Object.keys(put).length === 0 && del.length === 0 && !orderChanged) {
    return undefined;
  }

  /** @type {DeltaPatch} */
  const patch = { v: 1, at: version.at, device: version.device };
  if (Object.keys(put).length > 0) patch.put = put;
  if (del.length > 0) patch.del = del;
  if (orderChanged) patch.order = order;

  return patch;
}

/**
 * The full-state entry written every `SNAPSHOT_EVERY` pushes so the log stays
 * replayable after old entries are trimmed.
 *
 * @param {State} state
 * @param {Version} version
 * @returns {SnapshotPatch}
 */
export function snapshotPatch(state, version) {
  return {
    v: 1,
    at: version.at,
    device: version.device,
    snapshot: {
      records: state.records,
      tombstones: state.tombstones,
      order: state.order,
    },
  };
}
