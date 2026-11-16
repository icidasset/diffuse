import { ApiError } from "@tokono.ma/firkin";

import * as DagCBOR from "@ipld/dag-cbor";

import { encodeFileName } from "~/common/firkin/files.js";

import {
  CODEC_DAG_CBOR,
  CODEC_RAW,
  MANIFEST_VERSION,
  logName,
  registryNamespace,
} from "./constants.js";

/**
 * @import {Firkin} from "@tokono.ma/firkin"
 * @import {LogEntry, LogManifestPayload} from "@specs/components/output/bytes/firkin/types.d.ts"
 */

/**
 * The transport behind {@link FirkinOutput}: a sharded, append-only log.
 *
 * Writers never touch each other's entries. Each one keeps a *signed* manifest
 * under its own registry name (derived from its public key) and only ever
 * rewrites that manifest, so two devices writing at the same time can't lose
 * each other's appends even though the registry has no compare-and-swap.
 * Readers union the manifests.
 *
 * Signing matters because anyone holding the account's `/firkin` capability can
 * write any registry name. A manifest binds the device, the collection and a
 * monotonic sequence number, so a reader can tell a device's own manifest from
 * one forged or replayed under its ref.
 *
 * These helpers take a `Firkin` client plus a `LogAuth` so they can be tested
 * without a session or a browser.
 */

/**
 * @typedef {Object} LogAuth
 * @property {string} device - Hex-encoded public key; also the ref suffix.
 * @property {(bytes: Uint8Array) => Promise<Uint8Array>} sign
 * @property {(device: string, signature: Uint8Array, bytes: Uint8Array) => Promise<boolean>} verify
 */

/**
 * @typedef {Object} ReadOptions
 * @property {LogAuth} auth
 * @property {Map<string, number>} [floors] - Newest sequence already accepted
 *   per device; a lower one is a rollback and is ignored.
 * @property {(device: string, seq: number) => void} [onAccept] - Reports the
 *   sequence of every accepted manifest so the caller can raise its floors.
 * @property {(namespace: string) => Promise<Record<string, string> | null>} [loadNames]
 *   - Where the namespace listing comes from. Defaults to the client, and
 *   exists so a caller can serve several reads from one request.
 */

/**
 * Resolve every log entry for `name`, across all writers, oldest first.
 *
 * Manifests that fail verification, don't belong to their ref, or roll a
 * device's sequence back are ignored; the rest of the log still reads.
 *
 * @param {Firkin} rb
 * @param {string} namespace
 * @param {string} name
 * @param {ReadOptions} options
 * @returns {Promise<LogEntry[]>}
 */
export async function readEntries(rb, namespace, name, options) {
  const prefix = `${encodeFileName(name)}.`;
  const loadNames = options.loadNames ??
    ((/** @type {string} */ ns) => rb.registryNames(ns));
  const names = await loadNames(registryNamespace(namespace));

  /** @type {LogEntry[]} */
  const entries = [];
  const seen = new Set();

  for (
    const [key, manifestCid] of Object.entries(names ?? {}).sort((a, b) =>
      a[0].localeCompare(b[0])
    )
  ) {
    if (!key.startsWith(prefix)) continue;

    const device = key.slice(prefix.length);
    const payload = await readManifest(rb, name, device, manifestCid, options);
    if (!payload) continue;

    const floor = options.floors?.get(device) ?? 0;
    if (payload.seq < floor) continue;

    options.onAccept?.(device, payload.seq);

    for (const entry of payload.packs) {
      if (seen.has(entry.cid)) continue;

      seen.add(entry.cid);
      entries.push(entry);
    }
  }

  return entries.sort((a, b) => a.at - b.at);
}

/**
 * The most recently appended entry, or `undefined` when there is none.
 *
 * @param {Firkin} rb
 * @param {string} namespace
 * @param {string} name
 * @param {ReadOptions} options
 * @returns {Promise<LogEntry | undefined>}
 */
export async function latestEntry(rb, namespace, name, options) {
  const entries = await readEntries(rb, namespace, name, options);

  return entries.at(-1);
}

/**
 * Append one entry to the caller's log.
 *
 * Re-appending identical bytes is a no-op: the same bytes hash to the same CID,
 * and the CID is already in the log.
 *
 * @param {Firkin} rb
 * @param {string} namespace
 * @param {string} name
 * @param {Uint8Array} bytes
 * @param {{ auth: LogAuth; keep?: number; floors?: Map<string, number> }} options
 *   - `keep` trims the log to its newest entries, for consumers that only ever
 *   read the latest value.
 * @returns {Promise<{ cid: string; appended: boolean }>}
 */
export async function appendEntry(rb, namespace, name, bytes, options) {
  const { auth } = options;
  const { cid } = await rb.upload(bytes, { codec: CODEC_RAW });

  // `registryLookup` returns only names that are set, so a device writing for
  // the first time costs one request and no 404.
  const ref = logName(name, auth.device);
  const found = await rb.registryLookup(registryNamespace(namespace), [ref]);

  const current = await readManifest(rb, name, auth.device, found[ref], {
    auth,
    floors: options.floors,
  });

  if (current?.packs.some((entry) => entry.cid === cid)) {
    return { cid, appended: false };
  }

  /** @type {LogEntry} */
  const entry = { cid, at: Date.now() };

  const packs = [...current?.packs ?? [], entry];
  const keep = options.keep;
  const trimmed = keep && keep > 0 && packs.length > keep
    ? packs.slice(-keep)
    : packs;

  await writeManifest(rb, namespace, name, auth, trimmed, (current?.seq ?? 0) + 1);

  return { cid, appended: true };
}

/**
 * Read and verify one manifest.
 *
 * @param {Firkin} rb
 * @param {string} name
 * @param {string} device - Taken from the ref name.
 * @param {string | null} cid
 * @param {{ auth?: LogAuth; floors?: Map<string, number> }} options
 * @returns {Promise<LogManifestPayload | undefined>}
 */
async function readManifest(rb, name, device, cid, options) {
  if (!cid) return undefined;

  /** @type {any} */
  let envelope;
  try {
    envelope = DagCBOR.decode(await rb.get(cid));
  } catch (err) {
    // A manifest whose blob is gone behaves like an empty log; the reader
    // decides whether the remaining entries are enough.
    if (err instanceof ApiError && err.status === 404) return undefined;
    throw err;
  }

  const signed = envelope?.p;
  const signature = envelope?.s;
  if (!(signed instanceof Uint8Array) || !(signature instanceof Uint8Array)) {
    return undefined;
  }

  /** @type {LogManifestPayload} */
  let payload;
  try {
    payload = DagCBOR.decode(signed);
  } catch {
    return undefined;
  }

  // The manifest has to be about the log it is filed under, and signed by the
  // key the ref is named after.
  if (payload.device !== device) return undefined;
  if (payload.collection !== encodeFileName(name)) return undefined;
  if (payload.v !== MANIFEST_VERSION) return undefined;
  if (!Number.isInteger(payload.seq) || payload.seq < 1) return undefined;
  if (!Array.isArray(payload.packs)) return undefined;

  const auth = options.auth;
  if (!auth) return undefined;

  let valid = false;
  try {
    valid = await auth.verify(payload.device, signature, signed);
  } catch (err) {
    console.warn(`Could not verify a '${name}' manifest:`, err);
  }

  return valid ? payload : undefined;
}

/**
 * @param {Firkin} rb
 * @param {string} namespace
 * @param {string} name
 * @param {LogAuth} auth
 * @param {LogEntry[]} packs
 * @param {number} seq - The caller already read the current manifest, so the
 *   write doesn't need a second read.
 */
async function writeManifest(rb, namespace, name, auth, packs, seq) {

  /** @type {LogManifestPayload} */
  const payload = {
    v: MANIFEST_VERSION,
    collection: encodeFileName(name),
    device: auth.device,
    seq,
    packs,
  };

  const signed = DagCBOR.encode(payload);
  const signature = await auth.sign(signed);

  const { cid } = await rb.upload(DagCBOR.encode({ p: signed, s: signature }), {
    codec: CODEC_DAG_CBOR,
  });

  await rb.registrySet(
    registryNamespace(namespace),
    logName(name, auth.device),
    cid,
  );
}
