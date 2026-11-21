import { ApiError, cidForBytes } from "@tokono.ma/firkin";

import * as DagCBOR from "@ipld/dag-cbor";

import { encodeFileName } from "~/common/firkin/files.js";

import {
  CODEC_DAG_CBOR,
  CODEC_RAW,
  MANIFEST_CAS_RETRIES,
  INLINE_ENTRY_MAX_BYTES,
  MANIFEST_VERSION,
  logName,
  registryNamespace,
} from "./constants.js";

/**
 * @import {Firkin} from "@tokono.ma/firkin"
 * @import {LogEntry, LogManifestPayload} from "@specs/components/output/raw/firkin/types.d.ts"
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
 * @property {(device: string, manifestCid: string, payload: LogManifestPayload) => void} [onManifest]
 *   - Receives every manifest the reader accepted, so a writer can reuse it
 *   instead of re-reading its own ref on the next append.
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
    options.onManifest?.(device, manifestCid, payload);

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
 * A writer's manifest, as the next append needs it: the registry value (for the
 * compare-and-swap) and its verified payload (for `seq` and the retained
 * entries).
 *
 * @typedef {Object} ManifestState
 * @property {string} cid
 * @property {LogManifestPayload} payload
 */

/**
 * Append one entry to the caller's log.
 *
 * Re-appending identical bytes is a no-op: the same bytes hash to the same CID,
 * and the CID is already in the log.
 *
 * Small entries ride inside the signed manifest instead of a blob of their own,
 * which removes both the upload and the fetch for the common case (a settings
 * edit, a bundle update). Larger entries stay separate blocks.
 *
 * A caller that already read (or wrote) its manifest passes it as `current`,
 * so the append costs one upload and one compare-and-swap. Without it, the
 * append reads the ref once; either way the registry write is an `If-Match`
 * against the CID the append was built on, so two tabs of the same device
 * cannot overwrite each other. A lost race re-reads and rebuilds.
 *
 * @param {Firkin} rb
 * @param {string} namespace
 * @param {string} name
 * @param {Uint8Array} bytes
 * @param {{ auth: LogAuth; keep?: number; floors?: Map<string, number>; current?: ManifestState }} options
 *   - `keep` trims the log to its newest entries, for consumers that only ever
 *   read the latest value.
 * @returns {Promise<{ cid: string; appended: boolean; manifest: ManifestState }>}
 */
export async function appendEntry(rb, namespace, name, bytes, options) {
  const { auth } = options;
  const inline = bytes.length <= INLINE_ENTRY_MAX_BYTES;

  // A large entry stays its own content-addressed block; a small one rides in
  // the manifest and needs no upload at all.
  const cid = inline
    ? cidForBytes(bytes, { codec: CODEC_RAW })
    : (await rb.upload(bytes, { codec: CODEC_RAW })).cid;
  const ref = logName(name, auth.device);
  const registry = registryNamespace(namespace);

  /** @type {ManifestState | undefined} */
  let current = options.current;

  for (let attempt = 0; attempt < MANIFEST_CAS_RETRIES; attempt++) {
    /** @type {string | null} */
    let when;

    if (current) {
      when = current.cid;
    } else {
      // `registryLookup` returns only names that are set, so a device writing
      // for the first time costs one request and no 404.
      const found = await rb.registryLookup(registry, [ref]);
      when = found[ref] ?? null;

      if (when) {
        const payload = await readManifest(rb, name, auth.device, when, {
          auth,
          floors: options.floors,
        });

        // A manifest we can't verify (forged, or from an older format) is
        // treated as empty: the CAS still replaces it on this device's ref.
        if (payload) current = { cid: when, payload };
      }
    }

    if (current && current.payload.packs.some((entry) => entry.cid === cid)) {
      return { cid, appended: false, manifest: current };
    }

    /** @type {LogEntry} */
    const entry = inline
      ? { cid, at: Date.now(), bytes }
      : { cid, at: Date.now() };

    const packs = [...current?.payload.packs ?? [], entry];
    const keep = options.keep;
    const trimmed = keep && keep > 0 && packs.length > keep
      ? packs.slice(-keep)
      : packs;

    /** @type {LogManifestPayload} */
    const payload = {
      v: MANIFEST_VERSION,
      collection: encodeFileName(name),
      device: auth.device,
      seq: (current?.payload.seq ?? 0) + 1,
      packs: trimmed,
    };

    const written = await writeManifest(
      rb,
      namespace,
      name,
      auth,
      payload,
      when,
    );

    if (written.ok) {
      return { cid, appended: true, manifest: { cid: written.cid, payload } };
    }

    // Another writer (usually another tab of this device) landed an append
    // between our read and the CAS. Re-read and rebuild on top of it.
    current = undefined;
  }

  throw new Error(
    `Could not append to the '${name}' log: its manifest kept changing`,
  );
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
 * @param {LogManifestPayload} payload
 * @param {string | null} when - CID the name must still hold (`null`: unset).
 * @returns {Promise<{ ok: true; cid: string } | { ok: false; current: string | null }>}
 */
async function writeManifest(rb, namespace, name, auth, payload, when) {
  const signed = DagCBOR.encode(payload);
  const signature = await auth.sign(signed);

  const { cid } = await rb.upload(DagCBOR.encode({ p: signed, s: signature }), {
    codec: CODEC_DAG_CBOR,
  });

  return await rb.registrySetIf(
    registryNamespace(namespace),
    logName(name, auth.device),
    cid,
    when,
  );
}
