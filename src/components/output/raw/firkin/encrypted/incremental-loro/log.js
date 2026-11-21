import { ApiError, carFile, cidForBytes } from "@tokono.ma/firkin";

import * as DagCBOR from "@ipld/dag-cbor";
import { CID } from "multiformats/cid";

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
 * The manifest is a small DAG: the registry points at an envelope block that
 * links the signed payload, which links each entry stored as its own block.
 * That is what lets a reader fetch a whole log — or just the part it is missing
 * — in one request, and a writer upload the manifest and its new entry in one
 * CAR instead of one request per block.
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
    const manifest = await readManifest(rb, name, device, manifestCid, options);
    if (!manifest) continue;

    const { payload, blocks } = manifest;

    const floor = options.floors?.get(device) ?? 0;
    if (payload.seq < floor) continue;

    options.onAccept?.(device, payload.seq);
    options.onManifest?.(device, manifestCid, payload);

    for (const entry of payload.packs) {
      if (seen.has(entry.cid)) continue;

      seen.add(entry.cid);

      // A fetched entry block is already the bytes the caller would have read
      // one by one; hand them over instead of asking for them again.
      const bytes = entry.bytes ?? blocks.get(entry.cid);

      entries.push(bytes ? { ...entry, bytes } : entry);
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
 * Small entries ride inside the signed payload instead of a block of their own,
 * which removes the upload and the fetch for the common case (a settings edit, a
 * bundle update). A large entry is a block linked from the payload; the append
 * uploads it in the same CAR as the manifest.
 *
 * A caller that already read (or wrote) its manifest passes it as `current`,
 * so the append costs one CAR ingest and one compare-and-swap. Without it, the
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
  const cid = cidForBytes(bytes, { codec: CODEC_RAW });
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
        const manifest = await readManifest(rb, name, auth.device, when, {
          auth,
          floors: options.floors,
        });

        // A manifest we can't verify (forged, or from an older format) is
        // treated as empty: the CAS still replaces it on this device's ref.
        if (manifest) current = { cid: when, payload: manifest.payload };
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
      inline ? undefined : { cid, bytes },
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
 * All blocks below a manifest root, when the root itself still exists.
 *
 * `dagPull` returns the manifest and every entry block in one request, and only
 * the blocks the client's cache lacks. A server without it (or a graph with a
 * missing entry) falls back to the two header blocks, and the caller fetches
 * entries individually.
 *
 * @param {Firkin} rb
 * @param {string} cid
 * @returns {Promise<Map<string, Uint8Array> | undefined>}
 */
async function pullBlocks(rb, cid) {
  if (typeof rb.dagPull === "function") {
    try {
      return await rb.dagPull(cid);
    } catch {
      // Incomplete graph: fall through to the header blocks.
    }
  }

  try {
    const blocks = new Map();
    const envelopeBytes = await rb.get(cid);
    blocks.set(cid, envelopeBytes);

    let envelope;
    try {
      envelope = DagCBOR.decode(envelopeBytes);
    } catch {
      return blocks;
    }

    const payloadCid = cidOf(envelope?.p);
    if (payloadCid) blocks.set(payloadCid, await rb.get(payloadCid));

    return blocks;
  } catch (err) {
    // A manifest whose blob is gone behaves like an empty log; the reader
    // decides whether the remaining entries are enough.
    if (err instanceof ApiError && err.status === 404) return undefined;
    throw err;
  }
}

/** @param {unknown} value */
function cidOf(value) {
  if (typeof value === "string") return value;
  if (value && typeof (/** @type {any} */ (value).toString) === "function") {
    return /** @type {any} */ (value).toString();
  }
  return undefined;
}

/**
 * Read and verify one manifest.
 *
 * @param {Firkin} rb
 * @param {string} name
 * @param {string} device - Taken from the ref name.
 * @param {string | null} cid
 * @param {{ auth?: LogAuth; floors?: Map<string, number> }} options
 * @returns {Promise<{ payload: LogManifestPayload; blocks: Map<string, Uint8Array> } | undefined>}
 */
async function readManifest(rb, name, device, cid, options) {
  if (!cid) return undefined;

  const blocks = await pullBlocks(rb, cid);
  if (!blocks) return undefined;

  const envelopeBytes = blocks.get(cid);
  if (!(envelopeBytes instanceof Uint8Array)) return undefined;

  /** @type {any} */
  let envelope;
  try {
    envelope = DagCBOR.decode(envelopeBytes);
  } catch {
    return undefined;
  }

  if (envelope?.v !== MANIFEST_VERSION) return undefined;

  const payloadCid = cidOf(envelope?.p);
  const signed = payloadCid ? blocks.get(payloadCid) : undefined;
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

  if (!valid) return undefined;

  // Entry CIDs are links in the stored payload; consumers (and the manifest
  // cache) deal in strings.
  payload.packs = payload.packs.map((/** @type {any} */ entry) => ({
    ...entry,
    cid: cidOf(entry.cid) ?? "",
  }));

  return { payload, blocks };
}

/**
 * Write and sign a manifest, uploading it and `entry` in one CAR.
 *
 * @param {Firkin} rb
 * @param {string} namespace
 * @param {string} name
 * @param {LogAuth} auth
 * @param {LogManifestPayload} payload
 * @param {string | null} when - CID the name must still hold (`null`: unset).
 * @param {{ cid: string; bytes: Uint8Array }} [entry] - A large entry to store
 *   alongside the manifest.
 * @returns {Promise<{ ok: true; cid: string } | { ok: false; current: string | null }>}
 */
async function writeManifest(rb, namespace, name, auth, payload, when, entry) {
  // Block entries are links in the DAG; inline entries stay strings.
  const stored = {
    ...payload,
    packs: payload.packs.map((/** @type {any} */ pack) =>
      pack.bytes ? pack : { ...pack, cid: CID.parse(pack.cid) }
    ),
  };

  const payloadBytes = DagCBOR.encode(stored);
  const payloadCid = cidForBytes(payloadBytes, { codec: CODEC_DAG_CBOR });
  const signature = await auth.sign(payloadBytes);

  const envelope = { v: MANIFEST_VERSION, p: CID.parse(payloadCid), s: signature };
  const envelopeBytes = DagCBOR.encode(envelope);
  const envelopeCid = cidForBytes(envelopeBytes, { codec: CODEC_DAG_CBOR });

  /** @type {Array<[string, Uint8Array, number]>} */
  const blocks = [
    [payloadCid, payloadBytes, CODEC_DAG_CBOR],
    [envelopeCid, envelopeBytes, CODEC_DAG_CBOR],
  ];

  if (entry) blocks.push([entry.cid, entry.bytes, CODEC_RAW]);

  await uploadBlocks(rb, blocks);

  return await rb.registrySetIf(
    registryNamespace(namespace),
    logName(name, auth.device),
    envelopeCid,
    when,
  );
}

/**
 * Store several blocks in one request when the transport supports CAR ingest.
 *
 * @param {Firkin} rb
 * @param {Array<[string, Uint8Array, number]>} blocks
 */
async function uploadBlocks(rb, blocks) {
  if (typeof rb.ingestCar === "function") {
    const car = carFile(
      null,
      blocks.map(([cid, bytes]) => [CID.parse(cid), bytes]),
    );

    try {
      const result = await rb.ingestCar(car);
      const failed = Number(
        result?.failed_total ?? (Array.isArray(result?.failed)
          ? result.failed.length
          : 0),
      );

      if (failed > 0) {
        throw new Error(`Firkin rejected ${failed} log block(s)`);
      }

      return;
    } catch (err) {
      // An older server without the CAR endpoint: fall back to one upload per
      // block. Anything else (a rejected block, a network failure) is real.
      if (
        !(err instanceof ApiError) || (err.status !== 404 && err.status !== 405)
      ) {
        throw err;
      }
    }
  }

  for (const [, bytes, codec] of blocks) {
    await rb.upload(bytes, { codec });
  }
}
