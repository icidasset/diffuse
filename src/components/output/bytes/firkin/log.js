import * as DagCBOR from "@ipld/dag-cbor";
import { ApiError } from "@tokono.ma/firkin";

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
 * @import {LogEntry, LogManifest} from "@specs/components/output/bytes/firkin/types.d.ts"
 */

/**
 * The transport behind {@link FirkinOutput}: a sharded, append-only log.
 *
 * Writers never touch each other's entries. Each one keeps a manifest under its
 * own registry name and only ever rewrites that manifest, so two devices
 * writing at the same time can't lose each other's appends even though the
 * registry has no compare-and-swap. Readers union the manifests.
 *
 * Entries are stamped with the wall clock at append time. That is what makes
 * "the newest entry" meaningful across writers; it only has to be roughly
 * right, because a consumer that needs convergence merges every entry instead
 * of picking one.
 *
 * These helpers take a `Firkin` client so they can be tested without a session
 * or a browser.
 */

/**
 * Resolve every log entry for `name`, across all writers, oldest first.
 *
 * @param {Firkin} rb
 * @param {string} namespace
 * @param {string} name
 * @returns {Promise<LogEntry[]>}
 */
export async function readEntries(rb, namespace, name) {
  const prefix = `${encodeFileName(name)}.`;
  const names = await rb.registryNames(registryNamespace(namespace));

  /** @type {LogEntry[]} */
  const entries = [];
  const seen = new Set();

  for (
    const [key, manifestCid] of Object.entries(names ?? {}).sort((a, b) =>
      a[0].localeCompare(b[0])
    )
  ) {
    if (!key.startsWith(prefix)) continue;

    const manifest = await readManifestByCid(rb, manifestCid);
    for (const entry of manifest?.packs ?? []) {
      if (seen.has(entry.cid)) continue;

      seen.add(entry.cid);
      entries.push(entry);
    }
  }

  return entries.sort((a, b) => a.at - b.at);
}

/**
 * The most recently appended entry, or `undefined` when the log is empty.
 *
 * @param {Firkin} rb
 * @param {string} namespace
 * @param {string} name
 * @returns {Promise<LogEntry | undefined>}
 */
export async function latestEntry(rb, namespace, name) {
  const entries = await readEntries(rb, namespace, name);

  return entries.at(-1);
}

/**
 * Append one entry to a writer's log.
 *
 * Re-appending identical bytes is a no-op: the same bytes hash to the same CID,
 * and the CID is already in the log.
 *
 * @param {Firkin} rb
 * @param {string} namespace
 * @param {string} name
 * @param {string} device
 * @param {Uint8Array} bytes
 * @param {{ keep?: number }} [options] - `keep` trims the log to its newest
 *   entries, for consumers that only ever read the latest value.
 * @returns {Promise<{ cid: string; appended: boolean }>}
 */
export async function appendEntry(rb, namespace, name, device, bytes, options = {}) {
  const { cid } = await rb.upload(bytes, { codec: CODEC_RAW });

  const manifest = await readManifest(rb, namespace, name, device);
  if (manifest?.packs.some((entry) => entry.cid === cid)) {
    return { cid, appended: false };
  }

  /** @type {LogEntry} */
  const entry = { cid, at: Date.now() };

  const packs = [...manifest?.packs ?? [], entry];
  const keep = options.keep;
  const trimmed = keep && keep > 0 && packs.length > keep
    ? packs.slice(-keep)
    : packs;

  await writeManifest(rb, namespace, name, device, trimmed);

  return { cid, appended: true };
}

/**
 * How many entries `device` has appended for `name`.
 *
 * @param {Firkin} rb
 * @param {string} namespace
 * @param {string} name
 * @param {string} device
 * @returns {Promise<number>}
 */
export async function countEntries(rb, namespace, name, device) {
  const manifest = await readManifest(rb, namespace, name, device);

  return manifest?.packs.length ?? 0;
}

/**
 * Replace `device`'s log with a single entry.
 *
 * Other writers' manifests are untouched, so their entries stay reachable.
 *
 * @param {Firkin} rb
 * @param {string} namespace
 * @param {string} name
 * @param {string} device
 * @param {Uint8Array} snapshot
 * @returns {Promise<string>} CID of the snapshot entry.
 */
export async function replaceLog(rb, namespace, name, device, snapshot) {
  const { cid } = await rb.upload(snapshot, { codec: CODEC_RAW });
  await writeManifest(rb, namespace, name, device, [{ cid, at: Date.now() }]);

  return cid;
}

/**
 * @param {Firkin} rb
 * @param {string} namespace
 * @param {string} name
 * @param {string} device
 * @returns {Promise<LogManifest | undefined>}
 */
async function readManifest(rb, namespace, name, device) {
  const cid = await rb.registryGet(
    registryNamespace(namespace),
    logName(name, device),
  );
  if (!cid) return undefined;

  return await readManifestByCid(rb, cid);
}

/**
 * @param {Firkin} rb
 * @param {string} cid
 * @returns {Promise<LogManifest | undefined>}
 */
async function readManifestByCid(rb, cid) {
  try {
    const bytes = await rb.get(cid);
    return normalizeManifest(/** @type {LogManifest} */ (DagCBOR.decode(bytes)));
  } catch (err) {
    // A manifest whose blob is gone behaves like an empty log; the reader
    // decides whether the remaining entries are enough.
    if (err instanceof ApiError && err.status === 404) return undefined;
    throw err;
  }
}

/**
 * Accept the older `packs: string[]` shape so a manifest written by an earlier
 * build doesn't read as garbage. Those entries sort before every stamped one.
 *
 * @param {LogManifest | { v?: number; packs?: unknown }} manifest
 * @returns {LogManifest}
 */
function normalizeManifest(manifest) {
  const packs = Array.isArray(manifest.packs) ? manifest.packs : [];

  return {
    v: MANIFEST_VERSION,
    packs: packs.map((/** @type {any} */ entry) =>
      typeof entry === "string" ? { cid: entry, at: 0 } : entry
    ),
  };
}

/**
 * @param {Firkin} rb
 * @param {string} namespace
 * @param {string} name
 * @param {string} device
 * @param {LogEntry[]} packs
 */
async function writeManifest(rb, namespace, name, device, packs) {
  /** @type {LogManifest} */
  const manifest = { v: MANIFEST_VERSION, packs };
  const { cid } = await rb.upload(DagCBOR.encode(manifest), {
    codec: CODEC_DAG_CBOR,
  });

  await rb.registrySet(
    registryNamespace(namespace),
    logName(name, device),
    cid,
  );
}
