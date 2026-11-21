import { encodeFileName } from "~/common/firkin/files.js";

/**
 * Constants for the merged Firkin + encrypted + Automerge output.
 *
 * The log protocol (registry names, manifests, codecs), the encryption framing
 * and the Automerge collection model all live in this one component, so their
 * constants do too.
 */

/** Collections every output stores. */
export const COLLECTIONS = /** @type {import("~/common/self-describing.js").CollectionName[]} */ ([
  "collaborations",
  "facets",
  "playlistItems",
  "settings",
  "tracks",
]);

/**
 * Write a whole document instead of a delta this often.
 *
 * The log is replayed from scratch by new devices, so it keeps itself
 * self-sufficient by periodically appending a snapshot: any window that still
 * contains the last snapshot can be replayed without its trimmed ancestors.
 */
export const SNAPSHOT_EVERY = 32;

/**
 * How long to wait for the output to become usable before showing an empty
 * library.
 *
 * A collection stays `loading` until it has an answer, which is what keeps the
 * UI from flashing empty. But if the output can never answer — signed out, or
 * no encryption key on this browser — waiting forever just spins.
 */
export const NOT_READY_GRACE_MS = 750;

/**
 * How long the first read of a collection may take before the UI shows what it
 * has (usually nothing) instead of waiting.
 *
 * The collections are local-first, so this only matters when the local cache is
 * empty and the log has to be read over the network.
 */
export const FIRST_READ_TIMEOUT_MS = 1_500;

/**
 * Attempts at a compare-and-swap manifest write before giving up. A conflict
 * means another tab of this same device appended in between; re-reading and
 * rebuilding resolves it.
 */
export const MANIFEST_CAS_RETRIES = 5;

/**
 * How long a save waits before it is pushed. Saves are local-first, so a burst
 * (processing a library, a batch of edits) coalesces into one remote append
 * instead of one manifest rewrite per batch.
 */
export const PUSH_COALESCE_MS = 750;

/**
 * Collections at least this large are saved in yielding batches, because one
 * `Automerge.change` with tens of thousands of writes blocks the page (and is
 * slower than the same writes split into changes).
 */
export const BULK_SAVE_THRESHOLD = 500;

/**
 * IndexedDB prefixes. Documents, push cursors and the actor id are per device;
 * pack and floor caches are shared by every instance using the same namespace.
 */
export const IDB_DOCS = "diffuse/output/firkin/encrypted/incremental-automerge";
export const IDB_ACTOR = `${IDB_DOCS}/actor`;
export const IDB_FLOORS = "diffuse/output/firkin/encrypted/incremental-automerge/floors";
export const IDB_PACKS = "diffuse/output/firkin/encrypted/incremental-automerge/packs";

/**
 * Prefix of the registry namespace holding each output's append logs.
 *
 * Follows the shared output scheme, `diffuse.output.<kind>`, so the registry
 * stays organised by what owns each entry. The element's `namespace` attribute
 * is appended so separate outputs don't share a log.
 */
export const REGISTRY_NAMESPACE_PREFIX = "diffuse.output.automerge.incremental";

/** Raw block codec for log entries. */
export const CODEC_RAW = 0x55;

/** dag-cbor codec for the per-device manifests. */
export const CODEC_DAG_CBOR = 0x71;

/** Version of the manifest payload written to the registry. */
export const MANIFEST_VERSION = 2;

/**
 * How long one namespace listing is reused.
 *
 * Every collection of an output lives in the same namespace, so a pull asks
 * the same question five times in a row. Coalescing those is enough; anything
 * longer would hide another device's write from an explicit reload.
 */
export const NAMES_TTL_MS = 1_000;

export const NOT_CONNECTED =
  "Firkin is not connected. Sign in on the Connect → Firkin page first.";

/**
 * Registry namespace for one output element. The element's `namespace`
 * attribute is base64url-encoded so any value is a valid Firkin namespace.
 *
 * @param {string | null | undefined} namespace
 */
export function registryNamespace(namespace) {
  return `${REGISTRY_NAMESPACE_PREFIX}.${encodeFileName(namespace ?? "default")}`;
}

/**
 * Registry name of one writer's manifest for a log. Devices are separated by a
 * dot (the collection name is base64url and a device id is hex, so neither
 * contains one), which lets a reader split them back apart.
 *
 * @param {string} name
 * @param {string} device
 */
export function logName(name, device) {
  return `${encodeFileName(name)}.${device}`;
}
