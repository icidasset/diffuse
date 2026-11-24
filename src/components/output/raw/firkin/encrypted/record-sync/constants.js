import { encodeFileName } from "~/common/firkin/files.js";

/**
 * The protocol/transport constants are shared with the Loro output: both use
 * the same manifest, CAR and entry framing on Firkin. Only the log namespace
 * and the local IndexedDB keys differ.
 */
export {
  CODEC_DAG_CBOR,
  CODEC_RAW,
  COLLECTIONS,
  FIRST_READ_TIMEOUT_MS,
  INLINE_ENTRY_MAX_BYTES,
  logName,
  MANIFEST_VERSION,
  NAMES_TTL_MS,
  NOT_CONNECTED,
  NOT_READY_GRACE_MS,
  PUSH_COALESCE_MS,
  SNAPSHOT_EVERY,
} from "../incremental-loro/constants.js";

/** Registry namespace holding each output's Record Sync logs. */
export const REGISTRY_NAMESPACE_PREFIX = "diffuse.output.record-sync";

/** IndexedDB prefixes. Documents, cursors and applied records are per device. */
export const IDB_DOCS = "diffuse/output/record-sync";
export const IDB_PACKS = "diffuse/output/record-sync/packs";

/**
 * Registry namespace for one output element. The element's `namespace`
 * attribute is hex-encoded so any value is a valid Firkin namespace.
 *
 * @param {string | null | undefined} namespace
 */
export function registryNamespace(namespace) {
  return `${REGISTRY_NAMESPACE_PREFIX}.${
    encodeFileName(namespace ?? "default")
  }`;
}
