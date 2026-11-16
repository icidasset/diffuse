import { encodeFileName } from "~/common/firkin/files.js";

/** Prefix of the registry namespace holding each output's append logs. */
export const REGISTRY_NAMESPACE_PREFIX = "diffuse.output.firkin";

/** Raw block codec for log entries. */
export const CODEC_RAW = 0x55;

/** dag-cbor codec for the per-device manifests. */
export const CODEC_DAG_CBOR = 0x71;

/** Version of the manifest payload written to the registry. */
export const MANIFEST_VERSION = 2;

/** IndexedDB prefix for the newest accepted sequence per device. */
export const IDB_FLOORS = "diffuse/output/bytes/firkin/floors";

/** IndexedDB prefix for cached log entries, keyed by CID. */
export const IDB_PACKS = "diffuse/output/bytes/firkin/packs";

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
