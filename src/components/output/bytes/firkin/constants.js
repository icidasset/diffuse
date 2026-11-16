import * as IDB from "idb-keyval";

import { encodeFileName } from "~/common/firkin/files.js";

/** Prefix of the registry namespace holding each output's append logs. */
export const REGISTRY_NAMESPACE_PREFIX = "diffuse.output.firkin";

/** Raw block codec for log entries. */
export const CODEC_RAW = 0x55;

/** dag-cbor codec for the per-device manifests. */
export const CODEC_DAG_CBOR = 0x71;

/** Version of the manifest shape written to the registry. */
export const MANIFEST_VERSION = 1;

/** IndexedDB key holding this browser's log writer id. */
export const IDB_DEVICE = "diffuse/output/bytes/firkin/device";

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
 * dot (base64url never contains one), so a name can be split back apart.
 *
 * @param {string} name
 * @param {string} device
 */
export function logName(name, device) {
  return `${encodeFileName(name)}.${device}`;
}

/** @type {Promise<string> | undefined} */
let deviceId;

/**
 * The id this browser writes under. It only has to be unique per account, so a
 * random id persisted in IndexedDB is enough; all tabs share it.
 *
 * @returns {Promise<string>}
 */
export function readDeviceId() {
  deviceId ??= (async () => {
    const stored = await IDB.get(IDB_DEVICE);
    if (typeof stored === "string" && stored.length > 0) return stored;

    const created = crypto.randomUUID();
    await IDB.set(IDB_DEVICE, created);

    return created;
  })();

  return deviceId;
}
