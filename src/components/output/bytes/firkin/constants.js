import { encodeFileName } from "~/common/firkin/files.js";

/** Prefix of the registry namespace holding each output's collection pointers. */
export const REGISTRY_NAMESPACE_PREFIX = "diffuse.output.firkin";

/** Raw block codec for the encoded collection bytes. */
export const CODEC_RAW = 0x55;

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
