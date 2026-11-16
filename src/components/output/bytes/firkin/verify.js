import { hexToBytes } from "@tokono.ma/firkin";

/**
 * WebCrypto types want an `ArrayBuffer`-backed view; bytes that came out of a
 * decoder can be typed as `ArrayBufferLike`.
 *
 * @param {Uint8Array} bytes
 * @returns {BufferSource}
 */
function buffer(bytes) {
  return /** @type {BufferSource} */ (/** @type {unknown} */ (bytes));
}

/**
 * Verify a manifest signature against the writer's public key.
 *
 * The device id is the hex-encoded verifying key, so the reader never has to
 * trust the manifest for the key. Ed25519 keys are 32 bytes and P-256 keys are
 * 65-byte uncompressed points; anything else is rejected.
 *
 * Returns `false` (rather than throwing) when the browser can't do the
 * algorithm, so a bad manifest is ignored instead of breaking the read.
 *
 * @param {string} device - Hex-encoded public key.
 * @param {Uint8Array} signature
 * @param {Uint8Array} message
 * @returns {Promise<boolean>}
 */
export async function verifySignature(device, signature, message) {
  /** @type {Uint8Array} */
  let key;
  try {
    key = hexToBytes(device);
  } catch {
    return false;
  }

  try {
    if (key.length === 32) {
      const imported = await crypto.subtle.importKey(
        "raw",
        buffer(key),
        { name: "Ed25519" },
        false,
        ["verify"],
      );

      return await crypto.subtle.verify(
        "Ed25519",
        imported,
        buffer(signature),
        buffer(message),
      );
    }

    if (key.length === 65) {
      const imported = await crypto.subtle.importKey(
        "raw",
        buffer(key),
        { name: "ECDSA", namedCurve: "P-256" },
        false,
        ["verify"],
      );

      return await crypto.subtle.verify(
        { name: "ECDSA", hash: "SHA-256" },
        imported,
        buffer(signature),
        buffer(message),
      );
    }
  } catch (err) {
    console.warn("Could not verify a Firkin manifest signature:", err);
  }

  return false;
}
