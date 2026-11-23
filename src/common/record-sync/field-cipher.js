import { decode, encode } from "cborg";
import { base64url } from "iso-base/rfc4648";
import { utf8 } from "iso-base/utf8";

/**
 * Field-level encryption for Record Sync.
 *
 * Prototype for `specs/record-sync/SPEC.md`; not wired into an output yet.
 * The point is to measure whether sealing whole values is compatible with an
 * idempotent patch log: an unchanged plaintext must keep its ciphertext, or
 * every save rewrites every record (new CIDs, new entries, new pulls).
 *
 * The envelope is a standard COSE_Encrypt0 (RFC 9052) with A256GCM (RFC 9053,
 * COSE algorithm 3). The associated data binds the value to its position, so a
 * ciphertext cannot be moved to another collection, record or field.
 *
 * Symmetric keys are quantum-resistant in the relevant sense: Grover halves
 * the effective key length, so a 256-bit key still gives ~128-bit security.
 * The post-quantum work is in the asymmetric layer (signatures/key wrapping);
 * see the spec.
 */

/** COSE algorithm: A256GCM. */
const ALGORITHM = 3;

/** COSE header labels. */
const LABEL_ALGORITHM = 1;
const LABEL_KEY_ID = 4;
const LABEL_IV = 5;

/** Domain separation for the external AAD. */
const CONTEXT = "diffuse.records.field.v1";

const DEFAULT_KEY_ID = "k1";
const KEY_BYTES = 32;
const IV_BYTES = 12;

/**
 * The external AAD for one field: which collection and record it belongs to,
 * and which field it is. Both sides must derive the same value.
 *
 * @param {string} collection
 * @param {string} id
 * @param {string} field
 * @returns {string[]}
 */
export function fieldContext(collection, id, field) {
  return [CONTEXT, collection, id, field];
}

/**
 * @param {Uint8Array} protectedBytes
 * @param {string[]} context
 */
function aad(protectedBytes, context) {
  return encode(["Encrypt0", protectedBytes, encode(context)]);
}

/**
 * WebCrypto's DOM types want an `ArrayBuffer`-backed view; the repo's
 * `Uint8Array` values are typed as `ArrayBufferLike`.
 *
 * @param {Uint8Array} value
 * @returns {BufferSource}
 */
const source = (value) => /** @type {BufferSource} */ (
  /** @type {unknown} */ (value)
);

/**
 * Import raw key bytes (256-bit) as an AES-GCM key.
 *
 * Key separation matters: derive this from a distinct label than the entry
 * encryption key. The spec uses
 * `HKDF-SHA256(secret, info = "diffuse.records.field.v1")`.
 *
 * @param {Uint8Array} raw
 * @returns {Promise<CryptoKey>}
 */
export async function importFieldKey(raw) {
  if (!(raw instanceof Uint8Array) || raw.length !== KEY_BYTES) {
    throw new TypeError("Expected 32 raw key bytes");
  }

  return await crypto.subtle.importKey("raw", source(raw), "AES-GCM", false, [
    "encrypt",
    "decrypt",
  ]);
}

/**
 * Seal one field value.
 *
 * Passing the previous sealed value keeps the ciphertext when the plaintext is
 * unchanged, so a save that only touches other fields does not rewrite this
 * one.
 *
 * @param {string} plaintext
 * @param {Object} options
 * @param {CryptoKey} options.key
 * @param {string[]} options.context
 * @param {string} [options.keyId]
 * @param {Uint8Array} [options.iv] - Fixed nonce, for test vectors only.
 * @param {string} [options.previous] - The field's current sealed value.
 * @returns {Promise<string>} base64url(COSE_Encrypt0)
 */
export async function sealField(plaintext, options) {
  const { key, context, keyId = DEFAULT_KEY_ID, iv, previous } = options ?? {};

  if (!key) throw new Error("The field key is not loaded");
  if (!Array.isArray(context)) {
    throw new TypeError("Expected the field context");
  }

  if (previous) {
    try {
      if (await openField(previous, { key, context }) === plaintext) {
        return previous;
      }
    } catch {
      // Not readable with this key: replace it.
    }
  }

  const nonce = iv ?? crypto.getRandomValues(new Uint8Array(IV_BYTES));

  /** @type {Map<number, number | string>} */
  const header = new Map();
  header.set(LABEL_ALGORITHM, ALGORITHM);
  header.set(LABEL_KEY_ID, keyId);

  const protectedHeader = new Uint8Array(encode(header));
  const unprotected = new Map([[LABEL_IV, nonce]]);

  const ciphertext = new Uint8Array(
    await crypto.subtle.encrypt(
      {
        name: "AES-GCM",
        iv: source(nonce),
        additionalData: source(aad(protectedHeader, context)),
        tagLength: 128,
      },
      key,
      source(utf8.decode(plaintext)),
    ),
  );

  return base64url.encode(
    new Uint8Array(encode([protectedHeader, unprotected, ciphertext])),
  );
}

/**
 * Open one sealed field value. Throws when the ciphertext was tampered with or
 * belongs to another collection, record or field.
 *
 * @param {string} sealed - base64url(COSE_Encrypt0)
 * @param {Object} options
 * @param {CryptoKey} options.key
 * @param {string[]} options.context
 * @returns {Promise<string>}
 */
export async function openField(sealed, options) {
  const { key, context } = options ?? {};

  if (!key) throw new Error("The field key is not loaded");
  if (!Array.isArray(context)) {
    throw new TypeError("Expected the field context");
  }

  const decoded = /** @type {any} */ (
    decode(base64url.decode(sealed), { useMaps: true })
  );

  if (!Array.isArray(decoded) || decoded.length !== 3) {
    throw new TypeError("Not a COSE_Encrypt0 structure");
  }

  const [protectedBytes, unprotected, ciphertext] = decoded;
  if (!(protectedBytes instanceof Uint8Array)) {
    throw new TypeError("Missing COSE protected header");
  }
  if (!(ciphertext instanceof Uint8Array)) {
    throw new TypeError("Missing COSE ciphertext");
  }

  const header = decode(protectedBytes, { useMaps: true });
  if (!(header instanceof Map)) {
    throw new TypeError("Invalid COSE protected header");
  }
  if (header.get(LABEL_ALGORITHM) !== ALGORITHM) {
    throw new Error("Unsupported field cipher algorithm");
  }

  const iv = unprotected instanceof Map ? unprotected.get(LABEL_IV) : undefined;
  if (!(iv instanceof Uint8Array)) {
    throw new TypeError("Missing COSE nonce");
  }

  const plaintext = await crypto.subtle.decrypt(
    {
      name: "AES-GCM",
      iv: source(iv),
      additionalData: source(aad(protectedBytes, context)),
      tagLength: 128,
    },
    key,
    source(ciphertext),
  );

  return utf8.encode(new Uint8Array(plaintext));
}
