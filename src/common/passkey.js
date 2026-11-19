import { xchacha20poly1305 } from "@noble/ciphers/chacha.js";
import { managedNonce } from "@noble/ciphers/utils.js";

import * as IDB from "idb-keyval";
import { base64url } from "iso-base/rfc4648";
import { utf8 } from "iso-base/utf8";

////////////////////////////////////////////
// CONSTANTS
////////////////////////////////////////////

const IDB_PREFIX = "diffuse/transformer/output/refiner/passkey-encryption";

/**
 * @param {string} namespace
 * @returns {{ credential: string, cipher: string }}
 */
function idbKeys(namespace) {
  const prefix = namespace?.length ? `${IDB_PREFIX}/${namespace}` : IDB_PREFIX;
  return {
    credential: `${prefix}/passkey`,
    cipher: `${prefix}/passkey/cipher-key`,
  };
}

////////////////////////////////////////////
// PRF HELPERS
////////////////////////////////////////////

/**
 * Normalize a PRF result to the ArrayBuffer the rest of this module expects.
 * The DOM types describe PRF results as `BufferSource`, but authenticators
 * return an `ArrayBuffer`.
 *
 * @param {BufferSource | undefined} value
 * @returns {ArrayBuffer | undefined}
 */
function prfBuffer(value) {
  if (!value) return undefined;
  if (value instanceof ArrayBuffer) return value;

  return new Uint8Array(value.buffer, value.byteOffset, value.byteLength)
    .slice().buffer;
}

/**
 * The `prf` extension input for both sign-up and sign-in: separate PRF inputs
 * for the account signing key and the output encryption key.
 *
 * @param {string} rpId
 * @returns {AuthenticationExtensionsClientInputs}
 */
function prfExtensions(rpId) {
  return {
    prf: {
      eval: {
        first: new TextEncoder().encode(rpId + "signing"),
        second: new TextEncoder().encode(rpId + "encryption"),
      },
    },
  };
}

////////////////////////////////////////////
// RELYING PARTY
////////////////////////////////////////////

/**
 * @returns {{ name: string, id: string }}
 */
export function relyingParty() {
  const id = document.location.hostname;
  return { name: id, id };
}

////////////////////////////////////////////
// PASSKEY MANAGEMENT
////////////////////////////////////////////

/**
 * Register a new passkey with the PRF extension.
 *
 * @param {string} namespace
 * @returns {Promise<{ supported: true, credentialId: Uint8Array, prfFirst?: ArrayBuffer, prfSecond: ArrayBuffer } | { supported: false, reason: string }>}
 */
export async function createPasskey(namespace) {
  const rp = relyingParty();
  const challenge = crypto.getRandomValues(new Uint8Array(32));
  const userId = crypto.getRandomValues(new Uint8Array(16));

  /** @type {PublicKeyCredential | null} */
  let credential;

  try {
    credential = /** @type {PublicKeyCredential} */ (
      await navigator.credentials.create({
        publicKey: {
          challenge,
          rp,
          user: {
            id: userId,
            name: rp.id,
            displayName: "Diffuse – " + rp.id,
          },
          pubKeyCredParams: [
            { type: "public-key", alg: -7 },
            { type: "public-key", alg: -257 },
          ],
          attestation: "none",
          authenticatorSelection: {
            userVerification: "required",
            requireResidentKey: true,
            residentKey: "required",
          },
          extensions: prfExtensions(rp.id),
        },
      })
    );
  } catch (err) {
    return {
      supported: false,
      reason: err instanceof Error ? err.message : String(err),
    };
  }

  if (!credential) {
    return { supported: false, reason: "Credential creation returned null" };
  }

  const extensions = credential.getClientExtensionResults();

  if (extensions.prf?.enabled !== true) {
    return {
      supported: false,
      reason: "This authenticator does not support the WebAuthn PRF extension",
    };
  }

  const prfFirst = prfBuffer(extensions.prf?.results?.first);
  const prfSecond = prfBuffer(extensions.prf?.results?.second);

  if (!prfSecond) {
    return {
      supported: false,
      reason: "Authenticator did not return PRF results at registration time",
    };
  }

  const credentialId = new Uint8Array(credential.rawId);
  await IDB.set(idbKeys(namespace).credential, {
    credentialId: Array.from(credentialId),
  });

  return {
    supported: true,
    credentialId,
    prfFirst,
    prfSecond,
  };
}

/**
 * Authenticate with an existing passkey via discoverable-credential lookup
 * (no `allowCredentials`), so it works on a new device that has no stored
 * credential ID yet. Saves the credential ID to IDB and returns PRF material.
 *
 * @param {string} namespace
 * @returns {Promise<{ supported: true, credentialId: Uint8Array, prfFirst?: ArrayBuffer, prfSecond: ArrayBuffer } | { supported: false, reason: string }>}
 */
export async function adoptPasskeyPrfResult(namespace) {
  const rp = relyingParty();
  const challenge = crypto.getRandomValues(new Uint8Array(32));

  /** @type {PublicKeyCredential | null} */
  let assertion;

  try {
    assertion = /** @type {PublicKeyCredential} */ (
      await navigator.credentials.get({
        publicKey: {
          challenge,
          rpId: rp.id,
          userVerification: "required",
          extensions: prfExtensions(rp.id),
        },
      })
    );
  } catch (err) {
    return {
      supported: false,
      reason: err instanceof Error ? err.message : String(err),
    };
  }

  if (!assertion) {
    return { supported: false, reason: "Credential get returned null" };
  }

  const extensions = assertion.getClientExtensionResults();

  const prfFirst = prfBuffer(extensions.prf?.results?.first);
  const prfSecond = prfBuffer(extensions.prf?.results?.second);

  if (!prfSecond) {
    return {
      supported: false,
      reason:
        "This authenticator did not return PRF results — PRF extension may not be supported",
    };
  }

  const credentialId = new Uint8Array(assertion.rawId);
  await IDB.set(idbKeys(namespace).credential, {
    credentialId: Array.from(credentialId),
  });

  return {
    supported: true,
    credentialId,
    prfFirst,
    prfSecond,
  };
}

/**
 * Remove the stored passkey credential ID and cached cipher key from IDB.
 *
 * @param {string} namespace
 * @returns {Promise<void>}
 */
export async function removeStoredPasskey(namespace) {
  const keys = idbKeys(namespace);
  await Promise.all([IDB.del(keys.credential), IDB.del(keys.cipher)]);
}

/**
 * Persist the derived cipher key to IDB so it survives page reloads.
 *
 * @param {string} namespace
 * @param {Uint8Array} key
 * @returns {Promise<void>}
 */
export async function storeCipherKey(namespace, key) {
  await IDB.set(idbKeys(namespace).cipher, key);
}

/**
 * Retrieve the previously persisted cipher key from IDB.
 *
 * @param {string} namespace
 * @returns {Promise<Uint8Array | undefined>}
 */
export async function loadStoredCipherKey(namespace) {
  return await IDB.get(idbKeys(namespace).cipher);
}

////////////////////////////////////////////
// KEY DERIVATION
////////////////////////////////////////////

/** HKDF salt shared by every key derived from the passkey. */
const PASSKEY_SALT = "diffuse-atproto-passkey-salt";

/** Default purpose: the key that hides track URIs in a collection. */
export const TRACK_URI_INFO = "diffuse-atproto-track-uri";

/**
 * Derive a 256-bit key from the PRF "second" output via HKDF.
 * Returns raw bytes suitable for use with XChaCha20-Poly1305 or AES-GCM.
 *
 * `info` separates purposes: the same passkey derives a different key per
 * label, so encrypting output entries never reuses the track-URI key.
 *
 * @param {ArrayBuffer} prfSecond
 * @param {string} [info]
 * @returns {Promise<Uint8Array>}
 */
export async function deriveCipherKey(prfSecond, info = TRACK_URI_INFO) {
  const keyMaterial = await crypto.subtle.importKey(
    "raw",
    prfSecond,
    { name: "HKDF" },
    false,
    ["deriveBits"],
  );

  const bits = await crypto.subtle.deriveBits(
    {
      name: "HKDF",
      hash: "SHA-256",

      salt: new TextEncoder().encode(PASSKEY_SALT),
      info: new TextEncoder().encode(info),
    },
    keyMaterial,
    256,
  );

  return new Uint8Array(bits);
}

////////////////////////////////////////////
// ENCRYPT / DECRYPT
////////////////////////////////////////////

/**
 * Detect whether a URI is encrypted by this module.
 *
 * @param {string} uri
 * @returns {boolean}
 */
export function isEncryptedUri(uri) {
  return uri.startsWith("encrypted://");
}

const xchacha = managedNonce(xchacha20poly1305);

/**
 * Encrypt a plaintext URI with XChaCha20-Poly1305.
 * Returns a string of the form: `encrypted://<base64url(nonce || ciphertext)>`
 * The nonce is prepended automatically by `managedNonce`.
 *
 * @param {Uint8Array} key
 * @param {string} plaintext
 * @returns {string}
 */
export function encryptUri(key, plaintext) {
  const ciphertext = xchacha(key).encrypt(utf8.decode(plaintext));
  return "encrypted://" + base64url.encode(ciphertext);
}

/**
 * Decrypt an encrypted URI produced by `encryptUri`.
 *
 * @param {Uint8Array} key
 * @param {string} encryptedUri
 * @returns {string}
 */
export function decryptUri(key, encryptedUri) {
  const ciphertext = base64url.decode(encryptedUri.slice(12));
  return utf8.encode(xchacha(key).decrypt(ciphertext));
}
