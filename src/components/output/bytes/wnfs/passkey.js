import { xchacha20poly1305 } from "@noble/ciphers/chacha.js";
import { managedNonce } from "@noble/ciphers/utils.js";
import { utf8 } from "iso-base/utf8";

import {
  Signer,
  WebCryptoSigner,
} from "@tokono.ma/firkin";

import { KEY_DERIVATION_INFO, KEY_DERIVATION_SALT } from "./constants.js";

const xchacha = managedNonce(xchacha20poly1305);

/**
 * Derive the symmetric key that encrypts the WNFS exchange private key before
 * it is uploaded to the Firkin registry.
 *
 * @param {ArrayBuffer} prfSecond - The passkey's PRF "encryption" output.
 * @returns {Promise<Uint8Array>}
 */
export async function deriveStorageKey(prfSecond) {
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
      salt:
        /** @type {ArrayBuffer} */ (/** @type {unknown} */ (utf8.decode(
          KEY_DERIVATION_SALT,
        ))),
      info:
        /** @type {ArrayBuffer} */ (/** @type {unknown} */ (utf8.decode(
          KEY_DERIVATION_INFO,
        ))),
    },
    keyMaterial,
    256,
  );

  return new Uint8Array(bits);
}

/**
 * @param {Uint8Array} key
 * @param {unknown} payload
 * @returns {Uint8Array}
 */
export function encryptPrivateKey(key, payload) {
  return xchacha(key).encrypt(utf8.decode(JSON.stringify(payload)));
}

/**
 * @param {Uint8Array} key
 * @param {Uint8Array} bytes
 * @returns {unknown}
 */
export function decryptPrivateKey(key, bytes) {
  return JSON.parse(utf8.encode(xchacha(key).decrypt(bytes)));
}

/**
 * Turn the passkey's PRF "signing" output into the account's ed25519 signer.
 *
 * Prefers a non-extractable WebCrypto key so the seed never lives in JS
 * memory longer than the import. Falls back to the raw seed on browsers
 * without WebCrypto Ed25519.
 *
 * @param {ArrayBuffer} prfFirst
 */
export async function accountSignerFromPrf(prfFirst) {
  const seed = new Uint8Array(prfFirst);

  if (seed.length !== 32) {
    throw new Error("Passkey PRF output was not 32 bytes");
  }

  try {
    return await WebCryptoSigner.fromSeedNonExtractable(seed);
  } catch {
    return await Signer.fromSeed(seed);
  }
}

/**
 * Import the WNFS exchange private key (JWK) as a non-extractable key.
 *
 * @param {JsonWebKey} jwk
 * @returns {Promise<CryptoKey>}
 */
export async function importExchangePrivateKey(jwk) {
  return await crypto.subtle.importKey(
    "jwk",
    jwk,
    { name: "RSA-OAEP", hash: "SHA-256" },
    false,
    ["decrypt"],
  );
}

/**
 * Generate an extractable RSA-OAEP-256 exchange key pair. It has to be
 * extractable so the private JWK can be encrypted and stored in the registry
 * for recovery on another device.
 */
export async function generateExchangeKeyPair() {
  return /** @type {CryptoKeyPair} */ (
    await crypto.subtle.generateKey(
      {
        name: "RSA-OAEP",
        modulusLength: 2048,
        publicExponent: new Uint8Array([0x01, 0x00, 0x01]),
        hash: "SHA-256",
      },
      true,
      ["encrypt", "decrypt"],
    )
  );
}
