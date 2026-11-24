import * as IDB from "idb-keyval";

import { signal } from "~/common/signal.js";
import { deriveCipherKey } from "~/common/passkey.js";

/**
 * The key that encrypts output entries can come from more than one place. It is
 * always the same thing once unlocked — a non-extractable AES-GCM key — so the
 * encryption element never has to know where it came from.
 *
 * - **passkey** — derived from the Firkin passkey's second PRF output. Needs no
 *   extra input, but only reproduces on a device with the same credential
 *   (i.e. a passkey the OS syncs).
 * - **passphrase** — derived from a passphrase with PBKDF2. Reproduces anywhere
 *   the user can type it, which is what makes a second device possible when
 *   passkeys aren't synced.
 *
 * Both are stored the same way: the non-extractable handle in IndexedDB, so
 * later page loads don't need the secret again.
 */

/** HKDF label of the passkey-derived output key. */
export const OUTPUT_KEY_INFO = "diffuse-firkin-output";

/**
 * HKDF label of the field key used by Record Sync.
 *
 * Key separation: never reuse the entry-encryption key for field values.
 */
export const FIELD_KEY_INFO = "diffuse-firkin-record-fields";

/** IndexedDB key for the non-extractable output encryption key. */
export const IDB_OUTPUT_KEY = "diffuse/firkin/output-key";

/** IndexedDB key for the Record Sync field encryption key. */
export const IDB_FIELD_KEY = "diffuse/firkin/field-key";

/** IndexedDB key for where the key came from, plus the passphrase salt. */
export const IDB_OUTPUT_META = "diffuse/firkin/output-key-meta";

/** PBKDF2 work factor. OWASP's 2023 minimum for PBKDF2-HMAC-SHA256. */
export const PBKDF2_ITERATIONS = 600_000;

/**
 * The stored output key as reactive state, so consumers (the encryption
 * element) can read it without the wiring having to deliver it at exactly
 * the right moment.
 */
const keySignal = signal(/** @type {CryptoKey | undefined} */ (undefined));
/** The Record Sync field key; independent of the entry key. */
const fieldKeySignal = signal(/** @type {CryptoKey | undefined} */ (undefined));

/**
 * The stored output key, reactively: `undefined` until the key is derived by
 * a sign-in (on this page or another tab) and again after sign-out.
 *
 * @returns {CryptoKey | undefined}
 */
export function outputKey() {
  return keySignal.get();
}

/**
 * The Record Sync field key, reactively.
 *
 * A device that unlocked the output before Record Sync existed has the entry
 * key but not this one; the Record Sync output stays not-ready until the user
 * signs in again.
 *
 * @returns {CryptoKey | undefined}
 */
export function fieldKey() {
  return fieldKeySignal.get();
}

// Hydrate once, when this module is first imported. Deliberately only ever
// *adds* a key here: a store that commits while this read is in flight must
// not be overwritten with `undefined` (sign-out clears it explicitly).
void loadOutputKey().then((stored) => {
  if (stored) keySignal.set(stored);
});

void loadFieldKey().then((stored) => {
  if (stored) fieldKeySignal.set(stored);
});

const channel = (() => {
  try {
    return typeof BroadcastChannel === "undefined"
      ? undefined
      : new BroadcastChannel("diffuse/firkin/output-key");
  } catch {
    // An opaque origin or a runtime without the API; other tabs then simply
    // pick the key up on their next page load.
    return undefined;
  }
})();

// Sign-in/out in another tab: re-read the source of truth. Unlike the
// hydration above this may also clear the signal, matching the other tab.
channel?.addEventListener("message", () => {
  void loadOutputKey().then((stored) => keySignal.set(stored));
  void loadFieldKey().then((stored) => fieldKeySignal.set(stored));
});

/** @typedef {"passkey" | "passphrase"} OutputKeySource */

/**
 * @typedef {Object} OutputKeyMeta
 * @property {OutputKeySource} source
 * @property {Uint8Array} [salt] - Present for the passphrase source.
 */

/**
 * @param {Uint8Array} raw
 * @returns {Promise<CryptoKey>}
 */
export async function importAesKey(raw) {
  return await crypto.subtle.importKey(
    "raw",
    /** @type {BufferSource} */ (/** @type {unknown} */ (raw)),
    { name: "AES-GCM" },
    false,
    ["encrypt", "decrypt"],
  );
}

/** A fresh 16-byte salt for a passphrase derivation. */
export function newSalt() {
  return crypto.getRandomValues(new Uint8Array(16));
}

/**
 * Derive the output key from a passphrase.
 *
 * The salt is per account and persisted, so the same passphrase always lands on
 * the same key for this output — which is what lets another device decrypt.
 *
 * @param {string} passphrase
 * @param {Uint8Array} salt
 * @param {number} [iterations]
 * @returns {Promise<CryptoKey>}
 */
export async function derivePassphraseKey(
  passphrase,
  salt,
  iterations = PBKDF2_ITERATIONS,
) {
  return (await derivePassphraseKeys(passphrase, salt, iterations)).entry;
}

/**
 * Derive both keys from one passphrase stretch.
 *
 * The entry key is the first 256 bits, so it stays identical to the previous
 * single-key derivation; the field key is the next 256 bits.
 *
 * @param {string} passphrase
 * @param {Uint8Array} salt
 * @param {number} [iterations]
 * @returns {Promise<{ entry: CryptoKey; field: CryptoKey }>}
 */
async function derivePassphraseKeys(
  passphrase,
  salt,
  iterations = PBKDF2_ITERATIONS,
) {
  const material = await crypto.subtle.importKey(
    "raw",
    new TextEncoder().encode(passphrase),
    "PBKDF2",
    false,
    ["deriveBits"],
  );

  const bits = await crypto.subtle.deriveBits(
    {
      name: "PBKDF2",
      hash: "SHA-256",
      salt: /** @type {BufferSource} */ (/** @type {unknown} */ (salt)),
      iterations,
    },
    material,
    512,
  );

  const raw = new Uint8Array(bits);

  return {
    entry: await importAesKey(raw.slice(0, 32)),
    field: await importAesKey(raw.slice(32)),
  };
}

/**
 * Derive the output key from the Firkin passkey and store it.
 *
 * @param {ArrayBuffer} prfSecond
 * @returns {Promise<CryptoKey>}
 */
export async function storePasskeyOutputKey(prfSecond) {
  const entry = await importAesKey(
    await deriveCipherKey(prfSecond, OUTPUT_KEY_INFO),
  );
  const field = await importAesKey(
    await deriveCipherKey(prfSecond, FIELD_KEY_INFO),
  );

  await storeOutputKeys(entry, field, { source: "passkey" });

  return entry;
}

/**
 * Unlock the output from a passphrase, reusing the account's salt.
 *
 * @param {string} passphrase
 * @returns {Promise<CryptoKey>}
 */
export async function unlockOutputKeyWithPassphrase(passphrase) {
  const meta = await readMeta();
  const salt = meta?.salt instanceof Uint8Array ? meta.salt : newSalt();

  const keys = await derivePassphraseKeys(passphrase, salt);
  await storeOutputKeys(keys.entry, keys.field, { source: "passphrase", salt });

  return keys.entry;
}

/**
 * @param {CryptoKey} entry
 * @param {CryptoKey} field
 * @param {OutputKeyMeta} meta
 */
async function storeOutputKeys(entry, field, meta) {
  await IDB.set(IDB_OUTPUT_KEY, entry);
  await IDB.set(IDB_FIELD_KEY, field);
  await IDB.set(IDB_OUTPUT_META, meta);

  keySignal.set(entry);
  fieldKeySignal.set(field);
  channel?.postMessage(null);
}

/**
 * The stored output key, if this device has unlocked it before.
 *
 * @returns {Promise<CryptoKey | undefined>}
 */
export async function loadOutputKey() {
  try {
    const stored = await IDB.get(IDB_OUTPUT_KEY);

    return stored instanceof CryptoKey ? stored : undefined;
  } catch {
    return undefined;
  }
}

/**
 * The stored Record Sync field key, if this device has unlocked it before.
 *
 * @returns {Promise<CryptoKey | undefined>}
 */
export async function loadFieldKey() {
  try {
    const stored = await IDB.get(IDB_FIELD_KEY);

    return stored instanceof CryptoKey ? stored : undefined;
  } catch {
    return undefined;
  }
}

/**
 * Where the stored key came from, so a UI can ask for the right secret (or none
 * at all).
 *
 * @returns {Promise<OutputKeyMeta | undefined>}
 */
export async function readOutputKeyMeta() {
  return await readMeta();
}

/** Forget the output key (and its passphrase salt). */
export async function forgetOutputKey() {
  await IDB.del(IDB_OUTPUT_KEY);
  await IDB.del(IDB_FIELD_KEY);
  await IDB.del(IDB_OUTPUT_META);

  keySignal.set(undefined);
  fieldKeySignal.set(undefined);
  channel?.postMessage(null);
}

/** @returns {Promise<OutputKeyMeta | undefined>} */
async function readMeta() {
  try {
    const stored = await IDB.get(IDB_OUTPUT_META);

    if (!stored || typeof stored !== "object") return undefined;
    if (stored.source !== "passkey" && stored.source !== "passphrase") {
      return undefined;
    }

    return /** @type {OutputKeyMeta} */ (stored);
  } catch {
    return undefined;
  }
}
