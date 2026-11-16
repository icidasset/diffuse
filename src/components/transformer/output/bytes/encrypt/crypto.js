/** AES-GCM nonce length, prepended to each entry's ciphertext. */
export const IV_LENGTH = 12;

/** Bytes of framing before each entry: a big-endian u32 payload length. */
export const LENGTH_PREFIX = 4;

/**
 * Pack one plaintext entry as `[u32 length][iv][ciphertext]`.
 *
 * A log value is the concatenation of these frames, so a reader can split it
 * back into entries without knowing anything about the layer above — which is
 * what keeps the encrypted output a plain `Uint8Array` store.
 *
 * @param {CryptoKey} key
 * @param {Uint8Array} plaintext
 * @returns {Promise<Uint8Array>}
 */
export async function encryptEntry(key, plaintext) {
  const iv = crypto.getRandomValues(new Uint8Array(IV_LENGTH));

  const sealed = new Uint8Array(
    await crypto.subtle.encrypt(
      { name: "AES-GCM", iv },
      key,
      buffer(plaintext),
    ),
  );

  const payload = new Uint8Array(iv.length + sealed.length);
  payload.set(iv, 0);
  payload.set(sealed, iv.length);

  const frame = new Uint8Array(LENGTH_PREFIX + payload.length);
  new DataView(frame.buffer).setUint32(0, payload.length);
  frame.set(payload, LENGTH_PREFIX);

  return frame;
}

/**
 * Split a log value into frames and decrypt each one.
 *
 * @param {CryptoKey} key
 * @param {Uint8Array} value
 * @returns {Promise<Uint8Array>} The plaintext entries, concatenated.
 */
export async function decryptLog(key, value) {
  /** @type {Uint8Array[]} */
  const entries = [];

  let offset = 0;
  while (offset < value.length) {
    if (offset + LENGTH_PREFIX > value.length) {
      throw new Error("Encrypted log ends mid-frame");
    }

    const length = new DataView(
      value.buffer,
      value.byteOffset + offset,
      LENGTH_PREFIX,
    ).getUint32(0);

    const start = offset + LENGTH_PREFIX;
    const end = start + length;
    if (length <= IV_LENGTH || end > value.length) {
      throw new Error("Encrypted log has an invalid frame length");
    }

    const payload = value.subarray(start, end);
    const iv = payload.subarray(0, IV_LENGTH);
    const sealed = payload.subarray(IV_LENGTH);

    entries.push(
      new Uint8Array(
        await crypto.subtle.decrypt(
          { name: "AES-GCM", iv: buffer(iv) },
          key,
          buffer(sealed),
        ),
      ),
    );

    offset = end;
  }

  return concat(entries);
}

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
 * @param {Uint8Array[]} entries
 */
function concat(entries) {
  const total = entries.reduce((sum, bytes) => sum + bytes.length, 0);
  const out = new Uint8Array(total);

  let offset = 0;
  for (const bytes of entries) {
    out.set(bytes, offset);
    offset += bytes.length;
  }

  return out;
}
