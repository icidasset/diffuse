// @ts-nocheck -- javadoc-only module, exercised through the element too

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import {
  IV_LENGTH,
  LENGTH_PREFIX,
  decryptLog,
  encryptEntry,
} from "~/components/transformer/output/bytes/encrypt/crypto.js";

/** @param {string} text */
const bytes = (text) => new TextEncoder().encode(text);

/** @param {Uint8Array} value */
const text = (value) => new TextDecoder().decode(value);

function aesKey() {
  return crypto.subtle.generateKey({ name: "AES-GCM", length: 256 }, false, [
    "encrypt",
    "decrypt",
  ]);
}

/** @param {Uint8Array[]} entries */
function join(entries) {
  const total = entries.reduce((sum, entry) => sum + entry.length, 0);
  const out = new Uint8Array(total);

  let offset = 0;
  for (const entry of entries) {
    out.set(entry, offset);
    offset += entry.length;
  }

  return out;
}

describe("components/transformer/output/bytes/encrypt/crypto", () => {
  it("round-trips a single entry", async () => {
    const key = await aesKey();

    const frame = await encryptEntry(key, bytes("hello"));

    expect(frame.length).toBe(LENGTH_PREFIX + IV_LENGTH + 5 + 16);
    expect(text(await decryptLog(key, frame))).toBe("hello");
  });

  it("keeps the plaintext out of the frame", async () => {
    const key = await aesKey();

    const frame = await encryptEntry(key, bytes("distinctive-plaintext"));

    expect(text(frame)).not.toContain("distinctive-plaintext");
  });

  it("round-trips a concatenated log", async () => {
    const key = await aesKey();

    const log = join([
      await encryptEntry(key, bytes("one")),
      await encryptEntry(key, bytes("two")),
      await encryptEntry(key, bytes("three")),
    ]);

    expect(text(await decryptLog(key, log))).toBe("onetwothree");
  });

  it("uses a fresh nonce per entry", async () => {
    const key = await aesKey();

    const first = await encryptEntry(key, bytes("same"));
    const second = await encryptEntry(key, bytes("same"));

    expect(first.length).toBe(second.length);
    expect([...first]).not.toEqual([...second]);
  });

  it("refuses another key", async () => {
    const key = await aesKey();
    const other = await aesKey();

    const frame = await encryptEntry(key, bytes("secret"));

    let error = "";
    try {
      await decryptLog(other, frame);
    } catch (err) {
      error = err instanceof Error ? err.message : String(err);
    }

    expect(error.length).toBeGreaterThan(0);
  });

  it("refuses a tampered entry", async () => {
    const key = await aesKey();

    const frame = await encryptEntry(key, bytes("secret"));
    frame[frame.length - 1] ^= 0xff;

    let error = "";
    try {
      await decryptLog(key, frame);
    } catch (err) {
      error = err instanceof Error ? err.message : String(err);
    }

    expect(error.length).toBeGreaterThan(0);
  });

  it("refuses a truncated log", async () => {
    const key = await aesKey();

    const frame = await encryptEntry(key, bytes("secret"));

    for (const cut of [1, LENGTH_PREFIX]) {
      let error = "";
      try {
        await decryptLog(key, frame.subarray(0, frame.length - cut));
      } catch (err) {
        error = err instanceof Error ? err.message : String(err);
      }

      expect(error.length).toBeGreaterThan(0);
    }
  });

  it("treats an empty log as empty plaintext", async () => {
    const key = await aesKey();

    expect((await decryptLog(key, new Uint8Array(0))).length).toBe(0);
  });
});
