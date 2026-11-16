import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import {
  PBKDF2_ITERATIONS,
  derivePassphraseKey,
  newSalt,
} from "~/common/firkin/output-key.js";
import {
  decryptLog,
  encryptEntry,
} from "~/components/transformer/output/bytes/encrypt/crypto.js";

// PBKDF2 at the real work factor is slow by design; the derivation tests only
// care that it is deterministic and separated, so they use a cheap count.
const FAST = 1_000;

const derive = (
  salt: Uint8Array,
  passphrase: string,
  iterations = FAST,
) => derivePassphraseKey(passphrase, salt, iterations);

describe("common/firkin/output-key", () => {
  it("derives the same key from the same passphrase and salt", async () => {
    const salt = newSalt();

    const first = await derive(salt, "correct horse");
    const second = await derive(salt, "correct horse");

    const frame = await encryptEntry(first, new TextEncoder().encode("hi"));
    const plaintext = await decryptLog(second, frame);

    expect(new TextDecoder().decode(plaintext)).toBe("hi");
  });

  it("separates keys by salt", async () => {
    const first = await derive(newSalt(), "correct horse");
    const second = await derive(newSalt(), "correct horse");

    const frame = await encryptEntry(first, new TextEncoder().encode("hi"));

    let failed = false;
    try {
      await decryptLog(second, frame);
    } catch {
      failed = true;
    }

    expect(failed).toBe(true);
  });

  it("separates keys by passphrase", async () => {
    const salt = newSalt();

    const first = await derive(salt, "correct horse");
    const second = await derive(salt, "battery staple");

    const frame = await encryptEntry(first, new TextEncoder().encode("hi"));

    let failed = false;
    try {
      await decryptLog(second, frame);
    } catch {
      failed = true;
    }

    expect(failed).toBe(true);
  });

  it("produces a non-extractable key", async () => {
    const key = await derive(newSalt(), "correct horse");

    expect(key.extractable).toBe(false);
    expect(key.algorithm.name).toBe("AES-GCM");
  });

  it("uses a fresh salt every time", () => {
    const first = newSalt();
    const second = newSalt();

    expect(first.length).toBe(16);
    expect([...first]).not.toEqual([...second]);
  });

  it("defaults to the documented work factor", () => {
    expect(PBKDF2_ITERATIONS).toBeGreaterThanOrEqual(600_000);
  });
});
