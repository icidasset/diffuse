import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("common/passkey", () => {
  it("isEncryptedUri returns true for encrypted:// URIs", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/common/passkey.js");
      return mod.isEncryptedUri("encrypted://abc123def456");
    });

    expect(result).toBe(true);
  });

  it("isEncryptedUri returns false for plain URIs", async () => {
    const results = await testWeb(async () => {
      const mod = await import("~/common/passkey.js");
      return [
        mod.isEncryptedUri("https://example.com/track.mp3"),
        mod.isEncryptedUri("icecast://radio.example.com/stream"),
        mod.isEncryptedUri("local://tid/path.mp3"),
        mod.isEncryptedUri(""),
      ];
    });

    expect(results).toEqual([false, false, false, false]);
  });

  it("encryptUri and decryptUri round-trip preserves the original URI", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/common/passkey.js");

      const key = crypto.getRandomValues(new Uint8Array(32));
      const original = "https://example.com/my-track.mp3";

      const encrypted = mod.encryptUri(key, original);
      const decrypted = mod.decryptUri(key, encrypted);

      return { encrypted: encrypted.startsWith("encrypted://"), decrypted };
    });

    expect(result.encrypted).toBe(true);
    expect(result.decrypted).toBe("https://example.com/my-track.mp3");
  });

  it("encryptUri produces different ciphertext each time (managed nonce)", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/common/passkey.js");

      const key = crypto.getRandomValues(new Uint8Array(32));
      const uri = "https://example.com/track.mp3";

      const enc1 = mod.encryptUri(key, uri);
      const enc2 = mod.encryptUri(key, uri);
      return enc1 !== enc2;
    });

    expect(result).toBe(true);
  });

  it("decryptUri with wrong key throws", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/common/passkey.js");

      const key1 = crypto.getRandomValues(new Uint8Array(32));
      const key2 = crypto.getRandomValues(new Uint8Array(32));
      const encrypted = mod.encryptUri(key1, "https://example.com/track.mp3");

      try {
        mod.decryptUri(key2, encrypted);
        return false;
      } catch {
        return true;
      }
    });

    expect(result).toBe(true);
  });
});
