// @ts-nocheck -- the evaluated browser function is untyped

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("common/firkin/output-key (browser)", () => {
  it("stores, reloads and forgets a passphrase key", async () => {
    const result = await testWeb(async () => {
      const {
        forgetOutputKey,
        loadOutputKey,
        readOutputKeyMeta,
        unlockOutputKeyWithPassphrase,
      } = await import("~/common/firkin/output-key.js");

      const before = {
        key: await loadOutputKey(),
        meta: await readOutputKeyMeta(),
      };

      await unlockOutputKeyWithPassphrase("a memorable passphrase");

      const stored = await loadOutputKey();
      const meta = await readOutputKeyMeta();

      await forgetOutputKey();

      return {
        beforeKey: before.key !== undefined,
        beforeMeta: before.meta !== undefined,
        stored: stored instanceof CryptoKey,
        extractable: stored?.extractable,
        algorithm: stored?.algorithm?.name,
        source: meta?.source,
        saltLength: meta?.salt?.length,
        afterForget: (await loadOutputKey()) !== undefined,
      };
    });

    expect(result.beforeKey).toBe(false);
    expect(result.beforeMeta).toBe(false);
    expect(result.stored).toBe(true);
    expect(result.extractable).toBe(false);
    expect(result.algorithm).toBe("AES-GCM");
    expect(result.source).toBe("passphrase");
    expect(result.saltLength).toBe(16);
    expect(result.afterForget).toBe(false);
  });

  it("derives the key from the passkey's PRF output", async () => {
    const result = await testWeb(async () => {
      const {
        forgetOutputKey,
        loadOutputKey,
        outputKey,
        readOutputKeyMeta,
        storePasskeyOutputKey,
      } = await import("~/common/firkin/output-key.js");

      // The PRF extension yields 32 raw bytes per evaluation; sign-in passes the
      // second one (`…encryption`) straight to this function.
      const prfSecond = crypto.getRandomValues(new Uint8Array(32)).buffer;

      await storePasskeyOutputKey(prfSecond);

      const stored = await loadOutputKey();
      const meta = await readOutputKeyMeta();
      const reactive = outputKey() !== undefined;

      // Same PRF bytes must land on the same key, and a different purpose must
      // not: that separation is what keeps this key apart from the track-URI key.
      const { deriveCipherKey } = await import(
        "~/components/transformer/output/refiner/passkey-encryption/passkey.js"
      );

      const iv = crypto.getRandomValues(new Uint8Array(12));
      const message = new TextEncoder().encode("round trip");
      const sealed = await crypto.subtle.encrypt(
        { name: "AES-GCM", iv },
        /** @type {CryptoKey} */ (stored),
        message,
      );
      const opened = new Uint8Array(
        await crypto.subtle.decrypt({ name: "AES-GCM", iv }, stored, sealed),
      );

      const other = await deriveCipherKey(prfSecond, "some-other-purpose");
      const same = await deriveCipherKey(prfSecond, "diffuse-firkin-output");

      await forgetOutputKey();

      return {
        reactive,
        stored: stored instanceof CryptoKey,
        extractable: stored?.extractable,
        algorithm: stored?.algorithm?.name,
        source: meta?.source,
        salt: meta?.salt,
        opened: new TextDecoder().decode(opened),
        purposesDiffer: same.some((byte, index) => byte !== other[index]),
        afterForget: (await loadOutputKey()) !== undefined,
      };
    });

    expect(result.reactive).toBe(true);
    expect(result.stored).toBe(true);
    expect(result.extractable).toBe(false);
    expect(result.algorithm).toBe("AES-GCM");
    expect(result.source).toBe("passkey");
    expect(result.salt).toBe(undefined);
    expect(result.opened).toBe("round trip");
    expect(result.purposesDiffer).toBe(true);
    expect(result.afterForget).toBe(false);
  });
});
