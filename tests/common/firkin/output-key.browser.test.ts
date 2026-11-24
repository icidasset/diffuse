// @ts-nocheck -- the evaluated browser function is untyped

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("common/firkin/output-key (browser)", () => {
  it("stores, reloads and forgets a passphrase key", async () => {
    const result = await testWeb(async () => {
      const {
        fieldKey,
        forgetOutputKey,
        loadFieldKey,
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
      const field = await loadFieldKey();
      const fieldReactive = fieldKey() !== undefined;
      const meta = await readOutputKeyMeta();

      // The field key is derived from the same secret but is a different key:
      // the entry key must not open something sealed with the field key.
      const iv = crypto.getRandomValues(new Uint8Array(12));
      const message = new TextEncoder().encode("separation");
      const sealed = await crypto.subtle.encrypt(
        { name: "AES-GCM", iv },
        /** @type {CryptoKey} */ (field),
        message,
      );

      let entryOpensField = true;

      try {
        await crypto.subtle.decrypt(
          { name: "AES-GCM", iv },
          /** @type {CryptoKey} */ (stored),
          sealed,
        );
      } catch {
        entryOpensField = false;
      }

      await forgetOutputKey();

      return {
        beforeKey: before.key !== undefined,
        beforeMeta: before.meta !== undefined,
        stored: stored instanceof CryptoKey,
        extractable: stored?.extractable,
        algorithm: stored?.algorithm?.name,
        field: field instanceof CryptoKey,
        fieldReactive,
        source: meta?.source,
        saltLength: meta?.salt?.length,
        entryOpensField,
        afterForget: (await loadOutputKey()) !== undefined,
        fieldAfterForget: (await loadFieldKey()) !== undefined,
      };
    });

    expect(result.beforeKey).toBe(false);
    expect(result.beforeMeta).toBe(false);
    expect(result.stored).toBe(true);
    expect(result.extractable).toBe(false);
    expect(result.algorithm).toBe("AES-GCM");
    expect(result.field).toBe(true);
    expect(result.fieldReactive).toBe(true);
    expect(result.source).toBe("passphrase");
    expect(result.saltLength).toBe(16);
    expect(result.entryOpensField).toBe(false);
    expect(result.afterForget).toBe(false);
    expect(result.fieldAfterForget).toBe(false);
  });

  it("derives the key from the passkey's PRF output", async () => {
    const result = await testWeb(async () => {
      const {
        fieldKey,
        forgetOutputKey,
        loadFieldKey,
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
      const field = await loadFieldKey();
      const fieldReactive = fieldKey() !== undefined;
      const meta = await readOutputKeyMeta();
      const reactive = outputKey() !== undefined;

      // Same PRF bytes must land on the same key, and a different purpose must
      // not: that separation is what keeps this key apart from the track-URI key.
      const { deriveCipherKey } = await import(
        "~/common/passkey.js"
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
        fieldReactive,
        field: field instanceof CryptoKey,
        stored: stored instanceof CryptoKey,
        extractable: stored?.extractable,
        algorithm: stored?.algorithm?.name,
        source: meta?.source,
        salt: meta?.salt,
        opened: new TextDecoder().decode(opened),
        purposesDiffer: same.some((byte, index) => byte !== other[index]),
        afterForget: (await loadOutputKey()) !== undefined,
        fieldAfterForget: (await loadFieldKey()) !== undefined,
      };
    });

    expect(result.reactive).toBe(true);
    expect(result.fieldReactive).toBe(true);
    expect(result.field).toBe(true);
    expect(result.stored).toBe(true);
    expect(result.extractable).toBe(false);
    expect(result.algorithm).toBe("AES-GCM");
    expect(result.source).toBe("passkey");
    expect(result.salt).toBe(undefined);
    expect(result.opened).toBe("round trip");
    expect(result.purposesDiffer).toBe(true);
    expect(result.afterForget).toBe(false);
    expect(result.fieldAfterForget).toBe(false);
  });
});
