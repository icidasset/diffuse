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
});
