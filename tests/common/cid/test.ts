import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("common/cid", () => {
  describe("create", () => {
    it("returns a non-empty CID string", async () => {
      const result = await testWeb(async () => {
        const { create } = await import("~/common/cid.js");
        return create(0x55, new TextEncoder().encode("hello"));
      });
      expect(typeof result).toBe("string");
      expect(result.length).toBeGreaterThan(0);
    });

    it("returns consistent CID for same input", async () => {
      const result = await testWeb(async () => {
        const { create } = await import("~/common/cid.js");
        const data = new TextEncoder().encode("hello world");
        const a = await create(0x55, data);
        const b = await create(0x55, data);
        return a === b;
      });
      expect(result).toBe(true);
    });

    it("returns different CIDs for different inputs", async () => {
      const result = await testWeb(async () => {
        const { create } = await import("~/common/cid.js");
        const a = await create(0x55, new TextEncoder().encode("hello"));
        const b = await create(0x55, new TextEncoder().encode("world"));
        return a === b;
      });
      expect(result).toBe(false);
    });

    it("returns different CIDs for different codecs", async () => {
      const result = await testWeb(async () => {
        const { create } = await import("~/common/cid.js");
        const data = new TextEncoder().encode("hello");
        const a = await create(0x55, data);
        const b = await create(0x71, data);
        return a === b;
      });
      expect(result).toBe(false);
    });

    it("returns a CID starting with 'bafy'", async () => {
      const result = await testWeb(async () => {
        const { create } = await import("~/common/cid.js");
        return create(0x55, new TextEncoder().encode("test"));
      });
      expect(result.startsWith("bafy")).toBe(true);
    });
  });

  describe("verify", () => {
    it("returns true for matching data and CID", async () => {
      const result = await testWeb(async () => {
        const { create, verify } = await import("~/common/cid.js");
        const data = new TextEncoder().encode("hello");
        const cid = await create(0x55, data);
        return verify(data, cid);
      });
      expect(result).toBe(true);
    });

    it("returns false for mismatched data", async () => {
      const result = await testWeb(async () => {
        const { create, verify } = await import("~/common/cid.js");
        const data = new TextEncoder().encode("hello");
        const cid = await create(0x55, data);
        return verify(new TextEncoder().encode("world"), cid);
      });
      expect(result).toBe(false);
    });
  });
});
