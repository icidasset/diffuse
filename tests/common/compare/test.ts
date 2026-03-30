import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("common/compare", () => {
  describe("diff", () => {
    it("returns true for identical primitives", async () => {
      const result = await testWeb(async () => {
        const { diff } = await import("~/common/compare.js");
        return diff(1, 1);
      });
      expect(result).toBe(true);
    });

    it("returns false for different primitives", async () => {
      const result = await testWeb(async () => {
        const { diff } = await import("~/common/compare.js");
        return diff(1, 2);
      });
      expect(result).toBe(false);
    });

    it("returns true for deeply equal objects", async () => {
      const result = await testWeb(async () => {
        const { diff } = await import("~/common/compare.js");
        return diff({ a: 1, b: { c: 2 } }, { a: 1, b: { c: 2 } });
      });
      expect(result).toBe(true);
    });

    it("returns false for objects with different values", async () => {
      const result = await testWeb(async () => {
        const { diff } = await import("~/common/compare.js");
        return diff({ a: 1 }, { a: 2 });
      });
      expect(result).toBe(false);
    });

    it("returns false for objects with different keys", async () => {
      const result = await testWeb(async () => {
        const { diff } = await import("~/common/compare.js");
        return diff({ a: 1 }, { b: 1 });
      });
      expect(result).toBe(false);
    });

    it("returns true for identical arrays", async () => {
      const result = await testWeb(async () => {
        const { diff } = await import("~/common/compare.js");
        return diff([1, 2, 3], [1, 2, 3]);
      });
      expect(result).toBe(true);
    });

    it("returns false for arrays with different elements", async () => {
      const result = await testWeb(async () => {
        const { diff } = await import("~/common/compare.js");
        return diff([1, 2], [1, 3]);
      });
      expect(result).toBe(false);
    });
  });

  describe("strictEquality", () => {
    it("returns true for same primitive value", async () => {
      const result = await testWeb(async () => {
        const { strictEquality } = await import("~/common/compare.js");
        return strictEquality(42, 42);
      });
      expect(result).toBe(true);
    });

    it("returns false for different primitive values", async () => {
      const result = await testWeb(async () => {
        const { strictEquality } = await import("~/common/compare.js");
        return strictEquality(42, 43);
      });
      expect(result).toBe(false);
    });

    it("returns false for same-content objects (reference inequality)", async () => {
      const result = await testWeb(async () => {
        const { strictEquality } = await import("~/common/compare.js");
        return strictEquality({ a: 1 }, { a: 1 });
      });
      expect(result).toBe(false);
    });

    it("returns true for same object reference", async () => {
      const result = await testWeb(async () => {
        const { strictEquality } = await import("~/common/compare.js");
        const obj = { a: 1 };
        return strictEquality(obj, obj);
      });
      expect(result).toBe(true);
    });

    it("returns false for null vs undefined", async () => {
      const result = await testWeb(async () => {
        const { strictEquality } = await import("~/common/compare.js");
        return strictEquality(null, undefined);
      });
      expect(result).toBe(false);
    });
  });
});
