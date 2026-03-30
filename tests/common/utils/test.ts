import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("common/utils", () => {
  describe("arrayShuffle", () => {
    it("returns empty array for empty input", async () => {
      const result = await testWeb(async () => {
        const { arrayShuffle } = await import("~/common/utils.js");
        return arrayShuffle([]);
      });
      expect(result).toEqual([]);
    });

    it("returns array with same elements", async () => {
      const result = await testWeb(async () => {
        const { arrayShuffle } = await import("~/common/utils.js");
        return arrayShuffle([1, 2, 3, 4, 5]).sort((a, b) => a - b);
      });
      expect(result).toEqual([1, 2, 3, 4, 5]);
    });

    it("does not mutate the original array", async () => {
      const result = await testWeb(async () => {
        const { arrayShuffle } = await import("~/common/utils.js");
        const original = [1, 2, 3];
        arrayShuffle(original);
        return original;
      });
      expect(result).toEqual([1, 2, 3]);
    });

    it("returns a single-element array unchanged", async () => {
      const result = await testWeb(async () => {
        const { arrayShuffle } = await import("~/common/utils.js");
        return arrayShuffle([42]);
      });
      expect(result).toEqual([42]);
    });
  });

  describe("boolAttr", () => {
    it("returns true for empty string (present attribute)", async () => {
      const result = await testWeb(async () => {
        const { boolAttr } = await import("~/common/utils.js");
        return boolAttr("");
      });
      expect(result).toBe(true);
    });

    it("returns false for null", async () => {
      const result = await testWeb(async () => {
        const { boolAttr } = await import("~/common/utils.js");
        return boolAttr(null);
      });
      expect(result).toBe(false);
    });

    it("returns false for undefined", async () => {
      const result = await testWeb(async () => {
        const { boolAttr } = await import("~/common/utils.js");
        return boolAttr(undefined);
      });
      expect(result).toBe(false);
    });

    it("returns false for non-empty string", async () => {
      const result = await testWeb(async () => {
        const { boolAttr } = await import("~/common/utils.js");
        return boolAttr("true");
      });
      expect(result).toBe(false);
    });
  });

  describe("groupTracksPerScheme", () => {
    it("groups tracks by URI scheme", async () => {
      const result = await testWeb(async () => {
        const { groupTracksPerScheme } = await import("~/common/utils.js");
        const tracks = [
          { $type: "sh.diffuse.output.track", id: "1", uri: "http://example.com/a.mp3" },
          { $type: "sh.diffuse.output.track", id: "2", uri: "s3://bucket/b.mp3" },
          { $type: "sh.diffuse.output.track", id: "3", uri: "http://example.com/c.mp3" },
        ] as never[];
        const groups = groupTracksPerScheme(tracks);
        return { httpCount: groups["http"].length, s3Count: groups["s3"].length };
      });
      expect(result.httpCount).toBe(2);
      expect(result.s3Count).toBe(1);
    });

    it("merges into provided initial object", async () => {
      const result = await testWeb(async () => {
        const { groupTracksPerScheme } = await import("~/common/utils.js");
        const initial = { http: [{ $type: "sh.diffuse.output.track", id: "0", uri: "http://existing.com/x.mp3" }] } as never;
        const tracks = [{ $type: "sh.diffuse.output.track", id: "1", uri: "http://example.com/a.mp3" }] as never[];
        const groups = groupTracksPerScheme(tracks, initial);
        return groups["http"].length;
      });
      expect(result).toBe(2);
    });
  });

  describe("groupUrisPerScheme", () => {
    it("groups URIs by scheme", async () => {
      const result = await testWeb(async () => {
        const { groupUrisPerScheme } = await import("~/common/utils.js");
        const groups = groupUrisPerScheme([
          "http://a.com/track.mp3",
          "s3://bucket/track.flac",
          "http://b.com/track.mp3",
        ]);
        return { httpCount: groups["http"].length, s3Count: groups["s3"].length };
      });
      expect(result.httpCount).toBe(2);
      expect(result.s3Count).toBe(1);
    });

    it("returns empty object for empty input", async () => {
      const result = await testWeb(async () => {
        const { groupUrisPerScheme } = await import("~/common/utils.js");
        return Object.keys(groupUrisPerScheme([])).length;
      });
      expect(result).toBe(0);
    });
  });

  describe("isPrimitive", () => {
    it("returns true for number", async () => {
      const result = await testWeb(async () => {
        const { isPrimitive } = await import("~/common/utils.js");
        return isPrimitive(42);
      });
      expect(result).toBe(true);
    });

    it("returns true for string", async () => {
      const result = await testWeb(async () => {
        const { isPrimitive } = await import("~/common/utils.js");
        return isPrimitive("hello");
      });
      expect(result).toBe(true);
    });

    it("returns true for boolean", async () => {
      const result = await testWeb(async () => {
        const { isPrimitive } = await import("~/common/utils.js");
        return isPrimitive(true);
      });
      expect(result).toBe(true);
    });

    it("returns true for null", async () => {
      const result = await testWeb(async () => {
        const { isPrimitive } = await import("~/common/utils.js");
        return isPrimitive(null);
      });
      expect(result).toBe(true);
    });

    it("returns false for object", async () => {
      const result = await testWeb(async () => {
        const { isPrimitive } = await import("~/common/utils.js");
        return isPrimitive({ a: 1 });
      });
      expect(result).toBe(false);
    });

    it("returns false for array", async () => {
      const result = await testWeb(async () => {
        const { isPrimitive } = await import("~/common/utils.js");
        return isPrimitive([1, 2]);
      });
      expect(result).toBe(false);
    });
  });

  describe("jsonEncode / jsonDecode", () => {
    it("round-trips a plain object", async () => {
      const result = await testWeb(async () => {
        const { jsonEncode, jsonDecode } = await import("~/common/utils.js");
        const original = { a: 1, b: "hello", c: [1, 2, 3] };
        return jsonDecode(jsonEncode(original));
      });
      expect(result).toEqual({ a: 1, b: "hello", c: [1, 2, 3] });
    });

    it("jsonEncode returns a Uint8Array", async () => {
      const result = await testWeb(async () => {
        const { jsonEncode } = await import("~/common/utils.js");
        return jsonEncode({ x: 1 }) instanceof Uint8Array;
      });
      expect(result).toBe(true);
    });
  });

  describe("hash", () => {
    it("returns a string", async () => {
      const result = await testWeb(async () => {
        const { hash } = await import("~/common/utils.js");
        return typeof hash({ a: 1 });
      });
      expect(result).toBe("string");
    });

    it("returns the same hash for equal objects", async () => {
      const result = await testWeb(async () => {
        const { hash } = await import("~/common/utils.js");
        return hash({ a: 1, b: 2 }) === hash({ a: 1, b: 2 });
      });
      expect(result).toBe(true);
    });

    it("returns different hashes for different objects", async () => {
      const result = await testWeb(async () => {
        const { hash } = await import("~/common/utils.js");
        return hash({ a: 1 }) === hash({ a: 2 });
      });
      expect(result).toBe(false);
    });
  });

  describe("removeUndefinedValuesFromRecord", () => {
    it("removes keys with undefined values", async () => {
      const result = await testWeb(async () => {
        const { removeUndefinedValuesFromRecord } = await import(
          "~/common/utils.js"
        );
        return removeUndefinedValuesFromRecord({ a: 1, b: undefined, c: "x" });
      });
      expect(result).toEqual({ a: 1, c: "x" });
    });

    it("does not mutate the original record", async () => {
      const result = await testWeb(async () => {
        const { removeUndefinedValuesFromRecord } = await import(
          "~/common/utils.js"
        );
        const original = { a: 1, b: undefined };
        removeUndefinedValuesFromRecord(original);
        return "b" in original;
      });
      expect(result).toBe(true);
    });

    it("returns record unchanged when no undefined values", async () => {
      const result = await testWeb(async () => {
        const { removeUndefinedValuesFromRecord } = await import(
          "~/common/utils.js"
        );
        return removeUndefinedValuesFromRecord({ a: 1, b: 2 });
      });
      expect(result).toEqual({ a: 1, b: 2 });
    });
  });

  describe("recursivelyCloneRecords", () => {
    it("shallow-clones a flat record", async () => {
      const result = await testWeb(async () => {
        const { recursivelyCloneRecords } = await import("~/common/utils.js");
        const original = { a: 1, b: "x" };
        const clone = recursivelyCloneRecords(original);
        return clone !== original && clone.a === 1 && clone.b === "x";
      });
      expect(result).toBe(true);
    });

    it("deep-clones nested objects", async () => {
      const result = await testWeb(async () => {
        const { recursivelyCloneRecords } = await import("~/common/utils.js");
        const original = { outer: { inner: 42 } };
        const clone = recursivelyCloneRecords(original);
        return clone.outer !== original.outer && clone.outer.inner === 42;
      });
      expect(result).toBe(true);
    });
  });

  describe("safeDecodeURIComponent", () => {
    it("decodes standard percent-encoded characters", async () => {
      const result = await testWeb(async () => {
        const { safeDecodeURIComponent } = await import("~/common/utils.js");
        return safeDecodeURIComponent("hello%20world");
      });
      expect(result).toBe("hello world");
    });

    it("decodes %u unicode escapes", async () => {
      const result = await testWeb(async () => {
        const { safeDecodeURIComponent } = await import("~/common/utils.js");
        return safeDecodeURIComponent("%u0041"); // 'A'
      });
      expect(result).toBe("A");
    });

    it("leaves plain strings unchanged", async () => {
      const result = await testWeb(async () => {
        const { safeDecodeURIComponent } = await import("~/common/utils.js");
        return safeDecodeURIComponent("plain-string");
      });
      expect(result).toBe("plain-string");
    });

    it("decodes mixed encoded and plain text", async () => {
      const result = await testWeb(async () => {
        const { safeDecodeURIComponent } = await import("~/common/utils.js");
        return safeDecodeURIComponent("hello%2Fworld");
      });
      expect(result).toBe("hello/world");
    });
  });
});
