import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("common/temporal", () => {
  describe("compareTimestamps", () => {
    it("returns 0 for equal timestamps", async () => {
      const result = await testWeb(async () => {
        const { compareTimestamps } = await import("~/common/temporal.js");
        return compareTimestamps(
          "2024-01-01T00:00:00.000Z",
          "2024-01-01T00:00:00.000Z",
        );
      });
      expect(result).toBe(0);
    });

    it("returns negative when first is earlier", async () => {
      const result = await testWeb(async () => {
        const { compareTimestamps } = await import("~/common/temporal.js");
        return compareTimestamps(
          "2024-01-01T00:00:00.000Z",
          "2024-06-01T00:00:00.000Z",
        );
      });
      expect(result).toBeLessThan(0);
    });

    it("returns positive when first is later", async () => {
      const result = await testWeb(async () => {
        const { compareTimestamps } = await import("~/common/temporal.js");
        return compareTimestamps(
          "2024-06-01T00:00:00.000Z",
          "2024-01-01T00:00:00.000Z",
        );
      });
      expect(result).toBeGreaterThan(0);
    });

    it("handles millisecond precision", async () => {
      const result = await testWeb(async () => {
        const { compareTimestamps } = await import("~/common/temporal.js");
        return compareTimestamps(
          "2024-01-01T00:00:00.001Z",
          "2024-01-01T00:00:00.000Z",
        );
      });
      expect(result).toBeGreaterThan(0);
    });
  });
});
