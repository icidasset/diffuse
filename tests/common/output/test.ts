import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("common/output", () => {
  describe("data", () => {
    it("resolves immediately when collection is already loaded", async () => {
      const result = await testWeb(async () => {
        const { data } = await import("~/common/output.js");
        const { signal } = await import("~/common/signal.js");

        const col = signal<{ state: "loading" } | { state: "loaded"; data: string[] }>(
          { state: "loaded", data: ["a", "b"] },
        );

        return data({ collection: col.get });
      });
      expect(result).toEqual(["a", "b"]);
    });

    it("waits for collection to become loaded", async () => {
      const result = await testWeb(async () => {
        const { data } = await import("~/common/output.js");
        const { signal } = await import("~/common/signal.js");

        const col = signal<{ state: "loading" } | { state: "loaded"; data: number[] }>(
          { state: "loading" },
        );

        const promise = data({ collection: col.get });

        // Transition to loaded after a microtask
        await Promise.resolve();
        col.set({ state: "loaded", data: [1, 2, 3] });

        return promise;
      });
      expect(result).toEqual([1, 2, 3]);
    });
  });
});
