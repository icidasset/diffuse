import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("common/signal", () => {
  describe("signal", () => {
    it("get returns the initial value", async () => {
      const result = await testWeb(async () => {
        const { signal } = await import("~/common/signal.js");
        const s = signal(42);
        return s.get();
      });
      expect(result).toBe(42);
    });

    it("value getter returns the initial value", async () => {
      const result = await testWeb(async () => {
        const { signal } = await import("~/common/signal.js");
        const s = signal("hello");
        return s.value;
      });
      expect(result).toBe("hello");
    });

    it("set updates the value", async () => {
      const result = await testWeb(async () => {
        const { signal } = await import("~/common/signal.js");
        const s = signal(1);
        s.set(99);
        return s.get();
      });
      expect(result).toBe(99);
    });

    it("value setter updates the value", async () => {
      const result = await testWeb(async () => {
        const { signal } = await import("~/common/signal.js");
        const s = signal("a");
        s.value = "b";
        return s.value;
      });
      expect(result).toBe("b");
    });

    it("with compare option skips update when values are equal", async () => {
      const result = await testWeb(async () => {
        const { signal, effect } = await import("~/common/signal.js");
        let runCount = 0;
        const s = signal({ x: 1 }, { compare: (a, b) => a.x === b.x });

        effect(() => {
          s.get();
          runCount++;
        });

        const before = runCount;
        s.set({ x: 1 }); // same by compare
        return { before, after: runCount };
      });
      expect(result.before).toBe(1);
      expect(result.after).toBe(1); // effect not re-run
    });

    it("with compare option triggers update when values differ", async () => {
      const result = await testWeb(async () => {
        const { signal, effect } = await import("~/common/signal.js");
        let runCount = 0;
        const s = signal({ x: 1 }, { compare: (a, b) => a.x === b.x });

        effect(() => {
          s.get();
          runCount++;
        });

        const before = runCount;
        s.set({ x: 2 }); // different by compare
        return { before, after: runCount };
      });
      expect(result.before).toBe(1);
      expect(result.after).toBe(2);
    });
  });

  describe("batch", () => {
    it("defers effect execution until batch completes", async () => {
      const result = await testWeb(async () => {
        const { signal, effect, batch } = await import("~/common/signal.js");
        const a = signal(0);
        const b = signal(0);
        const values: number[] = [];

        effect(() => {
          values.push(a.get() + b.get());
        });

        const before = [...values]; // [0]
        batch(() => {
          a.set(1);
          b.set(2);
        });

        return { before, after: values };
      });
      expect(result.before).toEqual([0]);
      expect(result.after).toEqual([0, 3]); // only one update, not two
    });
  });

  describe("untracked", () => {
    it("reads signal value without tracking it as a dependency", async () => {
      const result = await testWeb(async () => {
        const { signal, effect, untracked } = await import(
          "~/common/signal.js"
        );
        const a = signal(1);
        const b = signal(10);
        let runCount = 0;

        effect(() => {
          a.get(); // tracked
          untracked(() => b.get()); // not tracked
          runCount++;
        });

        const before = runCount; // 1
        b.set(20); // should NOT re-run effect
        const afterB = runCount;
        a.set(2); // SHOULD re-run effect
        return { before, afterB, afterA: runCount };
      });
      expect(result.before).toBe(1);
      expect(result.afterB).toBe(1);
      expect(result.afterA).toBe(2);
    });

    it("returns the value from the callback", async () => {
      const result = await testWeb(async () => {
        const { signal, untracked } = await import("~/common/signal.js");
        const s = signal(42);
        return untracked(() => s.get());
      });
      expect(result).toBe(42);
    });
  });

  describe("untrackedAsync", () => {
    it("returns the resolved value", async () => {
      const result = await testWeb(async () => {
        const { untrackedAsync } = await import("~/common/signal.js");
        return untrackedAsync(async () => 99);
      });
      expect(result).toBe(99);
    });
  });
});
