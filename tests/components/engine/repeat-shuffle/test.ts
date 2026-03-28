import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/engine/repeat-shuffle", () => {
  it("defaults to false for both repeat and shuffle", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/engine/repeat-shuffle/element.js"
      );
      const engine = new mod.CLASS();
      document.body.append(engine);
      return { repeat: engine.repeat(), shuffle: engine.shuffle() };
    });

    expect(result.repeat).toBe(false);
    expect(result.shuffle).toBe(false);
  });

  it("setRepeat sets repeat to true", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/engine/repeat-shuffle/element.js"
      );
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setRepeat(true);
      return engine.repeat();
    });

    expect(result).toBe(true);
  });

  it("setRepeat sets repeat back to false", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/engine/repeat-shuffle/element.js"
      );
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setRepeat(true);
      await engine.setRepeat(false);
      return engine.repeat();
    });

    expect(result).toBe(false);
  });

  it("setShuffle sets shuffle to true", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/engine/repeat-shuffle/element.js"
      );
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setShuffle(true);
      return engine.shuffle();
    });

    expect(result).toBe(true);
  });

  it("setShuffle sets shuffle back to false", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/engine/repeat-shuffle/element.js"
      );
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setShuffle(true);
      await engine.setShuffle(false);
      return engine.shuffle();
    });

    expect(result).toBe(false);
  });

  it("repeat and shuffle are independent", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/engine/repeat-shuffle/element.js"
      );
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setRepeat(true);
      return { repeat: engine.repeat(), shuffle: engine.shuffle() };
    });

    expect(result.repeat).toBe(true);
    expect(result.shuffle).toBe(false);
  });

  it("persists repeat to localStorage", async () => {
    const stored = await testWeb(async () => {
      const mod = await import(
        "~/components/engine/repeat-shuffle/element.js"
      );
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setRepeat(true);

      // Find the key by iterating localStorage
      for (let i = 0; i < localStorage.length; i++) {
        const key = localStorage.key(i)!;
        if (key.includes("repeat-shuffle") && key.endsWith("/repeat")) {
          return localStorage.getItem(key);
        }
      }
      return null;
    });

    expect(stored).toBe("true");
  });

  it("persists shuffle to localStorage", async () => {
    const stored = await testWeb(async () => {
      const mod = await import(
        "~/components/engine/repeat-shuffle/element.js"
      );
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setShuffle(true);

      for (let i = 0; i < localStorage.length; i++) {
        const key = localStorage.key(i)!;
        if (key.includes("repeat-shuffle") && key.endsWith("/shuffle")) {
          return localStorage.getItem(key);
        }
      }
      return null;
    });

    expect(stored).toBe("true");
  });

  it("restores repeat state from localStorage on connect", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/engine/repeat-shuffle/element.js"
      );

      // Set state with first engine instance
      const engine1 = new mod.CLASS();
      document.body.append(engine1);
      await engine1.setRepeat(true);

      // Second instance reads same localStorage key
      const engine2 = new mod.CLASS();
      document.body.append(engine2);
      return engine2.repeat();
    });

    expect(result).toBe(true);
  });

  it("restores shuffle state from localStorage on connect", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/engine/repeat-shuffle/element.js"
      );

      // Set state with first engine instance
      const engine1 = new mod.CLASS();
      document.body.append(engine1);
      await engine1.setShuffle(true);

      // Second instance reads same localStorage key
      const engine2 = new mod.CLASS();
      document.body.append(engine2);
      return engine2.shuffle();
    });

    expect(result).toBe(true);
  });
});
