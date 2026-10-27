import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/engine/scope", () => {
  it("has default sortBy of tag fields", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      return engine.sortBy();
    });

    expect(result).toEqual(["tags.artist", "tags.album", "tags.disc.no", "tags.track.no"]);
  });

  it("has default sortDirection of 'asc'", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      return engine.sortDirection();
    });

    expect(result).toBe("asc");
  });

  it("has default groupBy of undefined", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      return engine.groupBy() ?? null;
    });

    expect(result).toBe(null);
  });

  it("has default playlist of undefined", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      return engine.playlist() ?? null;
    });

    expect(result).toBe(null);
  });

  it("has default searchTerm of undefined", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      return engine.searchTerm() ?? null;
    });

    expect(result).toBe(null);
  });

  it("has default selection of undefined", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      return engine.selection() ?? null;
    });

    expect(result).toBe(null);
  });

  it("setGroupBy updates groupBy", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setGroupBy("artist");
      return engine.groupBy();
    });

    expect(result).toBe("artist");
  });

  it("setGroupBy with undefined clears groupBy", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setGroupBy("artist");
      await engine.setGroupBy(undefined);
      return engine.groupBy() ?? null;
    });

    expect(result).toBe(null);
  });

  it("setPlaylist updates playlist", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setPlaylist("playlist-123");
      return engine.playlist();
    });

    expect(result).toBe("playlist-123");
  });

  it("setSearchTerm updates searchTerm", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setSearchTerm("hello world");
      return engine.searchTerm();
    });

    expect(result).toBe("hello world");
  });

  it("setSelection updates selection", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setSelection([
        { field: "tags.album", value: "blue", transformations: ["toLowerCase"] },
      ]);
      return engine.selection();
    });

    expect(result).toEqual([
      { field: "tags.album", value: "blue", transformations: ["toLowerCase"] },
    ]);
  });

  it("setSelection with undefined clears selection", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setSelection([{ field: "tags.album", value: "blue" }]);
      await engine.setSelection(undefined);
      return engine.selection() ?? null;
    });

    expect(result).toBe(null);
  });

  it("setSortBy updates sortBy", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setSortBy(["title", "artist"]);
      return engine.sortBy();
    });

    expect(result).toEqual(["title", "artist"]);
  });

  it("setSortDirection updates sortDirection", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setSortDirection("asc");
      return engine.sortDirection();
    });

    expect(result).toBe("asc");
  });

  it("revertToDefaultSort resets sortBy and sortDirection", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setSortBy(["title"]);
      await engine.setSortDirection("asc");
      await engine.revertToDefaultSort();
      return { sortBy: engine.sortBy(), sortDirection: engine.sortDirection() };
    });

    expect(result.sortBy).toEqual(["tags.artist", "tags.album", "tags.disc.no", "tags.track.no"]);
    expect(result.sortDirection).toBe("asc");
  });

  it("persists groupBy to localStorage", async () => {
    const stored = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setGroupBy("album");

      for (let i = 0; i < localStorage.length; i++) {
        const key = localStorage.key(i)!;
        if (key.includes("scope") && key.endsWith("/groupBy")) {
          return localStorage.getItem(key);
        }
      }
      return null;
    });

    expect(stored).toBe("album");
  });

  it("persists sortBy to localStorage as JSON", async () => {
    const stored = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setSortBy(["title", "artist"]);

      for (let i = 0; i < localStorage.length; i++) {
        const key = localStorage.key(i)!;
        if (key.includes("scope") && key.endsWith("/sortBy")) {
          return localStorage.getItem(key);
        }
      }
      return null;
    });

    expect(stored).toBe('["title","artist"]');
  });

  it("persists selection to localStorage as JSON", async () => {
    const stored = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setSelection([{ field: "tags.album", value: "blue" }]);

      for (let i = 0; i < localStorage.length; i++) {
        const key = localStorage.key(i)!;
        if (key.includes("scope") && key.endsWith("/selection")) {
          return localStorage.getItem(key);
        }
      }
      return null;
    });

    expect(stored).toBe('[{"field":"tags.album","value":"blue"}]');
  });

  it("restores values from localStorage on connect", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/scope/element.js");

      // Set state with first engine instance
      const engine1 = new mod.CLASS();
      document.body.append(engine1);
      await engine1.setGroupBy("artist");
      await engine1.setSearchTerm("test");
      await engine1.setSelection([{ field: "tags.album", value: "blue" }]);
      await engine1.setSortBy(["title"]);
      await engine1.setSortDirection("asc");

      // Second instance reads same localStorage keys
      const engine2 = new mod.CLASS();
      document.body.append(engine2);
      return {
        groupBy: engine2.groupBy(),
        searchTerm: engine2.searchTerm(),
        selection: engine2.selection(),
        sortBy: engine2.sortBy(),
        sortDirection: engine2.sortDirection(),
      };
    });

    expect(result.groupBy).toBe("artist");
    expect(result.searchTerm).toBe("test");
    expect(result.selection).toEqual([{ field: "tags.album", value: "blue" }]);
    expect(result.sortBy).toEqual(["title"]);
    expect(result.sortDirection).toBe("asc");
  });
});
