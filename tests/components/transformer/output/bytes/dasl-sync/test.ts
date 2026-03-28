import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/transformer/output/bytes/dasl-sync", () => {
  describe("updateContainer", () => {
    it("returns an empty container for an empty collection", async () => {
      const result = await testWeb(async () => {
        const { CLASS } = await import(
          "~/components/transformer/output/bytes/dasl-sync/element.js"
        );
        const el = new CLASS();
        const EMPTY = {
          cid: undefined,
          data: [],
          inventory: { current: {}, removed: [] },
        };
        return await el.updateContainer({ previous: EMPTY, collection: [] });
      });

      expect(result.data).toEqual([]);
      expect(result.inventory.current).toEqual({});
      expect(result.inventory.removed).toEqual([]);
    });

    it("assigns CIDs to newly added items", async () => {
      const result = await testWeb(async () => {
        const { CLASS } = await import(
          "~/components/transformer/output/bytes/dasl-sync/element.js"
        );
        const el = new CLASS();
        const EMPTY = {
          cid: undefined,
          data: [],
          inventory: { current: {}, removed: [] },
        };
        const items = [
          { id: "a", updatedAt: "2024-01-01T00:00:00Z" },
          { id: "b", updatedAt: "2024-01-02T00:00:00Z" },
        ];
        const c = await el.updateContainer({ previous: EMPTY, collection: items });
        return {
          dataLength: c.data.length,
          currentKeys: Object.keys(c.inventory.current),
          removed: c.inventory.removed,
          cidType: typeof c.cid,
          cidA: typeof c.inventory.current["a"],
          cidB: typeof c.inventory.current["b"],
        };
      });

      expect(result.dataLength).toBe(2);
      expect(result.currentKeys.sort()).toEqual(["a", "b"]);
      expect(result.removed).toEqual([]);
      expect(result.cidType).toBe("string");
      expect(result.cidA).toBe("string");
      expect(result.cidB).toBe("string");
    });

    it("moves removed items from current to removed list", async () => {
      const result = await testWeb(async () => {
        const { CLASS } = await import(
          "~/components/transformer/output/bytes/dasl-sync/element.js"
        );
        const el = new CLASS();
        const EMPTY = {
          cid: undefined,
          data: [],
          inventory: { current: {}, removed: [] },
        };
        const items = [
          { id: "a", updatedAt: "2024-01-01T00:00:00Z" },
          { id: "b", updatedAt: "2024-01-01T00:00:00Z" },
        ];
        const first = await el.updateContainer({
          previous: EMPTY,
          collection: items,
        });
        const second = await el.updateContainer({
          previous: first,
          collection: [items[1]],
        });
        return {
          dataLength: second.data.length,
          dataId: second.data[0]?.id,
          hasA: "a" in second.inventory.current,
          removed: second.inventory.removed,
        };
      });

      expect(result.dataLength).toBe(1);
      expect(result.dataId).toBe("b");
      expect(result.hasA).toBe(false);
      expect(result.removed).toContain("a");
    });

    it("preserves existing CIDs for unchanged items", async () => {
      const result = await testWeb(async () => {
        const { CLASS } = await import(
          "~/components/transformer/output/bytes/dasl-sync/element.js"
        );
        const el = new CLASS();
        const EMPTY = {
          cid: undefined,
          data: [],
          inventory: { current: {}, removed: [] },
        };
        const items = [{ id: "a", updatedAt: "2024-01-01T00:00:00Z" }];
        const first = await el.updateContainer({
          previous: EMPTY,
          collection: items,
        });
        const second = await el.updateContainer({
          previous: first,
          collection: items,
        });
        return {
          cidFirst: first.inventory.current["a"],
          cidSecond: second.inventory.current["a"],
        };
      });

      expect(result.cidFirst).toBe(result.cidSecond);
    });

    it("accumulates removed items across multiple updates", async () => {
      const result = await testWeb(async () => {
        const { CLASS } = await import(
          "~/components/transformer/output/bytes/dasl-sync/element.js"
        );
        const el = new CLASS();
        const EMPTY = {
          cid: undefined,
          data: [],
          inventory: { current: {}, removed: [] },
        };
        const items = [
          { id: "a", updatedAt: "2024-01-01T00:00:00Z" },
          { id: "b", updatedAt: "2024-01-01T00:00:00Z" },
          { id: "c", updatedAt: "2024-01-01T00:00:00Z" },
        ];
        const first = await el.updateContainer({
          previous: EMPTY,
          collection: items,
        });
        const second = await el.updateContainer({
          previous: first,
          collection: [items[1], items[2]],
        });
        const third = await el.updateContainer({
          previous: second,
          collection: [items[2]],
        });
        return third.inventory.removed;
      });

      expect(result).toContain("a");
      expect(result).toContain("b");
    });
  });

  describe("hasDiverged", () => {
    it("returns false when CIDs are equal", async () => {
      const result = await testWeb(async () => {
        const { CLASS } = await import(
          "~/components/transformer/output/bytes/dasl-sync/element.js"
        );
        const el = new CLASS();
        return el.hasDiverged({
          local: { cid: "bafyabc", data: [], inventory: { current: {}, removed: [] } },
          remote: { cid: "bafyabc", data: [], inventory: { current: {}, removed: [] } },
        });
      });

      expect(result).toBe(false);
    });

    it("returns true when CIDs differ", async () => {
      const result = await testWeb(async () => {
        const { CLASS } = await import(
          "~/components/transformer/output/bytes/dasl-sync/element.js"
        );
        const el = new CLASS();
        return el.hasDiverged({
          local: { cid: "bafyabc", data: [], inventory: { current: {}, removed: [] } },
          remote: { cid: "bafyxyz", data: [], inventory: { current: {}, removed: [] } },
        });
      });

      expect(result).toBe(true);
    });

    it("returns false when both CIDs are undefined", async () => {
      const result = await testWeb(async () => {
        const { CLASS } = await import(
          "~/components/transformer/output/bytes/dasl-sync/element.js"
        );
        const el = new CLASS();
        return el.hasDiverged({
          local: { cid: undefined, data: [], inventory: { current: {}, removed: [] } },
          remote: { cid: undefined, data: [], inventory: { current: {}, removed: [] } },
        });
      });

      expect(result).toBe(false);
    });

    it("returns true when one CID is undefined", async () => {
      const result = await testWeb(async () => {
        const { CLASS } = await import(
          "~/components/transformer/output/bytes/dasl-sync/element.js"
        );
        const el = new CLASS();
        return el.hasDiverged({
          local: { cid: "bafyabc", data: [], inventory: { current: {}, removed: [] } },
          remote: { cid: undefined, data: [], inventory: { current: {}, removed: [] } },
        });
      });

      expect(result).toBe(true);
    });
  });

  describe("merge", () => {
    it("includes non-overlapping items from both containers", async () => {
      const result = await testWeb(async () => {
        const { CLASS } = await import(
          "~/components/transformer/output/bytes/dasl-sync/element.js"
        );
        const el = new CLASS();
        const containerA = {
          cid: "cid-a",
          data: [{ id: "a", updatedAt: "2024-01-01T00:00:00Z" }],
          inventory: { current: { a: "cid-item-a" }, removed: [] },
        };
        const containerB = {
          cid: "cid-b",
          data: [{ id: "b", updatedAt: "2024-01-01T00:00:00Z" }],
          inventory: { current: { b: "cid-item-b" }, removed: [] },
        };
        const merged = await el.merge(containerA, containerB);
        return { ids: merged.data.map((i: { id: string }) => i.id).sort(), removed: merged.inventory.removed };
      });

      expect(result.ids).toEqual(["a", "b"]);
      expect(result.removed).toEqual([]);
    });

    it("respects items removed on side A", async () => {
      const result = await testWeb(async () => {
        const { CLASS } = await import(
          "~/components/transformer/output/bytes/dasl-sync/element.js"
        );
        const el = new CLASS();
        const itemA = { id: "a", updatedAt: "2024-01-01T00:00:00Z" };
        const itemB = { id: "b", updatedAt: "2024-01-01T00:00:00Z" };
        // A has removed "b"
        const containerA = {
          cid: "cid-a",
          data: [itemA],
          inventory: { current: { a: "cid-item-a" }, removed: ["b"] },
        };
        // B still has "b"
        const containerB = {
          cid: "cid-b",
          data: [itemA, itemB],
          inventory: { current: { a: "cid-item-a", b: "cid-item-b" }, removed: [] },
        };
        const merged = await el.merge(containerA, containerB);
        return { ids: merged.data.map((i: { id: string }) => i.id), removed: merged.inventory.removed };
      });

      expect(result.ids).not.toContain("b");
      expect(result.removed).toContain("b");
    });

    it("respects items removed on side B", async () => {
      const result = await testWeb(async () => {
        const { CLASS } = await import(
          "~/components/transformer/output/bytes/dasl-sync/element.js"
        );
        const el = new CLASS();
        const itemA = { id: "a", updatedAt: "2024-01-01T00:00:00Z" };
        const itemB = { id: "b", updatedAt: "2024-01-01T00:00:00Z" };
        // A still has "b"
        const containerA = {
          cid: "cid-a",
          data: [itemA, itemB],
          inventory: { current: { a: "cid-item-a", b: "cid-item-b" }, removed: [] },
        };
        // B has removed "b"
        const containerB = {
          cid: "cid-b",
          data: [itemA],
          inventory: { current: { a: "cid-item-a" }, removed: ["b"] },
        };
        const merged = await el.merge(containerA, containerB);
        return { ids: merged.data.map((i: { id: string }) => i.id), removed: merged.inventory.removed };
      });

      expect(result.ids).not.toContain("b");
      expect(result.removed).toContain("b");
    });

    it("uses the newer item when the same item conflicts", async () => {
      const result = await testWeb(async () => {
        const { CLASS } = await import(
          "~/components/transformer/output/bytes/dasl-sync/element.js"
        );
        const el = new CLASS();
        const oldItem = { id: "a", updatedAt: "2024-01-01T00:00:00Z", name: "Old" };
        const newItem = { id: "a", updatedAt: "2024-06-01T00:00:00Z", name: "New" };
        const containerA = {
          cid: "cid-a",
          data: [oldItem],
          inventory: { current: { a: "cid-old" }, removed: [] },
        };
        const containerB = {
          cid: "cid-b",
          data: [newItem],
          inventory: { current: { a: "cid-new" }, removed: [] },
        };
        const merged = await el.merge(containerA, containerB);
        return (merged.data[0] as { name: string }).name;
      });

      // The newer item ("New") should win the conflict
      expect(result).toBe("New");
    });

    it("keeps identical items without recomputing their CIDs", async () => {
      const result = await testWeb(async () => {
        const { CLASS } = await import(
          "~/components/transformer/output/bytes/dasl-sync/element.js"
        );
        const el = new CLASS();
        const item = { id: "a", updatedAt: "2024-01-01T00:00:00Z" };
        const container = {
          cid: "cid-a",
          data: [item],
          inventory: { current: { a: "original-cid" }, removed: [] },
        };
        const merged = await el.merge(container, container);
        return merged.inventory.current["a"];
      });

      expect(result).toBe("original-cid");
    });

    it("combines removed lists from both containers", async () => {
      const result = await testWeb(async () => {
        const { CLASS } = await import(
          "~/components/transformer/output/bytes/dasl-sync/element.js"
        );
        const el = new CLASS();
        const containerA = {
          cid: "cid-a",
          data: [],
          inventory: { current: {}, removed: ["x"] },
        };
        const containerB = {
          cid: "cid-b",
          data: [],
          inventory: { current: {}, removed: ["y"] },
        };
        const merged = await el.merge(containerA, containerB);
        return merged.inventory.removed;
      });

      expect(result).toContain("x");
      expect(result).toContain("y");
    });

    it("produces a deterministic CID from the merged inventory", async () => {
      const result = await testWeb(async () => {
        const { CLASS } = await import(
          "~/components/transformer/output/bytes/dasl-sync/element.js"
        );
        const el = new CLASS();
        const containerA = {
          cid: "cid-a",
          data: [{ id: "a", updatedAt: "2024-01-01T00:00:00Z" }],
          inventory: { current: { a: "cid-item-a" }, removed: [] },
        };
        const containerB = {
          cid: "cid-b",
          data: [{ id: "b", updatedAt: "2024-01-01T00:00:00Z" }],
          inventory: { current: { b: "cid-item-b" }, removed: [] },
        };
        const merge1 = await el.merge(containerA, containerB);
        const merge2 = await el.merge(containerA, containerB);
        return { cid1: merge1.cid, cid2: merge2.cid };
      });

      expect(result.cid1).toBe(result.cid2);
      expect(typeof result.cid1).toBe("string");
    });
  });

  describe("save", () => {
    it("round-trips a container through CBOR encode/decode", async () => {
      const result = await testWeb(async () => {
        const { decode } = await import("@atcute/cbor");
        const { CLASS } = await import(
          "~/components/transformer/output/bytes/dasl-sync/element.js"
        );
        const el = new CLASS();
        const container = {
          cid: "bafyabc",
          data: [{ id: "a", updatedAt: "2024-01-01T00:00:00Z", value: 42 }],
          inventory: { current: { a: "cid-a" }, removed: [] },
        };
        const bytes = el.save(container);
        return decode(bytes);
      });

      expect(result.cid).toBe("bafyabc");
      expect(result.data.length).toBe(1);
      expect(result.data[0].id).toBe("a");
      expect(result.data[0].value).toBe(42);
      expect(result.inventory.current).toEqual({ a: "cid-a" });
    });

    it("returns a Uint8Array", async () => {
      const result = await testWeb(async () => {
        const { CLASS } = await import(
          "~/components/transformer/output/bytes/dasl-sync/element.js"
        );
        const el = new CLASS();
        const container = {
          cid: undefined,
          data: [],
          inventory: { current: {}, removed: [] },
        };
        const bytes = el.save(container);
        return bytes instanceof Uint8Array;
      });

      expect(result).toBe(true);
    });
  });
});
