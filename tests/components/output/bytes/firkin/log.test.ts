// @ts-nocheck -- the fake client intentionally implements only the subset it uses

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import * as DagCBOR from "@ipld/dag-cbor";
import { ApiError } from "@tokono.ma/firkin";
import { cidForBytes } from "~/vendor/firkin/index.js";

import {
  logName,
  registryNamespace,
} from "~/components/output/bytes/firkin/constants.js";
import {
  appendEntry,
  countEntries,
  latestEntry,
  readEntries,
  replaceLog,
} from "~/components/output/bytes/firkin/log.js";

/**
 * The subset of the Firkin client the log helpers use, backed by maps.
 */
function fakeClient() {
  /** @type {Map<string, Uint8Array>} */
  const blobs = new Map();
  /** @type {Map<string, Map<string, string>>} */
  const registry = new Map();

  const namespace = (/** @type {string} */ ns) => {
    const stored = registry.get(ns);
    if (stored) return stored;

    const created = new Map();
    registry.set(ns, created);
    return created;
  };

  const client = {
    blobs,
    registry,

    /**
     * @param {Uint8Array} bytes
     * @param {{ codec?: number }} [opts]
     */
    async upload(bytes, opts = {}) {
      const cid = cidForBytes(bytes, { codec: opts.codec ?? 0x55 });
      const deduplicated = blobs.has(cid);
      blobs.set(cid, bytes);
      return { cid, deduplicated };
    },

    /** @param {string} cid */
    async get(cid) {
      const stored = blobs.get(cid);
      if (!stored) throw new ApiError("Not found", 404);

      return stored;
    },

    /**
     * @param {string} ns
     * @param {string} name
     * @param {string} cid
     */
    async registrySet(ns, name, cid) {
      namespace(ns).set(name, cid);
    },

    /** @param {string} ns; @param {string} name */
    async registryGet(ns, name) {
      return namespace(ns).get(name) ?? null;
    },

    /** @param {string} ns */
    async registryNames(ns) {
      const stored = namespace(ns);
      if (stored.size === 0) return null;

      return Object.fromEntries(stored);
    },
  };

  return /** @type {any} */ (client);
}

/** @param {string} text */
const bytes = (text) => new TextEncoder().encode(text);

/** @param {Uint8Array} value */
const text = (value) => new TextDecoder().decode(value);

describe("components/output/bytes/firkin/log", () => {
  it("unions the entries of every writer", async () => {
    const rb = fakeClient();

    await appendEntry(rb, "ns", "tracks", "device-a", bytes("a"));
    await appendEntry(rb, "ns", "tracks", "device-b", bytes("b"));

    const entries = await readEntries(rb, "ns", "tracks");

    expect(entries.length).toBe(2);
    expect(
      entries.map((entry) => text(rb.blobs.get(entry.cid))).sort(),
    ).toEqual(["a", "b"]);

    expect(await countEntries(rb, "ns", "tracks", "device-a")).toBe(1);
    expect(await countEntries(rb, "ns", "tracks", "device-b")).toBe(1);
  });

  it("returns the entry that was appended last", async () => {
    const rb = fakeClient();

    await appendEntry(rb, "ns", "tracks", "device-a", bytes("old"));
    const newest = await appendEntry(rb, "ns", "tracks", "device-a", bytes("new"));

    const entry = await latestEntry(rb, "ns", "tracks");

    expect(entry?.cid).toBe(newest.cid);
    expect(text(rb.blobs.get(entry.cid))).toBe("new");
  });

  it("returns nothing for an empty log", async () => {
    const rb = fakeClient();

    expect(await latestEntry(rb, "ns", "tracks")).toBe(undefined);
    expect(await readEntries(rb, "ns", "tracks")).toEqual([]);
  });

  it("does not append identical bytes twice", async () => {
    const rb = fakeClient();

    const first = await appendEntry(rb, "ns", "tracks", "device-a", bytes("same"));
    const second = await appendEntry(
      rb,
      "ns",
      "tracks",
      "device-a",
      bytes("same"),
    );

    expect(first.appended).toBe(true);
    expect(second.appended).toBe(false);
    expect(second.cid).toBe(first.cid);
    expect((await readEntries(rb, "ns", "tracks")).map((e) => e.cid)).toEqual([
      first.cid,
    ]);
  });

  it("keeps another writer's entries when replacing a log", async () => {
    const rb = fakeClient();

    await appendEntry(rb, "ns", "tracks", "device-a", bytes("1"));
    await appendEntry(rb, "ns", "tracks", "device-a", bytes("2"));
    const other = await appendEntry(rb, "ns", "tracks", "device-b", bytes("b"));

    // `at` has millisecond resolution and "newest" is arbitrary between
    // writers that append in the same millisecond.
    await new Promise((resolve) => setTimeout(resolve, 2));

    const snapshot = await replaceLog(rb, "ns", "tracks", "device-a", bytes("all"));

    expect(await countEntries(rb, "ns", "tracks", "device-a")).toBe(1);
    expect((await readEntries(rb, "ns", "tracks")).map((e) => e.cid).sort())
      .toEqual([snapshot, other.cid].sort());
    // The snapshot is the newest entry, so `get` sees the compacted value.
    expect((await latestEntry(rb, "ns", "tracks")).cid).toBe(snapshot);
  });

  it("keeps logs separate per collection", async () => {
    const rb = fakeClient();

    await appendEntry(rb, "ns", "tracks", "device-a", bytes("t"));
    await appendEntry(rb, "ns", "facets", "device-a", bytes("f"));

    expect((await readEntries(rb, "ns", "tracks")).length).toBe(1);
    expect((await readEntries(rb, "ns", "facets")).length).toBe(1);
    expect(await readEntries(rb, "ns", "settings")).toEqual([]);
  });

  it("trims a log to its newest entries when a limit is set", async () => {
    const rb = fakeClient();

    await appendEntry(rb, "ns", "tracks", "device-a", bytes("1"), { keep: 2 });
    await appendEntry(rb, "ns", "tracks", "device-a", bytes("2"), { keep: 2 });
    await appendEntry(rb, "ns", "tracks", "device-a", bytes("3"), { keep: 2 });

    const entries = await readEntries(rb, "ns", "tracks");

    expect(entries.map((entry) => text(rb.blobs.get(entry.cid)))).toEqual([
      "2",
      "3",
    ]);
    expect(await countEntries(rb, "ns", "tracks", "device-a")).toBe(2);
    expect(text(rb.blobs.get((await latestEntry(rb, "ns", "tracks")).cid)))
      .toBe("3");
  });

  it("trims each writer's log independently", async () => {
    const rb = fakeClient();

    await appendEntry(rb, "ns", "tracks", "device-a", bytes("a1"), { keep: 1 });
    await appendEntry(rb, "ns", "tracks", "device-a", bytes("a2"), { keep: 1 });
    await appendEntry(rb, "ns", "tracks", "device-b", bytes("b1"), { keep: 1 });

    const entries = await readEntries(rb, "ns", "tracks");

    expect(entries.map((entry) => text(rb.blobs.get(entry.cid))).sort())
      .toEqual(["a2", "b1"]);
  });

  it("keeps everything when the limit is not set", async () => {
    const rb = fakeClient();

    await appendEntry(rb, "ns", "tracks", "device-a", bytes("1"));
    await appendEntry(rb, "ns", "tracks", "device-a", bytes("2"));

    expect(await countEntries(rb, "ns", "tracks", "device-a")).toBe(2);
  });

  it("treats a manifest with a missing blob as an empty log", async () => {
    const rb = fakeClient();

    await appendEntry(rb, "ns", "tracks", "device-a", bytes("a"));

    const manifestCid = rb.registry
      .get(registryNamespace("ns"))
      .get(logName("tracks", "device-a"));
    rb.blobs.delete(manifestCid);

    expect(await readEntries(rb, "ns", "tracks")).toEqual([]);
  });

  it("reads a manifest written before entries were stamped", async () => {
    const rb = fakeClient();
    const { cid } = await rb.upload(bytes("legacy"));

    // Shape written by the first build of this element.
    const manifest = await rb.upload(
      DagCBOR.encode({ v: 1, packs: [cid] }),
    );
    await rb.registrySet(
      registryNamespace("ns"),
      logName("tracks", "device-a"),
      manifest.cid,
    );

    const entries = await readEntries(rb, "ns", "tracks");

    expect(entries).toEqual([{ cid, at: 0 }]);
  });
});
