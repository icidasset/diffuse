// @ts-nocheck -- the test builds Loro documents directly

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import {
  actorOf,
  adoptDocument,
  applyIncremental,
  applyLog,
  applyPack,
  changeDocument,
  changeDocumentBatched,
  changesSince,
  cloneDocument,
  createDocument,
  encodeDocument,
  hasRecords,
  headsOf,
  loadDocument,
  mergeDocument,
  mergeDocuments,
  recordsOf,
} from "~/components/output/raw/firkin/encrypted/incremental-loro/doc.js";

/** @param {string} id */
const track = (id) => ({ $type: "sh.diffuse.output.track", id, uri: `https://a/${id}` });

describe("components/output/raw/firkin/encrypted/incremental-loro/doc", () => {
  it("round-trips a collection through a document", () => {
    const items = [track("a"), track("b")];
    const { doc, changed } = changeDocument(createDocument(), items, {
      schema: "sh.diffuse.output.track",
    });

    expect(changed).toBe(true);
    expect(recordsOf(doc)).toEqual(items);

    const reloaded = loadDocument(encodeDocument(doc));
    expect(recordsOf(reloaded)).toEqual(items);
  });

  it("applies a large collection in one change", async () => {
    const items = Array.from({ length: 1_200 }, (_, index) =>
      track(`t${index}`)
    );
    const base = createDocument();
    let batches = 0;

    const { doc, changed } = await changeDocumentBatched(
      base,
      items,
      { schema: "sh.diffuse.output.track" },
      async () => {},
      () => batches++,
    );

    expect(changed).toBe(true);
    expect(recordsOf(doc)).toEqual(items);
    expect(batches).toBe(1);

    // The document is edited in place; no new identity is minted.
    expect(actorOf(doc)).toBe(actorOf(base));
  });

  it("keeps concurrent additions", async () => {
    const base = createDocument();

    const left = (await changeDocumentBatched(
      cloneDocument(base, "a".repeat(32)),
      [track("a")],
    )).doc;
    const right = (await changeDocumentBatched(
      cloneDocument(base, "b".repeat(32)),
      [track("b")],
    )).doc;

    const merged = mergeDocuments(left, right);

    expect(recordsOf(merged).map((item) => item.id).sort()).toEqual(["a", "b"]);
  });

  it("drops undefined values instead of throwing", () => {
    const withUndefined = {
      $type: "sh.diffuse.output.track",
      id: "a",
      uri: "https://a/a",
      stats: undefined,
      tags: { title: undefined, artist: "Artist" },
      list: ["kept", undefined],
    };

    const { doc, changed } = changeDocument(createDocument(), [withUndefined]);

    expect(changed).toBe(true);

    const [record] = recordsOf(doc);

    // Absent, not `null`: the key is gone entirely, which is also what
    // `JSON.stringify` would have produced for the same record.
    expect("stats" in record).toBe(false);
    expect(record.tags).toEqual({ artist: "Artist" });

    // Inside arrays an undefined entry becomes null, so the other items keep
    // their indices.
    expect(record.list).toEqual(["kept", null]);

    expect(recordsOf(loadDocument(encodeDocument(doc)))).toEqual([record]);
  });

  it("keeps list fields as arrays", () => {
    const facet = {
      $type: "sh.diffuse.output.facet",
      id: "f1",
      name: "Facet",
      tags: ["base", "extra"],
    };

    const { doc } = changeDocument(createDocument(), [facet]);
    const [record] = recordsOf(doc);

    expect(Array.isArray(record.tags)).toBe(true);
    expect(record).toEqual(facet);
  });

  it("repairs list fields that were written as numeric-keyed objects", () => {
    const mangled = {
      id: "f1",
      tags: { 0: "base", 1: "extra" },
      stats: { no: 1, nested: { 0: "a" } },
    };

    const { doc } = changeDocument(createDocument(), [mangled]);
    const [record] = recordsOf(doc);

    expect(record.tags).toEqual(["base", "extra"]);
    expect(record.stats).toEqual({ no: 1, nested: ["a"] });
  });

  it("skips the change when nothing differs", () => {
    const items = [track("a")];
    const first = changeDocument(createDocument(), items);

    const second = changeDocument(first.doc, items);

    expect(second.changed).toBe(false);
    expect(headsOf(second.doc)).toEqual(headsOf(first.doc));
  });

  it("keeps concurrent edits to different records", () => {
    const base = changeDocument(createDocument(), [track("a"), track("b")]).doc;

    const left = changeDocument(cloneDocument(base, "a".repeat(32)), [
      { ...track("a"), uri: "https://a/left" },
      track("b"),
    ]).doc;
    const right = changeDocument(cloneDocument(base, "b".repeat(32)), [
      track("a"),
      { ...track("b"), uri: "https://a/right" },
    ]).doc;

    const merged = mergeDocuments(left, right);

    expect(recordsOf(merged)).toEqual([
      { ...track("a"), uri: "https://a/left" },
      { ...track("b"), uri: "https://a/right" },
    ]);
  });

  it("keeps additions made on both sides", () => {
    const base = createDocument();

    const left = changeDocument(cloneDocument(base, "a".repeat(32)), [
      track("a"),
    ]).doc;
    const right = changeDocument(cloneDocument(base, "b".repeat(32)), [
      track("b"),
    ]).doc;

    const merged = mergeDocuments(left, right);

    expect(recordsOf(merged).map((item) => item.id).sort()).toEqual(["a", "b"]);
  });

  it("removes records that disappeared from the collection", () => {
    const { doc } = changeDocument(createDocument(), [track("a"), track("b")]);

    const next = changeDocument(doc, [track("a")]);

    expect(next.changed).toBe(true);
    expect(recordsOf(next.doc).map((item) => item.id)).toEqual(["a"]);
    expect(hasRecords(next.doc)).toBe(true);
  });

  it("applies only the changes since the given heads", () => {
    const many = Array.from({ length: 50 }, (_, i) => track(`t${i}`));
    const first = changeDocument(createDocument(), many).doc;
    const heads = headsOf(first);
    const base = cloneDocument(first);

    const second = changeDocument(first, [
      { ...track("t0"), uri: "https://a/changed" },
      ...many.slice(1),
    ]).doc;

    const increment = changesSince(second, heads);
    const applied = applyPack(base, increment);

    expect(recordsOf(applied)).toEqual(recordsOf(second));
    // One edited record is a fraction of the whole document.
    expect(increment.length * 2).toBeLessThan(encodeDocument(second).length);
  });

  it("falls back to a full document when the heads are unknown", () => {
    const doc = changeDocument(createDocument(), [track("a")]).doc;

    const bytes = changesSince(doc, ["not-hex"]);

    expect(recordsOf(loadDocument(bytes))).toEqual(recordsOf(doc));
  });

  it("applies a whole document entry", () => {
    const remote = changeDocument(
      cloneDocument(createDocument(), "b".repeat(32)),
      [track("b")],
    ).doc;

    const doc = applyPack(createDocument(), encodeDocument(remote));

    expect(recordsOf(doc)).toEqual([track("b")]);
  });

  it("treats an already applied entry as a no-op", () => {
    const remote = changeDocument(createDocument(), [track("a")]).doc;
    const bytes = encodeDocument(remote);

    const once = applyPack(createDocument(), bytes);
    const twice = applyPack(once, bytes);

    expect(recordsOf(twice)).toEqual([track("a")]);
    expect(headsOf(twice)).toEqual(headsOf(once));
  });

  it("adopts a shared document under this device's peer id", () => {
    const shared = changeDocument(createDocument(), [track("a")]).doc;
    const actor = "c".repeat(32);

    const adopted = adoptDocument(encodeDocument(shared), actor);

    expect(actorOf(adopted)).toBe("cccccccccccccccc");
    expect(recordsOf(adopted)).toEqual([track("a")]);
  });

  it("accepts changes from the original writer after adopting", () => {
    const shared = changeDocument(createDocument(), [track("a")]).doc;
    const heads = headsOf(shared);

    const adopted = adoptDocument(encodeDocument(shared), "c".repeat(32));
    const next = changeDocument(shared, [track("a"), track("b")]).doc;

    const applied = applyPack(adopted, changesSince(next, heads));

    expect(recordsOf(applied).map((item) => item.id)).toEqual(["a", "b"]);
  });

  it("merges documents that were created independently", () => {
    const left = changeDocument(createDocument(), [track("a")]).doc;
    const right = changeDocument(createDocument(), [track("b")]).doc;

    const merged = mergeDocuments(left, right);

    expect(recordsOf(merged).map((item) => item.id).sort()).toEqual(["a", "b"]);
  });

  it("merges a whole remote document", () => {
    const doc = changeDocument(
      cloneDocument(createDocument(), "a".repeat(32)),
      [track("a")],
    ).doc;
    const remote = changeDocument(
      cloneDocument(createDocument(), "b".repeat(32)),
      [track("b")],
    ).doc;

    const merged = mergeDocument(doc, encodeDocument(remote));

    expect(recordsOf(merged).map((item) => item.id).sort()).toEqual(["a", "b"]);
  });

  it("imports an update without a base when its dependencies arrive later", () => {
    const first = changeDocument(createDocument(), [track("a")]).doc;
    const heads = headsOf(first);
    const second = changeDocument(first, [track("a"), track("b")]).doc;
    const increment = changesSince(second, heads);

    // Applying the dependency-less update parks it; the snapshot then unparks.
    const doc = createDocument();
    applyIncremental(doc, increment);
    expect(recordsOf(doc)).toEqual([]);

    applyPack(doc, encodeDocument(second));
    expect(recordsOf(doc).map((item) => item.id)).toEqual(["a", "b"]);
  });

  /**
   * The decrypt layer length-prefixes each decrypted entry (u32be) — the
   * framing `applyLog` splits on. The tests encode it directly.
   *
   * @param {Uint8Array} bytes
   */
  const frame = (bytes) => {
    const out = new Uint8Array(bytes.length + 4);
    new DataView(out.buffer).setUint32(0, bytes.length);
    out.set(bytes, 4);
    return out;
  };

  const join = (parts) => {
    const total = parts.reduce((sum, part) => sum + part.length, 0);
    const out = new Uint8Array(total);
    let offset = 0;
    for (const part of parts) {
      out.set(part, offset);
      offset += part.length;
    }
    return out;
  };

  it("splits a framed log into its entries", async () => {
    const one = encodeDocument(changeDocument(createDocument(), [track("a")]).doc);
    const two = encodeDocument(changeDocument(createDocument(), [track("b")]).doc);

    const doc = await applyLog(
      createDocument("c".repeat(32)),
      join([frame(one), frame(two)]),
      "c".repeat(32),
    );

    expect(recordsOf(doc).map((item) => item.id).sort()).toEqual(["a", "b"]);
  });

  it("treats an unframed log as one legacy chunk", async () => {
    const one = encodeDocument(changeDocument(createDocument(), [track("a")]).doc);

    const doc = await applyLog(createDocument("c".repeat(32)), one, "c".repeat(32));

    expect(recordsOf(doc).map((item) => item.id)).toEqual(["a"]);
  });

  it("replays a log written by two devices without dropping either", async () => {
    const a = changeDocument(
      cloneDocument(createDocument(), "a".repeat(32)),
      [track("a1")],
    ).doc;
    const b = changeDocument(
      cloneDocument(createDocument(), "b".repeat(32)),
      [track("b1")],
    ).doc;

    const log = join([
      frame(encodeDocument(a)),
      frame(encodeDocument(b)),
    ]);

    const doc = await applyLog(createDocument("c".repeat(32)), log, "c".repeat(32));

    expect(recordsOf(doc).map((item) => item.id).sort()).toEqual(["a1", "b1"]);
  });
});
