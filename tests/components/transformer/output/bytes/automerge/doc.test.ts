// @ts-nocheck -- the fake client intentionally implements only the subset it uses

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import * as Automerge from "@automerge/automerge";

import {
  adoptDocument,
  applyPack,
  changeDocument,
  changesSince,
  createDocument,
  encodeDocument,
  headsOf,
  loadDocument,
  mergeDocument,
  mergeDocuments,
  recordsOf,
} from "~/components/transformer/output/bytes/automerge/doc.js";

/** @param {string} id */
const track = (id) => ({ $type: "sh.diffuse.output.track", id, uri: `https://a/${id}` });

describe("components/transformer/output/bytes/automerge/doc", () => {
  it("round-trips a collection through a document", () => {
    const items = [track("a"), track("b")];
    const { doc, changed } = changeDocument(createDocument(), items, {
      schema: "sh.diffuse.output.track",
    });

    expect(changed).toBe(true);
    expect(doc.$schema).toBe("sh.diffuse.output.track");
    expect(doc.order).toEqual(["a", "b"]);
    expect(recordsOf(doc)).toEqual(items);
  });

  it("skips the change when nothing differs", () => {
    const items = [track("a")];
    const first = changeDocument(createDocument(), items);

    const second = changeDocument(first.doc, items);

    expect(second.changed).toBe(false);
    expect(headsOf(second.doc)).toEqual(headsOf(first.doc));
  });

  it("keeps concurrent edits to different records", () => {
    // Both devices start from the same two records...
    const base = changeDocument(createDocument(), [track("a"), track("b")]).doc;

    // ...then edit a different record each. Cloning with a fresh actor models a
    // second device: changes from one actor share a sequence number.
    const left = changeDocument(Automerge.clone(base, { actor: "a".repeat(64) }), [
      { ...track("a"), uri: "https://a/left" },
      track("b"),
    ]).doc;
    const right = changeDocument(Automerge.clone(base, { actor: "b".repeat(64) }), [
      track("a"),
      { ...track("b"), uri: "https://a/right" },
    ]).doc;

    const merged = Automerge.merge(left, right);

    expect(recordsOf(merged)).toEqual([
      { ...track("a"), uri: "https://a/left" },
      { ...track("b"), uri: "https://a/right" },
    ]);
  });

  it("keeps additions made on both sides", () => {
    const base = createDocument();

    const left = changeDocument(Automerge.clone(base, { actor: "a".repeat(64) }), [
      track("a"),
    ]).doc;
    const right = changeDocument(Automerge.clone(base, { actor: "b".repeat(64) }), [
      track("b"),
    ]).doc;

    const merged = Automerge.merge(left, right);

    expect(recordsOf(merged).map((item) => item.id).sort()).toEqual(["a", "b"]);
  });

  it("removes records that disappeared from the collection", () => {
    const { doc } = changeDocument(createDocument(), [track("a"), track("b")]);

    const next = changeDocument(doc, [track("a")]);

    expect(next.changed).toBe(true);
    expect(next.doc.order).toEqual(["a"]);
    expect(Object.keys(next.doc.records)).toEqual(["a"]);
  });

  it("applies only the changes since the given heads", () => {
    const many = Array.from({ length: 50 }, (_, i) => track(`t${i}`));
    const first = changeDocument(createDocument(), many).doc;
    const heads = headsOf(first);

    const second = changeDocument(first, [
      { ...track("t0"), uri: "https://a/changed" },
      ...many.slice(1),
    ]).doc;

    const increment = changesSince(second, heads);
    // `second` was derived from `first`, so the original handle can no longer
    // be changed; a consumer would hold the live document instead.
    const applied = applyPack(Automerge.clone(first), increment);

    expect(recordsOf(applied)).toEqual(recordsOf(second));
    // One edited record is a fraction of the whole document.
    expect(increment.length * 2).toBeLessThan(encodeDocument(second).length);
  });

  it("falls back to a full document when the heads are unknown", () => {
    const doc = changeDocument(createDocument(), [track("a")]).doc;

    const bytes = changesSince(doc, ["f".repeat(64)]);

    // A concatenated document + change log loads as the same document.
    expect(recordsOf(Automerge.load(bytes))).toEqual(recordsOf(doc));
  });

  it("applies a whole document entry", () => {
    // A bootstrap or compacted entry is a document, not a change chunk;
    // `loadIncremental` ignores those, so `applyPack` must not.
    const remote = changeDocument(
      Automerge.clone(createDocument(), { actor: "b".repeat(64) }),
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

  it("adopts a shared document under this device's actor", () => {
    const shared = changeDocument(createDocument(), [track("a")]).doc;
    const actor = "c".repeat(64);

    const adopted = adoptDocument(encodeDocument(shared), actor);

    expect(Automerge.getActorId(adopted)).toBe(actor);
    expect(recordsOf(adopted)).toEqual([track("a")]);
  });

  it("accepts changes from the original writer after adopting", () => {
    const shared = changeDocument(createDocument(), [track("a")]).doc;
    const heads = headsOf(shared);

    const adopted = adoptDocument(encodeDocument(shared), "c".repeat(64));
    // The original writer keeps writing under its own actor; adopting must not
    // make the two of them share a sequence number.
    const next = changeDocument(shared, [track("a"), track("b")]).doc;

    const applied = applyPack(adopted, changesSince(next, heads));

    expect(recordsOf(applied).map((item) => item.id)).toEqual(["a", "b"]);
  });

  it("merges documents that were created independently", () => {
    // Each device starts with its own empty document (its own root), so a
    // plain merge would keep one side only.
    const left = changeDocument(createDocument(), [track("a")]).doc;
    const right = changeDocument(createDocument(), [track("b")]).doc;

    const merged = mergeDocuments(left, right);

    expect(recordsOf(merged).map((item) => item.id).sort()).toEqual(["a", "b"]);
  });

  it("merges a whole remote document", () => {
    const empty = createDocument();
    const doc = changeDocument(Automerge.clone(empty, { actor: "a".repeat(64) }), [
      track("a"),
    ]).doc;
    const remote = changeDocument(
      Automerge.clone(empty, { actor: "b".repeat(64) }),
      [track("b")],
    ).doc;

    const merged = mergeDocument(doc, encodeDocument(remote));

    expect(recordsOf(merged).map((item) => item.id).sort()).toEqual(["a", "b"]);
  });

  it("upgrades legacy array-shaped documents instead of losing them", () => {
    const legacy = Automerge.change(Automerge.init(), (d) => {
      d.collection = [track("a")];
    });

    const doc = loadDocument(encodeDocument(legacy));

    expect(recordsOf(doc)).toEqual([track("a")]);
  });
});
