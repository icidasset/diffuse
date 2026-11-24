// @ts-nocheck -- the codec is plain JS with JSDoc types

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import {
  applyPatch,
  decodePatch,
  decodeState,
  deltaPatch,
  emptyState,
  encodePatch,
  encodeState,
  hasState,
  snapshotPatch,
  stateItems,
} from "~/components/output/raw/firkin/encrypted/record-sync/codec.js";

const version = (at, device = "aa") => ({ at, device });

describe("components/output/raw/firkin/encrypted/record-sync codec", () => {
  it("applies LWW puts and deletes regardless of arrival order", () => {
    const put = (at) => ({
      v: 1,
      at,
      device: "aa",
      put: { t1: { id: "t1", title: at } },
    });
    const del = (at) => ({ v: 1, at, device: "aa", del: ["t1"] });

    const forward = emptyState();
    applyPatch(forward, put("2026-01-01T00:00:00.000Z"));
    applyPatch(forward, put("2026-01-02T00:00:00.000Z"));
    applyPatch(forward, del("2026-01-03T00:00:00.000Z"));

    const backward = emptyState();
    applyPatch(backward, del("2026-01-03T00:00:00.000Z"));
    applyPatch(backward, put("2026-01-02T00:00:00.000Z"));
    applyPatch(backward, put("2026-01-01T00:00:00.000Z"));

    expect(stateItems(forward)).toEqual([]);
    expect(stateItems(backward)).toEqual([]);
    expect(forward.tombstones.t1).toEqual(version("2026-01-03T00:00:00.000Z"));
    expect(backward.tombstones.t1).toEqual(
      version("2026-01-03T00:00:00.000Z"),
    );
  });

  it("resurrects a deleted record with a newer put", () => {
    const state = emptyState();
    applyPatch(state, {
      v: 1,
      at: "2026-01-02T00:00:00.000Z",
      device: "aa",
      del: ["t1"],
    });
    applyPatch(state, {
      v: 1,
      at: "2026-01-03T00:00:00.000Z",
      device: "bb",
      put: { t1: { id: "t1", title: "Back" } },
    });

    expect(stateItems(state)).toEqual([{ id: "t1", title: "Back" }]);
    expect(state.tombstones.t1).toBeUndefined();
  });

  it("is idempotent", () => {
    const state = emptyState();
    const patch = {
      v: 1,
      at: "2026-01-02T00:00:00.000Z",
      device: "aa",
      put: { t1: { id: "t1" } },
      order: ["t1"],
    };

    expect(applyPatch(state, patch)).toBe(true);
    expect(applyPatch(state, patch)).toBe(false);
    expect(stateItems(state)).toEqual([{ id: "t1" }]);
  });

  it("keeps a newer local record when merging an older snapshot", () => {
    const state = emptyState();
    applyPatch(state, {
      v: 1,
      at: "2026-01-05T00:00:00.000Z",
      device: "bb",
      put: { t1: { id: "t1", title: "Local" } },
    });

    const changed = applyPatch(state, {
      v: 1,
      at: "2026-01-06T00:00:00.000Z",
      device: "aa",
      snapshot: {
        records: {
          t1: {
            record: { id: "t1", title: "Old" },
            at: "2026-01-01T00:00:00.000Z",
            device: "aa",
          },
        },
        tombstones: {},
        order: {
          value: ["t1"],
          at: "2026-01-01T00:00:00.000Z",
          device: "aa",
        },
      },
    });

    // The order is older, but the local record wins.
    expect(changed).toBe(true);
    expect(stateItems(state)[0].title).toBe("Local");
    expect(state.order.value).toEqual(["t1"]);
  });

  it("uses the snapshot's newer records and tombstones", () => {
    const state = emptyState();
    applyPatch(state, {
      v: 1,
      at: "2026-01-01T00:00:00.000Z",
      device: "aa",
      put: { t1: { id: "t1" }, t2: { id: "t2" } },
    });

    applyPatch(state, {
      v: 1,
      at: "2026-01-03T00:00:00.000Z",
      device: "bb",
      snapshot: {
        records: {
          t1: {
            record: { id: "t1", title: "Newer" },
            at: "2026-01-02T00:00:00.000Z",
            device: "bb",
          },
        },
        tombstones: {
          t2: version("2026-01-02T00:00:00.000Z", "bb"),
        },
        order: {
          value: [],
          at: "2026-01-02T00:00:00.000Z",
          device: "bb",
        },
      },
    });

    expect(stateItems(state)).toEqual([{ id: "t1", title: "Newer" }]);
    expect(state.tombstones.t2).toBeDefined();
  });

  it("diffs records and order into a patch", () => {
    const state = emptyState();
    const first = deltaPatch(
      state,
      { t1: { id: "t1", title: "A" } },
      ["t1"],
      version("2026-01-01T00:00:00.000Z"),
    );
    expect(first).toEqual({
      v: 1,
      at: "2026-01-01T00:00:00.000Z",
      device: "aa",
      put: { t1: { id: "t1", title: "A" } },
      order: ["t1"],
    });

    applyPatch(state, first);

    expect(
      deltaPatch(
        state,
        { t1: { id: "t1", title: "A" } },
        ["t1"],
        version("2026-01-02T00:00:00.000Z"),
      ),
    ).toBeUndefined();

    const second = deltaPatch(
      state,
      { t1: { id: "t1", title: "B" } },
      ["t1"],
      version("2026-01-02T00:00:00.000Z"),
    );
    expect(second.put).toEqual({ t1: { id: "t1", title: "B" } });
    expect(second.order).toBeUndefined();

    const removed = deltaPatch(
      state,
      {},
      [],
      version("2026-01-03T00:00:00.000Z"),
    );
    expect(removed).toEqual({
      v: 1,
      at: "2026-01-03T00:00:00.000Z",
      device: "aa",
      del: ["t1"],
      order: [],
    });
  });

  it("encodes and decodes state and patches", () => {
    const state = emptyState();
    const patch = deltaPatch(
      state,
      { t1: { id: "t1", "$enc": { uri: "sealed" } } },
      ["t1"],
      version("2026-01-01T00:00:00.000Z"),
    );

    applyPatch(state, patch);

    const decodedState = decodeState(encodeState(state));
    expect(decodedState).toEqual(state);
    expect(hasState(decodedState)).toBe(true);

    const decodedPatch = decodePatch(encodePatch(patch));
    expect(decodedPatch).toEqual(patch);
  });

  it("writes a snapshot with per-record versions", () => {
    const state = emptyState();
    applyPatch(state, {
      v: 1,
      at: "2026-01-02T00:00:00.000Z",
      device: "bb",
      put: { t1: { id: "t1" } },
      order: ["t1"],
    });

    const snapshot = snapshotPatch(state, version("2026-01-03T00:00:00.000Z"));
    expect(snapshot.snapshot.records.t1).toEqual({
      record: { id: "t1" },
      at: "2026-01-02T00:00:00.000Z",
      device: "bb",
    });
    expect(snapshot.snapshot.order).toEqual(state.order);
  });
});
