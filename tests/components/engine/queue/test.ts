import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";
import { tracks } from "~/testing/sample/tracks.js";

describe("components/engine/queue", () => {
  it("adds tracks", async () => {
    const items = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.add({ trackIds: tracks.map((t) => t.id) });
      return engine.future();
    });

    const futureIds = items.map((i) => i.id).join("/");
    const sampleIds = tracks.map((t) => t.id).join("/");

    expect(futureIds).toEqual(sampleIds);
  });

  it("pools + fills tracks and shifts the queue", async () => {
    const item = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.supply({ trackIds: tracks.map((t) => t.id) });
      await engine.fill({ amount: 1, shuffled: false });
      await engine.shift();

      return engine.now();
    });

    expect(item?.id).toBe(tracks[0].id);
  });

  it("[shared worker] adds tracks and shifts + unshifts the queue", async () => {
    const item = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();
      engine.setAttribute("group", "tests");

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.add({ trackIds: tracks.map((t) => t.id) });
      await engine.shift();
      await engine.shift();
      await engine.unshift();

      return engine.now();
    });

    expect(item?.id).toBe(tracks[0].id);
  });

  it("adds tracks to the front with inFront: true", async () => {
    const items = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.add({ trackIds: [tracks[1].id] });
      await engine.add({ inFront: true, trackIds: [tracks[0].id] });
      return engine.future();
    });

    expect(items[0].id).toBe(tracks[0].id);
    expect(items[1].id).toBe(tracks[1].id);
  });

  it("clears only auto-filled items when keepManual is true", async () => {
    const items = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.supply({ trackIds: tracks.map((t) => t.id) });
      await engine.add({ trackIds: [tracks[0].id] });
      await engine.fill({ amount: 2, shuffled: false });
      await engine.clear({ keepManual: true });
      return engine.future();
    });

    expect(items.length).toBe(1);
    expect(items[0].manualEntry).toBe(true);
  });

  it("clears all items when keepManual is false", async () => {
    const count = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.add({ trackIds: tracks.map((t) => t.id) });
      await engine.clear({ keepManual: false });
      return (await engine.future()).length;
    });

    expect(count).toBe(0);
  });

  it("supply fingerprint is consistent for the same track IDs", async () => {
    const [fp1, fp2, fp3] = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");
      const trackIds = tracks.map((t) => t.id);

      await engine.supply({ trackIds });
      const fp1 = await engine.supplyFingerprint();

      await engine.supply({ trackIds });
      const fp2 = await engine.supplyFingerprint();

      await engine.supply({ trackIds: [trackIds[0]] });
      const fp3 = await engine.supplyFingerprint();

      return [fp1, fp2, fp3];
    });

    expect(fp1).toBe(fp2);
    expect(fp1).not.toBe(fp3);
  });

  it("fill with augment adds on top of existing auto items", async () => {
    const count = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.supply({ trackIds: tracks.map((t) => t.id) });
      await engine.fill({ amount: 2, shuffled: true });
      await engine.fill({ augment: true, amount: 1, shuffled: true });
      return (await engine.future()).length;
    });

    expect(count).toBe(3);
  });

  it("moves a future item to a new position", async () => {
    const ids = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.add({ trackIds: tracks.slice(0, 3).map((t) => t.id) });
      await engine.move({ from: 0, to: 2 });
      return engine.future().map((i) => i.id);
    });

    expect(ids[0]).toBe(tracks[1].id);
    expect(ids[1]).toBe(tracks[2].id);
    expect(ids[2]).toBe(tracks[0].id);
  });

  it("move preserves now when reordering around it", async () => {
    const result = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.add({ trackIds: tracks.slice(0, 3).map((t) => t.id) });
      await engine.shift();
      // flat list: [past[0]=tracks[0]], now=tracks[1], future=[tracks[2]]
      // move future item (idx 2) before now (idx 1)
      await engine.move({ from: 2, to: 0 });

      return {
        now: engine.now()?.id,
        past: engine.past().map((i) => i.id),
        future: engine.future().map((i) => i.id),
      };
    });

    // shift() moved tracks[0] to now; move({ from:2, to:0 }) put tracks[2]
    // before now, so: past=[tracks[2]], now=tracks[0], future=[tracks[1]]
    expect(result.now).toBe(tracks[0].id);
    expect(result.past[0]).toBe(tracks[2].id);
    expect(result.future[0]).toBe(tracks[1].id);
  });

  it("move does nothing when from === to", async () => {
    const ids = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.add({ trackIds: tracks.slice(0, 3).map((t) => t.id) });
      await engine.move({ from: 1, to: 1 });
      return engine.future().map((i) => i.id);
    });

    expect(ids[0]).toBe(tracks[0].id);
    expect(ids[1]).toBe(tracks[1].id);
    expect(ids[2]).toBe(tracks[2].id);
  });

  it("move does nothing for out-of-bounds indices", async () => {
    const ids = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.add({ trackIds: tracks.slice(0, 2).map((t) => t.id) });
      await engine.move({ from: 0, to: 99 });
      return engine.future().map((i) => i.id);
    });

    expect(ids[0]).toBe(tracks[0].id);
    expect(ids[1]).toBe(tracks[1].id);
  });

  it("[shared worker] has the correct past", async () => {
    const item = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();
      engine.setAttribute("group", "tests");

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.add({ trackIds: tracks.map((t) => t.id) });
      await engine.shift();
      await engine.shift();

      return engine.past()[0];
    });

    expect(item?.id).toBe(tracks[0].id);
  });
});
