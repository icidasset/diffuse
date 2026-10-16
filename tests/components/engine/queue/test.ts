import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";
import { tracks } from "~/testing/sample/tracks.js";

describe("components/engine/queue", () => {
  it("adds tracks", async () => {
    const result = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.add({ trackIds: tracks.map((t) => t.id) });
      return { now: engine.now()?.id, future: engine.future() };
    });

    // The first added track is activated on an idle queue,
    // the rest are queued up in the future.
    expect(result.now).toBe(tracks[0].id);

    const futureIds = result.future.map((i) => i.id).join("/");
    const sampleIds = tracks.slice(1).map((t) => t.id).join("/");

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
      // activates tracks[0] as now
      await engine.shift(); // now = tracks[1]
      await engine.shift(); // now = tracks[2]
      await engine.unshift(); // rewind: now = tracks[1]

      return engine.now();
    });

    expect(item?.id).toBe(tracks[1].id);
  });

  it("adds tracks to the front with inFront: true", async () => {
    const items = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.add({ trackIds: [tracks[2].id] }); // activates as now on an idle queue
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
      await engine.add({ trackIds: [tracks[0].id] }); // activates as now on an idle queue
      await engine.add({ trackIds: [tracks[1].id] });
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
    const result = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.add({ trackIds: tracks.slice(0, 3).map((t) => t.id) });
      // activates tracks[0] (key "1") as now; future = [tracks[1] (key "2"), tracks[2] (key "3")]
      await engine.move({ key: "2", to: 2 });
      return {
        now: engine.now()?.id,
        future: engine.future().map((i) => i.id),
      };
    });

    expect(result.now).toBe(tracks[0].id);
    expect(result.future).toEqual([tracks[2].id, tracks[1].id]);
  });

  it("move preserves now when reordering around it", async () => {
    const result = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.add({ trackIds: tracks.slice(0, 3).map((t) => t.id) });
      // activates tracks[0] as now
      await engine.shift();
      // flat list: [past[0]=tracks[0]], now=tracks[1], future=[tracks[2]]
      // move future item (key "3") before now
      await engine.move({ key: "3", to: 0 });

      return {
        now: engine.now()?.id,
        past: engine.past().map((i) => i.id),
        future: engine.future().map((i) => i.id),
      };
    });

    // shift() moved tracks[0] to past, tracks[1] became now; move({ key:"3", to:0 })
    // put tracks[2] before now, so: past=[tracks[2], tracks[0]], now=tracks[1], future=[]
    expect(result.now).toBe(tracks[1].id);
    expect(result.past).toEqual([tracks[2].id, tracks[0].id]);
    expect(result.future).toEqual([]);
  });

  it("move does nothing when from === to", async () => {
    const result = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.add({ trackIds: tracks.slice(0, 3).map((t) => t.id) });
      // activates tracks[0] (key "1") as now; future = [tracks[1] (key "2"), tracks[2] (key "3")]
      await engine.move({ key: "2", to: 1 });
      return {
        now: engine.now()?.id,
        future: engine.future().map((i) => i.id),
      };
    });

    expect(result.now).toBe(tracks[0].id);
    expect(result.future).toEqual([tracks[1].id, tracks[2].id]);
  });

  it("move does nothing for out-of-bounds indices", async () => {
    const result = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.add({ trackIds: tracks.slice(0, 2).map((t) => t.id) });
      // activates tracks[0] (key "1") as now; future = [tracks[1] (key "2")]
      await engine.move({ key: "2", to: 99 });
      return {
        now: engine.now()?.id,
        future: engine.future().map((i) => i.id),
      };
    });

    expect(result.now).toBe(tracks[0].id);
    expect(result.future).toEqual([tracks[1].id]);
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

  it("shift with by advances multiple positions in one call", async () => {
    const result = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.add({ trackIds: tracks.map((t) => t.id) });
      await engine.shift(); // now = tracks[0]
      await engine.shift({ by: 2 }); // skip tracks[1], now = tracks[2]

      return {
        now: engine.now()?.id,
        past: engine.past().map((i) => i.id),
        future: engine.future().map((i) => i.id),
      };
    });

    expect(result.now).toBe(tracks[2].id);
    expect(result.past).toEqual([tracks[0].id, tracks[1].id]);
    expect(result.future).toEqual([]);
  });

  it("unshift with by rewinds multiple positions in one call", async () => {
    const result = await testWeb(async () => {
      const QueueEngine = await import("~/components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("~/testing/sample/tracks.js");

      await engine.add({ trackIds: tracks.map((t) => t.id) });
      await engine.shift(); // now = tracks[0]
      await engine.shift(); // now = tracks[1]
      await engine.shift(); // now = tracks[2]
      await engine.unshift({ by: 2 }); // rewind to tracks[0]

      return {
        now: engine.now()?.id,
        past: engine.past().map((i) => i.id),
        future: engine.future().map((i) => i.id),
      };
    });

    expect(result.now).toBe(tracks[0].id);
    expect(result.past).toEqual([]);
    expect(result.future).toEqual([tracks[1].id, tracks[2].id]);
  });
});
