import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";
import { tracks } from "@src/testing/sample/tracks.js";

import type { Item } from "@components/engine/queue/types.d.ts";

describe("components/engine/queue", () => {
  it("adds tracks", async () => {
    const items = await testWeb(async () => {
      const QueueEngine = await import("@components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("@src/testing/sample/tracks.js");

      await engine.add({ tracks });
      return engine.future();
    });

    expect(items.map((i) => i.id).join("/")).toBe(
      tracks.map((t) => t.id).join("/"),
    );
  });

  it("pools + fills tracks and shifts the queue", async () => {
    const item = await testWeb(async () => {
      const QueueEngine = await import("@components/engine/queue/element.js");
      const engine = new QueueEngine.CLASS();

      document.body.append(engine);

      const { tracks } = await import("@src/testing/sample/tracks.js");

      await engine.pool(tracks);
      await engine.fill({ amount: 1, shuffled: false });
      await engine.shift();

      return engine.now();
    });

    expect(item?.id).toBe(tracks[0].id);
  });
});
