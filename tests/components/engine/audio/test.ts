import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/engine/audio", () => {
  it("has default volume of 0.75", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/audio/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      return engine.volume();
    });

    expect(result).toBe(0.75);
  });

  it("adjustVolume updates the global volume signal", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/audio/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      engine.adjustVolume({ volume: 0.5 });
      return engine.volume();
    });

    expect(result).toBe(0.5);
  });

  it("adjustVolume clamps to the provided value", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/audio/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      engine.adjustVolume({ volume: 1.0 });
      return engine.volume();
    });

    expect(result).toBe(1.0);
  });

  it("isPlaying returns false with no items", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/audio/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      return engine.isPlaying();
    });

    expect(result).toBe(false);
  });

  it("supply with URL items updates the items signal", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/audio/element.js");
      const { trackA } = await import("~/testing/sample/tracks.js");
      const engine = new mod.CLASS();
      document.body.append(engine);

      engine.supply({
        audio: [
          {
            id: "audio-a",
            url: "/testing/sample/audio.mp3",
            isPreload: false,
            track: trackA,
          },
          {
            id: "audio-b",
            url: "/testing/sample/audio.mp3",
            isPreload: false,
            track: trackA,
          },
        ],
      });

      return engine.items().map((i) => i.id);
    });

    expect(result).toEqual(["audio-a", "audio-b"]);
  });

  it("supply with same IDs does not update items signal", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/audio/element.js");
      const { trackA } = await import("~/testing/sample/tracks.js");
      const engine = new mod.CLASS();
      document.body.append(engine);

      const item = {
        id: "audio-a",
        url: "/testing/sample/audio.mp3",
        isPreload: false,
        track: trackA,
      };

      engine.supply({ audio: [item] });
      const itemsAfterFirst = engine.items();

      engine.supply({ audio: [item] });
      const itemsAfterSecond = engine.items();

      // Same reference means the signal was not updated
      return itemsAfterFirst === itemsAfterSecond;
    });

    expect(result).toBe(true);
  });

  it("supply replaces items when IDs change", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/audio/element.js");
      const { trackA } = await import("~/testing/sample/tracks.js");
      const engine = new mod.CLASS();
      document.body.append(engine);

      engine.supply({
        audio: [{
          id: "audio-a",
          url: "/testing/sample/audio.mp3",
          isPreload: false,
          track: trackA,
        }],
      });

      engine.supply({
        audio: [{
          id: "audio-b",
          url: "/testing/sample/audio.mp3",
          isPreload: false,
          track: trackA,
        }],
      });

      return engine.items().map((i) => i.id);
    });

    expect(result).toEqual(["audio-b"]);
  });

  it("supply with isPreload change triggers items update", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/audio/element.js");
      const { trackA } = await import("~/testing/sample/tracks.js");
      const engine = new mod.CLASS();
      document.body.append(engine);

      engine.supply({
        audio: [{
          id: "audio-a",
          url: "/testing/sample/audio.mp3",
          isPreload: true,
          track: trackA,
        }],
      });

      engine.supply({
        audio: [{
          id: "audio-a",
          url: "/testing/sample/audio.mp3",
          isPreload: false,
          track: trackA,
        }],
      });

      return engine.items()[0]?.isPreload;
    });

    expect(result).toBe(false);
  });

  it("persists volume to localStorage", async () => {
    const stored = await testWeb(async () => {
      const mod = await import("~/components/engine/audio/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      engine.adjustVolume({ volume: 0.3 });

      for (let i = 0; i < localStorage.length; i++) {
        const key = localStorage.key(i)!;
        if (key.includes("engine/audio") && key.endsWith("/volume")) {
          return localStorage.getItem(key);
        }
      }
      return null;
    });

    expect(stored).toBe("0.3");
  });

  it("restores volume from localStorage on connect", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/audio/element.js");

      // Set volume with first engine instance
      const engine1 = new mod.CLASS();
      document.body.append(engine1);
      engine1.adjustVolume({ volume: 0.4 });

      // Second instance reads from localStorage
      const engine2 = new mod.CLASS();
      document.body.append(engine2);
      return engine2.volume();
    });

    expect(result).toBe(0.4);
  });
});
