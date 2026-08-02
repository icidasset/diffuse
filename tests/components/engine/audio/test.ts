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

  // Sample audio tests

  it("state returns undefined for unknown audio id", async () => {
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

      return engine.state("no-such-id");
    });

    expect(result).toBeUndefined();
  });

  it("state has initial loadingState of loading before audio loads", async () => {
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

      const st = engine.state("audio-a");
      return {
        loadingState: st?.loadingState(),
        currentTime: st?.currentTime(),
        isPlaying: st?.isPlaying(),
        hasEnded: st?.hasEnded(),
      };
    });

    expect(result.loadingState).toBe("loading");
    expect(result.currentTime).toBe(0);
    expect(result.isPlaying).toBe(false);
    expect(result.hasEnded).toBe(false);
  });

  it("loadingState becomes loaded after audio loads", async () => {
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

      const audioEl = engine.querySelector(
        'de-audio-item[id="audio-a"]:not([preload]) audio',
      ) as HTMLAudioElement;

      await new Promise<void>((resolve, reject) => {
        const st = engine.state("audio-a");
        if (st?.loadingState() === "loaded") { resolve(); return; }
        const timer = setTimeout(
          () => reject(new Error("timeout waiting for audio load")),
          10000,
        );
        const done = () => { clearTimeout(timer); resolve(); };
        audioEl.addEventListener("canplay", done, { once: true });
        audioEl.addEventListener("suspend", done, { once: true });
        audioEl.addEventListener(
          "error",
          () => { clearTimeout(timer); reject(new Error("audio load error")); },
          { once: true },
        );
      });

      return engine.state("audio-a")?.loadingState();
    });

    expect(result).toBe("loaded");
  });

  it("duration is positive after audio loads", async () => {
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

      const audioEl = engine.querySelector(
        'de-audio-item[id="audio-a"]:not([preload]) audio',
      ) as HTMLAudioElement;

      await new Promise<void>((resolve, reject) => {
        if (audioEl.duration > 0 && isFinite(audioEl.duration)) {
          resolve();
          return;
        }
        const timer = setTimeout(
          () => reject(new Error("timeout waiting for duration")),
          10000,
        );
        const done = () => { clearTimeout(timer); resolve(); };
        audioEl.addEventListener("durationchange", done, { once: true });
        audioEl.addEventListener(
          "error",
          () => { clearTimeout(timer); reject(new Error("audio load error")); },
          { once: true },
        );
      });

      return engine.state("audio-a")?.duration();
    });

    expect(result).toBeGreaterThan(0);
  });

  it("currentTime is 0 before seek", async () => {
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

      const audioEl = engine.querySelector(
        'de-audio-item[id="audio-a"]:not([preload]) audio',
      ) as HTMLAudioElement;

      await new Promise<void>((resolve, reject) => {
        const st = engine.state("audio-a");
        if (st?.loadingState() === "loaded") { resolve(); return; }
        const timer = setTimeout(
          () => reject(new Error("timeout")),
          10000,
        );
        const done = () => { clearTimeout(timer); resolve(); };
        audioEl.addEventListener("canplay", done, { once: true });
        audioEl.addEventListener("suspend", done, { once: true });
        audioEl.addEventListener(
          "error",
          () => { clearTimeout(timer); reject(new Error("audio load error")); },
          { once: true },
        );
      });

      return engine.state("audio-a")?.currentTime();
    });

    expect(result).toBe(0);
  });

  it("seek by currentTime updates audio element currentTime", async () => {
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

      const audioEl = engine.querySelector(
        'de-audio-item[id="audio-a"]:not([preload]) audio',
      ) as HTMLAudioElement;

      await new Promise<void>((resolve, reject) => {
        const st = engine.state("audio-a");
        if (st?.loadingState() === "loaded") { resolve(); return; }
        const timer = setTimeout(() => reject(new Error("timeout")), 10000);
        const done = () => { clearTimeout(timer); resolve(); };
        audioEl.addEventListener("canplay", done, { once: true });
        audioEl.addEventListener("suspend", done, { once: true });
        audioEl.addEventListener(
          "error",
          () => { clearTimeout(timer); reject(new Error("audio load error")); },
          { once: true },
        );
      });

      engine.seek({ audioId: "audio-a", currentTime: 1 });

      return audioEl.currentTime;
    });

    expect(result).toBe(1);
  });

  it("seek by percentage updates audio element currentTime proportionally", async () => {
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

      const audioEl = engine.querySelector(
        'de-audio-item[id="audio-a"]:not([preload]) audio',
      ) as HTMLAudioElement;

      await new Promise<void>((resolve, reject) => {
        if (audioEl.duration > 0 && isFinite(audioEl.duration)) { resolve(); return; }
        const timer = setTimeout(() => reject(new Error("timeout")), 10000);
        const done = () => { clearTimeout(timer); resolve(); };
        audioEl.addEventListener("durationchange", done, { once: true });
        audioEl.addEventListener(
          "error",
          () => { clearTimeout(timer); reject(new Error("audio load error")); },
          { once: true },
        );
      });

      engine.seek({ audioId: "audio-a", percentage: 0.5 });

      return {
        currentTime: audioEl.currentTime,
        expected: 0.5 * audioEl.duration,
      };
    });

    expect(result.currentTime).toBe(result.expected);
  });

  it("play sets isPlaying to true", async () => {
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

      const audioEl = engine.querySelector(
        'de-audio-item[id="audio-a"]:not([preload]) audio',
      ) as HTMLAudioElement;

      await new Promise<void>((resolve, reject) => {
        const st = engine.state("audio-a");
        if (st?.loadingState() === "loaded") { resolve(); return; }
        const timer = setTimeout(() => reject(new Error("timeout")), 10000);
        const done = () => { clearTimeout(timer); resolve(); };
        audioEl.addEventListener("canplay", done, { once: true });
        audioEl.addEventListener("suspend", done, { once: true });
        audioEl.addEventListener(
          "error",
          () => { clearTimeout(timer); reject(new Error("audio load error")); },
          { once: true },
        );
      });

      // play() sets isPlaying optimistically before the audio.play() promise settles
      engine.play({ audioId: "audio-a", volume: 0.5 });

      return engine.state("audio-a")?.isPlaying();
    });

    expect(result).toBe(true);
  });

  it("pause resets isPlaying to false", async () => {
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

      const audioEl = engine.querySelector(
        'de-audio-item[id="audio-a"]:not([preload]) audio',
      ) as HTMLAudioElement;

      await new Promise<void>((resolve, reject) => {
        const st = engine.state("audio-a");
        if (st?.loadingState() === "loaded") { resolve(); return; }
        const timer = setTimeout(() => reject(new Error("timeout")), 10000);
        const done = () => { clearTimeout(timer); resolve(); };
        audioEl.addEventListener("canplay", done, { once: true });
        audioEl.addEventListener("suspend", done, { once: true });
        audioEl.addEventListener(
          "error",
          () => { clearTimeout(timer); reject(new Error("audio load error")); },
          { once: true },
        );
      });

      engine.play({ audioId: "audio-a", volume: 0.5 });
      engine.pause({ audioId: "audio-a" });

      // Wait for pause event to propagate
      await new Promise<void>((resolve) => setTimeout(resolve, 50));

      return engine.state("audio-a")?.isPlaying();
    });

    expect(result).toBe(false);
  });

  it("playing event sets isPlaying to true", async () => {
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

      const audioEl = engine.querySelector(
        'de-audio-item[id="audio-a"]:not([preload]) audio',
      ) as HTMLAudioElement;

      await new Promise<void>((resolve, reject) => {
        const st = engine.state("audio-a");
        if (st?.loadingState() === "loaded") { resolve(); return; }
        const timer = setTimeout(() => reject(new Error("timeout")), 10000);
        const done = () => { clearTimeout(timer); resolve(); };
        audioEl.addEventListener("canplay", done, { once: true });
        audioEl.addEventListener("suspend", done, { once: true });
        audioEl.addEventListener(
          "error",
          () => { clearTimeout(timer); reject(new Error("audio load error")); },
          { once: true },
        );
      });

      // Pause first so isPlaying is false, then dispatch `playing` to verify
      // that playingEvent sets isPlaying to true on its own.
      engine.play({ audioId: "audio-a", volume: 0.5 });
      engine.pause({ audioId: "audio-a" });

      await new Promise<void>((resolve) => setTimeout(resolve, 50));
      const isPlayingBefore = engine.state("audio-a")?.isPlaying();

      audioEl.dispatchEvent(new Event("playing"));
      const isPlayingAfter = engine.state("audio-a")?.isPlaying();

      return { isPlayingBefore, isPlayingAfter };
    });

    expect(result.isPlayingBefore).toBe(false);
    expect(result.isPlayingAfter).toBe(true);
  });
});
