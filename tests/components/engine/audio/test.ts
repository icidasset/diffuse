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

  it("supply keeps the URL expiry so callers can refresh ahead of it", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/audio/element.js");
      const { trackA } = await import("~/testing/sample/tracks.js");
      const engine = new mod.CLASS();
      document.body.append(engine);

      engine.supply({
        audio: [{
          id: "audio-a",
          url: "/testing/sample/audio.mp3",
          expiresAt: 1_234_567,
          isPreload: true,
          track: trackA,
        }],
      });

      const item = engine.items()[0];
      return { expiresAt: item.expiresAt, url: item.url };
    });

    expect(result.url).toBe("/testing/sample/audio.mp3");
    expect(result.expiresAt).toBe(1_234_567);
  });

  it("reload only reloads a healthy element when forced", async () => {
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

      const itemEl = engine.querySelector(
        'de-audio-item[id="audio-a"]',
      ) as HTMLElement;
      const audio = itemEl.querySelector("audio") as HTMLAudioElement;

      // The state a refreshed presigned URL finds: loaded and healthy.
      Object.defineProperty(audio, "readyState", {
        value: 4,
        configurable: true,
      });
      let loads = 0;
      // deno-lint-ignore no-explicit-any
      (audio as any).load = () => {
        loads += 1;
      };

      // The initial render already writes a progress of 0.
      itemEl.removeAttribute("initial-progress");

      engine.reload({ audioId: "audio-a", play: false, progress: 0.25 });
      const withoutForce = {
        loads,
        progress: itemEl.getAttribute("initial-progress"),
      };

      engine.reload({
        audioId: "audio-a",
        play: false,
        progress: 0.5,
        force: true,
      });
      const withForce = {
        loads,
        progress: itemEl.getAttribute("initial-progress"),
      };

      return { withoutForce, withForce };
    });

    expect(result.withoutForce).toEqual({ loads: 0, progress: null });
    expect(result.withForce).toEqual({ loads: 1, progress: "0.5" });
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

  it("persists the audio output device to localStorage", async () => {
    const stored = await testWeb(async () => {
      const mod = await import("~/components/engine/audio/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setSink("test-output-device");

      for (let i = 0; i < localStorage.length; i++) {
        const key = localStorage.key(i)!;
        if (key.includes("engine/audio") && key.endsWith("/sink")) {
          return localStorage.getItem(key);
        }
      }
      return null;
    });

    expect(stored).toBe("test-output-device");
  });

  it("clears the stored audio output device when set to the system default", async () => {
    const stored = await testWeb(async () => {
      const mod = await import("~/components/engine/audio/element.js");
      const engine = new mod.CLASS();
      document.body.append(engine);
      await engine.setSink("test-output-device");
      await engine.setSink("");

      for (let i = 0; i < localStorage.length; i++) {
        const key = localStorage.key(i)!;
        if (key.includes("engine/audio") && key.endsWith("/sink")) {
          return localStorage.getItem(key);
        }
      }
      return null;
    });

    expect(stored).toBe(null);
  });

  it("applies the stored audio output device to the shared AudioContext", async () => {
    const calls = await testWeb(async () => {
      const calls: string[] = [];

      // Stub `setSinkId` so the test doesn't depend on native support and can
      // observe what the engine applies.
      const proto = AudioContext.prototype as unknown as {
        setSinkId?: (id: string) => Promise<void>;
      };
      const original = proto.setSinkId;
      proto.setSinkId = (id: string) => {
        calls.push(id);
        return Promise.resolve();
      };

      try {
        localStorage.setItem("diffuse/engine/audio/default/sink", "device-x");

        const mod = await import("~/components/engine/audio/element.js");
        const engine = new mod.CLASS();
        document.body.append(engine);

        // Accessing `webAudio` lazily creates the graph, which applies the
        // stored sink for the group.
        void engine.webAudio;
        await new Promise((resolve) => setTimeout(resolve, 0));

        return calls;
      } finally {
        if (original === undefined) delete proto.setSinkId;
        else proto.setSinkId = original;
      }
    });

    expect(calls).toContain("device-x");
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

  it("supply with an empty list removes all items", async () => {
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

      const hadItem = engine.querySelector('de-audio-item[id="audio-a"]') !== null;

      engine.supply({ audio: [] });

      return {
        hadItem,
        items: engine.items().length,
        rendered: engine.querySelectorAll("de-audio-item").length,
      };
    });

    expect(result.hadItem).toBe(true);
    expect(result.items).toBe(0);
    expect(result.rendered).toBe(0);
  });

  it("keeps a preloaded item's audio element when it becomes active", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/engine/audio/element.js");
      const { trackA, trackB } = await import("~/testing/sample/tracks.js");
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
            isPreload: true,
            track: trackB,
          },
        ],
      });

      const before = engine.querySelector(
        'de-audio-item[id="audio-b"] audio',
      ) as HTMLAudioElement;

      // The preloaded track is promoted to the active one. This used to
      // recreate its <audio> element (restarting the download) while dropping
      // the preloading element without unloading it — leaving that element's
      // network request (and connection) behind.
      engine.supply({
        audio: [{
          id: "audio-b",
          url: "/testing/sample/audio.mp3",
          isPreload: false,
          track: trackB,
        }],
      });

      const after = engine.querySelector(
        'de-audio-item[id="audio-b"] audio',
      ) as HTMLAudioElement;

      return {
        sameElement: before === after,
        rendered: engine.querySelectorAll("de-audio-item").length,
        ids: engine.items().map((item: { id: string }) => item.id),
      };
    });

    expect(result.sameElement).toBe(true);
    expect(result.rendered).toBe(1);
    expect(result.ids).toEqual(["audio-b"]);
  });

  it("releases the media resource when an item is removed", async () => {
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

      const itemEl = engine.querySelector(
        'de-audio-item[id="audio-a"]',
      ) as HTMLElement;
      const audioEl = itemEl.querySelector("audio") as HTMLAudioElement;

      itemEl.remove();

      // The unload is scheduled on a microtask so a plain DOM move (disconnect
      // + reconnect in the same task) doesn't interrupt playback.
      await Promise.resolve();

      const src = audioEl.getAttribute("src");
      const sources = Array.from(audioEl.querySelectorAll("source"));

      return {
        connected: audioEl.isConnected,
        paused: audioEl.paused,
        usesNetworkSource: !!src && !src.startsWith("data:"),
        sourceSrcs: sources.map((s) => s.getAttribute("src")),
      };
    });

    expect(result.connected).toBe(false);
    expect(result.paused).toBe(true);
    expect(result.usesNetworkSource).toBe(false);
    expect(result.sourceSrcs).toEqual([null]);
  });
});
