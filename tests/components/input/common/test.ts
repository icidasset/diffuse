import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";
import type { Track } from "~/definitions/types.d.ts";

describe("components/input/common", () => {
  it("isAudioFile returns truthy for audio extensions", async () => {
    const results = await testWeb(async () => {
      const mod = await import("~/components/input/common.js");
      return [
        !!mod.isAudioFile("track.mp3"),
        !!mod.isAudioFile("track.flac"),
        !!mod.isAudioFile("track.ogg"),
        !!mod.isAudioFile("track.opus"),
        !!mod.isAudioFile("track.wav"),
        !!mod.isAudioFile("track.m4a"),
        !!mod.isAudioFile("track.webm"),
      ];
    });

    expect(results).toEqual([true, true, true, true, true, true, true]);
  });

  it("isAudioFile returns falsy for non-audio extensions", async () => {
    const results = await testWeb(async () => {
      const mod = await import("~/components/input/common.js");
      return [
        !!mod.isAudioFile("track.txt"),
        !!mod.isAudioFile("track.jpg"),
        !!mod.isAudioFile("track.pdf"),
        !!mod.isAudioFile("track"),
      ];
    });

    expect(results).toEqual([false, false, false, false]);
  });

  it("groupKey returns scheme://groupId", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/common.js");
      return mod.groupKey("https", "example.com");
    });

    expect(result).toBe("https://example.com");
  });

  it("detach with matching scheme removes all tracks", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/common.js");

      const tracks: Track[] = [
        { $type: "sh.diffuse.output.track", id: "1", uri: "https://a.com/1.mp3" },
        { $type: "sh.diffuse.output.track", id: "2", uri: "https://b.com/2.mp3" },
      ];

      return mod.detach({
        fileUriOrScheme: "https",
        inputScheme: "https",
        handleFileUri: () => [],
        tracks,
      });
    });

    expect(result).toEqual([]);
  });

  it("detach with non-matching scheme returns all tracks unchanged", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/common.js");

      const tracks: Track[] = [
        { $type: "sh.diffuse.output.track", id: "1", uri: "https://a.com/1.mp3" },
        { $type: "sh.diffuse.output.track", id: "2", uri: "https://b.com/2.mp3" },
      ];

      return mod.detach({
        fileUriOrScheme: "ftp",
        inputScheme: "https",
        handleFileUri: () => [],
        tracks,
      });
    });

    expect(result.map((t: Track) => t.id)).toEqual(["1", "2"]);
  });

  it("detach delegates to handleFileUri when a full URI is given", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/common.js");

      const tracks: Track[] = [
        { $type: "sh.diffuse.output.track", id: "1", uri: "https://a.com/1.mp3" },
        { $type: "sh.diffuse.output.track", id: "2", uri: "https://b.com/2.mp3" },
      ];

      return mod.detach({
        fileUriOrScheme: "https://a.com/1.mp3",
        inputScheme: "https",
        handleFileUri: ({ tracks }) => tracks.filter((t) => t.id !== "1"),
        tracks,
      });
    });

    expect(result.map((t: Track) => t.id)).toEqual(["2"]);
  });

  it("cachedConsult caches and returns result", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/common.js");

      let callCount = 0;
      const cached = mod.cachedConsult(
        async (_uri: string) => {
          callCount++;
          return true;
        },
        (uri: string) => uri,
      );

      const r1 = await cached("https://example.com/stream");
      const r2 = await cached("https://example.com/stream");

      return { r1, r2, callCount };
    });

    expect(result.r1).toBe(true);
    expect(result.r2).toBe(true);
    expect(result.callCount).toBe(1);
  });
});
