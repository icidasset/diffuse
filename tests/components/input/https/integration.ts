import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import type { Track } from "~/definitions/types.d.ts";
import * as Worker from "~/components/input/https/worker.js";

describe("components/input/https (integration)", () => {
  it("resolve returns the URL as-is with a far-future expiry", async () => {
    const resolved = await Worker.resolve({
      uri: "https://example.com/audio.mp3",
    });
    expect(resolved).not.toBe(undefined);
    if (resolved && "url" in resolved) {
      expect(resolved.url).toBe("https://example.com/audio.mp3");
      expect(resolved.expiresAt).toBeGreaterThan(Date.now() / 1000);
    }
  });

  it("resolve passes through blob: URLs unchanged", async () => {
    const resolved = await Worker.resolve({
      uri: "blob:https://example.com/123-456",
    });
    expect(resolved).not.toBe(undefined);
    if (resolved && "url" in resolved) {
      expect(resolved.url).toBe("blob:https://example.com/123-456");
    }
  });

  it("resolve returns undefined for a non-HTTPS URI", async () => {
    const resolved = await Worker.resolve({ uri: "http://example.com/audio.mp3" });
    expect(resolved).toBe(undefined);
  });

  it("consult returns false for an unreachable host", async () => {
    // Port 1 on localhost — nothing listens, connection refused.
    const result = await Worker.consult("https://127.0.0.1:1/audio.mp3");
    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe(false);
    }
  });

  it("consult returns false for a non-HTTPS URL", async () => {
    const result = await Worker.consult("http://example.com/audio.mp3");
    expect(result.supported).toBe(false);
  });

  it("list clears placeholder kind from cached tracks", async () => {
    const tracks: Track[] = [
      {
        $type: "sh.diffuse.output.track",
        id: "t1",
        uri: "https://example.com/a.mp3",
        kind: "placeholder",
      },
    ];
    const refreshed = await Worker.list(tracks);
    expect(refreshed[0].kind).toBe(undefined);
  });

  it("list preserves existing non-placeholder kind", async () => {
    const tracks: Track[] = [
      {
        $type: "sh.diffuse.output.track",
        id: "t1",
        uri: "https://example.com/a.mp3",
        kind: "music",
      },
    ];
    const refreshed = await Worker.list(tracks);
    expect(refreshed[0].kind).toBe("music");
  });

  it("groupConsult groups URIs by host and reports availability", async () => {
    const result = await Worker.groupConsult([
      "https://127.0.0.1:1/a.mp3",
      "https://127.0.0.1:1/b.mp3",
    ]);
    const key = "https://127.0.0.1:1";
    expect(result[key]?.available).toBe(false);
    if (!result[key]?.available) {
      expect(result[key]?.reason).toBeDefined();
    }
    expect(result[key]?.uris).toEqual([
      "https://127.0.0.1:1/a.mp3",
      "https://127.0.0.1:1/b.mp3",
    ]);
  });

  it("detach with scheme removes all HTTPS tracks", async () => {
    const tracks: Track[] = [
      { $type: "sh.diffuse.output.track", id: "1", uri: "https://a.com/1.mp3" },
      { $type: "sh.diffuse.output.track", id: "2", uri: "https://b.com/2.mp3" },
    ];
    const remaining = await Worker.detach({ fileUriOrScheme: "https", tracks });
    expect(remaining.length).toBe(0);
  });

  it("detach with a specific host URI removes only that host's tracks", async () => {
    const tracks: Track[] = [
      { $type: "sh.diffuse.output.track", id: "1", uri: "https://example.com/a.mp3" },
      { $type: "sh.diffuse.output.track", id: "2", uri: "https://cdn.example.com/b.mp3" },
      { $type: "sh.diffuse.output.track", id: "3", uri: "https://example.com/c.mp3" },
    ];
    const remaining = await Worker.detach({
      fileUriOrScheme: "https://example.com/a.mp3",
      tracks,
    });
    // detach by URI removes the entire host group ("example.com")
    expect(remaining.length).toBe(1);
    expect(remaining[0].id).toBe("2");
  });

  it("detach with non-HTTPS scheme returns all tracks", async () => {
    const tracks: Track[] = [
      { $type: "sh.diffuse.output.track", id: "1", uri: "https://example.com/a.mp3" },
    ];
    const remaining = await Worker.detach({ fileUriOrScheme: "icecast", tracks });
    expect(remaining.length).toBe(1);
  });
});
