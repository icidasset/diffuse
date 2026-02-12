import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";
import type { Track } from "@definitions/types.d.ts";

describe("components/input/https", () => {
  it("resolves HTTPS URI to same URL", async () => {
    const resolved = await testWeb(async () => {
      const HttpsInput = await import("@components/input/https/element.js");
      const input = new HttpsInput.CLASS();
      document.body.append(input);

      return await input.resolve({
        uri: "https://example.com/audio.mp3",
      });
    });

    if (resolved && "url" in resolved) {
      expect(resolved.url).toBe("https://example.com/audio.mp3");
      expect(resolved.expiresAt).toBeGreaterThan(Date.now() / 1000);
    }
  });

  it("lists only HTTPS tracks from mixed collection", async () => {
    const tracks = await testWeb(async () => {
      const HttpsInput = await import("@components/input/https/element.js");
      const input = new HttpsInput.CLASS();
      document.body.append(input);

      const cachedTracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "1",
          uri: "https://example.com/a.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "2",
          uri: "s3://bucket/b.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "3",
          uri: "https://example.com/c.mp3",
        },
      ];

      return await input.list(cachedTracks);
    });

    expect(tracks.length).toBe(2);
    expect(tracks[0].id).toBe("1");
    expect(tracks[1].id).toBe("3");
  });

  it("provides sources list from tracks", async () => {
    const sources = await testWeb(async () => {
      const HttpsInput = await import("@components/input/https/element.js");
      const input = new HttpsInput.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "1",
          uri: "https://example.com/a.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "2",
          uri: "https://cdn.example.com/b.mp3",
        },
      ];

      return input.sources(tracks);
    });

    expect(sources.length).toBe(2);
    expect(sources[0].label).toBeDefined();
    expect(sources[0].uri).toContain("https://");
    expect(sources[1].label).toBeDefined();
    expect(sources[1].uri).toContain("https://");
  });

  it("consult returns undetermined for scheme only", async () => {
    const result = await testWeb(async () => {
      const HttpsInput = await import("@components/input/https/element.js");
      const input = new HttpsInput.CLASS();
      document.body.append(input);

      return await input.consult("https");
    });

    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe("undetermined");
    }
  });

  it("detaches all HTTPS tracks when given scheme", async () => {
    const remaining = await testWeb(async () => {
      const HttpsInput = await import("@components/input/https/element.js");
      const input = new HttpsInput.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "1",
          uri: "https://example.com/a.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "2",
          uri: "https://example.com/c.mp3",
        },
      ];

      return await input.detach({
        fileUriOrScheme: "https",
        tracks,
      });
    });

    expect(remaining.length).toBe(0);
  });

  it("detaches tracks from specific domain", async () => {
    const remaining = await testWeb(async () => {
      const HttpsInput = await import("@components/input/https/element.js");
      const input = new HttpsInput.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "1",
          uri: "https://example.com/a.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "2",
          uri: "https://cdn.example.com/b.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "3",
          uri: "https://example.com/c.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "4",
          uri: "https://cdn.example.com/d.mp3",
        },
      ];

      return await input.detach({
        fileUriOrScheme: "https://example.com",
        tracks,
      });
    });

    expect(remaining.length).toBe(2);
    expect(remaining[0].id).toBe("2");
    expect(remaining[1].id).toBe("4");
  });

  it("has correct SCHEME property", async () => {
    const scheme = await testWeb(async () => {
      const HttpsInput = await import("@components/input/https/element.js");
      const input = new HttpsInput.CLASS();
      document.body.append(input);

      return input.SCHEME;
    });

    expect(scheme).toBe("https");
  });
});
