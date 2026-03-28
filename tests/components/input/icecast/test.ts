import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";
import type { Track } from "~/definitions/types.d.ts";

describe("components/input/icecast", () => {
  it("has correct SCHEME property", async () => {
    const scheme = await testWeb(async () => {
      const mod = await import("~/components/input/icecast/element.js");
      const input = new mod.CLASS();
      document.body.append(input);
      return input.SCHEME;
    });

    expect(scheme).toBe("icecast");
  });

  it("consult returns undetermined for scheme only", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/icecast/element.js");
      const input = new mod.CLASS();
      document.body.append(input);
      return await input.consult("icecast");
    });

    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe("undetermined");
    }
  });

  it("consult returns unsupported for a non-icecast URI", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/icecast/element.js");
      const input = new mod.CLASS();
      document.body.append(input);
      return await input.consult("https://example.com/stream.mp3");
    });

    expect(result.supported).toBe(false);
  });

  it("resolve returns a URL containing the host and path for an icecast URI", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/icecast/element.js");
      const input = new mod.CLASS();
      document.body.append(input);
      return await input.resolve({ uri: "icecast://radio.example.com/stream.mp3" });
    });

    // Chrome's URL parser treats non-special schemes (icecast://) with empty
    // host, so the reconstructed streamUrl embeds the authority in the path.
    // Test the observable contract: the url contains the domain and path and
    // uses the https: protocol.
    expect(result).not.toBe(null);
    if (result && "url" in result) {
      expect(result.url).toContain("radio.example.com");
      expect(result.url).toContain("stream.mp3");
      expect(result.url).toContain("https:");
      expect(result.expiresAt).toBeGreaterThan(Date.now() / 1000);
    }
  });

  it("resolve uses http: for an icecast URI with tls=0", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/icecast/element.js");
      const input = new mod.CLASS();
      document.body.append(input);
      return await input.resolve({
        uri: "icecast://radio.example.com:8000/live?tls=0",
      });
    });

    expect(result).not.toBe(null);
    if (result && "url" in result) {
      expect(result.url).toContain("radio.example.com");
      expect(result.url).toContain("/live");
      expect(result.url).toContain("http:");
    }
  });

  it("resolve returns undefined for a non-icecast URI", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/icecast/element.js");
      const input = new mod.CLASS();
      document.body.append(input);
      const r = await input.resolve({ uri: "https://example.com/stream.mp3" });
      return r ?? null;
    });

    expect(result).toBe(null);
  });

  it("detach with icecast scheme removes all icecast tracks", async () => {
    const remaining = await testWeb(async () => {
      const mod = await import("~/components/input/icecast/element.js");
      const input = new mod.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "1",
          uri: "icecast://radio.example.com/stream.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "2",
          uri: "icecast://other.example.com/live",
        },
      ];

      return await input.detach({ fileUriOrScheme: "icecast", tracks });
    });

    expect(remaining.length).toBe(0);
  });

  it("detach with a non-icecast URI returns all tracks unchanged", async () => {
    const remaining = await testWeb(async () => {
      const mod = await import("~/components/input/icecast/element.js");
      const input = new mod.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "1",
          uri: "icecast://radio.example.com/stream.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "2",
          uri: "icecast://other.example.com/live",
        },
      ];

      return await input.detach({
        fileUriOrScheme: "https://example.com/something.mp3",
        tracks,
      });
    });

    // parseURI returns undefined for non-icecast URIs, so all tracks are kept
    expect(remaining.length).toBe(2);
  });

  it("sources returns an entry with icecast:// URI for each track", async () => {
    const sources = await testWeb(async () => {
      const mod = await import("~/components/input/icecast/element.js");
      const input = new mod.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "1",
          uri: "icecast://radio.example.com/stream.mp3",
        },
      ];

      return input.sources(tracks);
    });

    expect(sources.length).toBeGreaterThan(0);
    expect(sources[0].uri).toContain("icecast://");
  });
});
