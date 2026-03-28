import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/processor/metadata", () => {
  it("returns empty extraction when no urls or stream are provided", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/processor/metadata/element.js");
      const processor = new mod.CLASS();

      document.body.append(processor);

      // No urls/stream — musicMetadataTags throws, worker catches and returns {}
      return processor.supply({});
    });

    expect(result).toEqual({});
  });

  it("returns empty extraction when the URL is unreachable", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/processor/metadata/element.js");
      const processor = new mod.CLASS();

      document.body.append(processor);

      return processor.supply({
        urls: {
          get: "http://localhost/nonexistent-audio-file.mp3",
          head: "http://localhost/nonexistent-audio-file.mp3",
        },
      });
    });

    expect(result).toEqual({});
  });

  it("extracts tags from sample audio file", async () => {
    const tags = await testWeb(async () => {
      const mod = await import("~/components/processor/metadata/element.js");
      const processor = new mod.CLASS();
      document.body.append(processor);

      const blob = await fetch("/testing/sample/audio.mp3").then((r) =>
        r.blob()
      );
      const blobUrl = URL.createObjectURL(blob);
      const extraction = await processor.supply({
        urls: { get: blobUrl, head: blobUrl },
      });
      URL.revokeObjectURL(blobUrl);

      return extraction.tags ?? null;
    });

    expect(tags).not.toBe(null);
    expect(tags?.title).toBe("Mr. Sandman");
    expect(tags?.album).toBe("Mr. Sandman");
    expect(tags?.year).toBe(1954);
    expect(tags?.track?.no).toBe(1);
    expect(tags?.artist).toContain("The Chordettes");
  });

  it("extracts stats from sample audio file", async () => {
    const stats = await testWeb(async () => {
      const mod = await import("~/components/processor/metadata/element.js");
      const processor = new mod.CLASS();
      document.body.append(processor);

      const blob = await fetch("/testing/sample/audio.mp3").then((r) =>
        r.blob()
      );
      const blobUrl = URL.createObjectURL(blob);
      const extraction = await processor.supply({
        urls: { get: blobUrl, head: blobUrl },
      });
      URL.revokeObjectURL(blobUrl);

      return extraction.stats ?? null;
    });

    expect(stats).not.toBe(null);
    expect(stats?.bitrate).toBe(143320);
    // Duration is stored in milliseconds; 151.21s ± 500ms
    expect(stats?.duration).toBeGreaterThan(150000);
    expect(stats?.duration).toBeLessThan(152000);
  });
});
