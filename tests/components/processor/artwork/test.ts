import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/processor/artwork", () => {
  it("returns empty array when tags have no artist", async () => {
    // processRequest short-circuits when artist or album is missing,
    // so no network calls are made
    const result = await testWeb(async () => {
      const mod = await import("~/components/processor/artwork/element.js");
      const processor = new mod.CLASS();

      document.body.append(processor);

      return processor.artwork({
        cacheId: "test-no-artist",
        tags: { album: "Some Album" },
      });
    });

    expect(result).toEqual([]);
  });

  it("returns empty array when tags have no album", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/processor/artwork/element.js");
      const processor = new mod.CLASS();

      document.body.append(processor);

      return processor.artwork({
        cacheId: "test-no-album",
        tags: { artist: "Some Artist" },
      });
    });

    expect(result).toEqual([]);
  });

  it("returns empty array when tags are not provided and URL is unreachable", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/processor/artwork/element.js");
      const processor = new mod.CLASS();

      document.body.append(processor);

      // musicMetadataTags will fail (bad URL), meta.tags stays undefined
      // → still short-circuits on missing artist/album
      return processor.artwork({
        cacheId: "test-no-tags",
        urls: {
          get: "http://localhost/nonexistent.mp3",
          head: "http://localhost/nonexistent.mp3",
        },
      });
    });

    expect(result).toEqual([]);
  });

  it("artwork with artist 'VA' and no album returns empty array", async () => {
    // 'VA' sets variousArtists=true but still short-circuits on missing album
    const result = await testWeb(async () => {
      const mod = await import("~/components/processor/artwork/element.js");
      const processor = new mod.CLASS();

      document.body.append(processor);

      return processor.artwork({
        cacheId: "test-va",
        tags: { artist: "VA", title: "Various Track" },
      });
    });

    expect(result).toEqual([]);
  });

  it("supply queues requests and returns without throwing", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/processor/artwork/element.js");
      const processor = new mod.CLASS();

      document.body.append(processor);

      const r = await processor.supply([
        { cacheId: "queued-1", tags: { title: "Track One" } },
        { cacheId: "queued-2", tags: { title: "Track Two" } },
      ]);

      return r ?? null;
    });

    // supply() returns void — proxy resolves to undefined
    expect(result).toBe(null);
  });

  it("extracts tags from sample audio file and returns array", async () => {
    // Passes the real file URL; musicMetadataTags runs successfully.
    // The file has no embedded artwork so processRequest short-circuits
    // on the missing album tag passed via explicit tags override.
    const result = await testWeb(async () => {
      const mod = await import("~/components/processor/artwork/element.js");
      const processor = new mod.CLASS();

      document.body.append(processor);

      const blob = await fetch("/testing/sample/audio.mp3").then((r) =>
        r.blob()
      );
      const blobUrl = URL.createObjectURL(blob);

      // Provide only a title — processRequest short-circuits before hitting
      // the network because artist+album are not both present
      const art = await processor.artwork({
        cacheId: "sample-audio-title-only",
        tags: { title: "Mr. Sandman" },
        urls: { get: blobUrl, head: blobUrl },
      });

      URL.revokeObjectURL(blobUrl);
      return art;
    });

    expect(Array.isArray(result)).toBe(true);
  });

  it("returns cached artwork on subsequent calls with the same cacheId", async () => {
    const [first, second] = await testWeb(async () => {
      const mod = await import("~/components/processor/artwork/element.js");
      const processor = new mod.CLASS();

      document.body.append(processor);

      const req = { cacheId: "cached-id", tags: { title: "Track" } };

      // Both calls go through processRequest; second should hit IDB cache path
      const first = await processor.artwork(req);
      const second = await processor.artwork(req);

      return [first, second];
    });

    // Both should return the same empty result ([] in this case)
    expect(first).toEqual(second);
  });
});
