import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";
import { trackA, trackB, tracks } from "~/testing/sample/tracks.js";

describe("components/processor/search", () => {
  it("finds tracks by album", async () => {
    const results = await testWeb(async () => {
      const SearchProcessor = await import(
        "~/components/processor/search/element.js"
      );
      const processor = new SearchProcessor.CLASS();

      document.body.append(processor);

      // Add sample tracks to the supply first
      const { tracks } = await import("~/testing/sample/tracks.js");
      await processor.supply({ tracks });

      // Search for a specific term
      return processor.search({ term: tracks[1]?.tags?.album });
    });

    expect(results[0]?.id).toBe(trackB.id);
  });

  it("finds tracks by artist", async () => {
    const results = await testWeb(async () => {
      const SearchProcessor = await import(
        "~/components/processor/search/element.js"
      );
      const processor = new SearchProcessor.CLASS();

      document.body.append(processor);

      // Add sample tracks to the supply first
      const { tracks } = await import("~/testing/sample/tracks.js");
      await processor.supply({ tracks });

      // Search for a specific term
      return processor.search({ term: tracks[0]?.tags?.artist });
    });

    expect(results[0]?.id).toBe(trackA.id);
  });

  it("finds tracks by title", async () => {
    const results = await testWeb(async () => {
      const SearchProcessor = await import(
        "~/components/processor/search/element.js"
      );
      const processor = new SearchProcessor.CLASS();

      document.body.append(processor);

      // Add sample tracks to the supply first
      const { tracks } = await import("~/testing/sample/tracks.js");
      await processor.supply({ tracks });

      // Search for a specific term
      return processor.search({ term: tracks[1]?.tags?.title });
    });

    expect(results[0]?.id).toBe(trackB.id);
  });

  it("returns empty array when no tracks match the search term", async () => {
    const results = await testWeb(async () => {
      const SearchProcessor = await import(
        "~/components/processor/search/element.js"
      );
      const processor = new SearchProcessor.CLASS();

      document.body.append(processor);

      const { tracks } = await import("~/testing/sample/tracks.js");
      await processor.supply({ tracks });

      return processor.search({ term: "zzz-no-match-zzz" });
    });

    expect(results).toEqual([]);
  });

  it("supplyFingerprint is undefined before first supply", async () => {
    const fp = await testWeb(async () => {
      const SearchProcessor = await import(
        "~/components/processor/search/element.js"
      );
      const processor = new SearchProcessor.CLASS();

      document.body.append(processor);

      return (await processor.supplyFingerprint()) ?? null;
    });

    expect(fp).toBe(null);
  });

  it("supplyFingerprint is set after supply", async () => {
    const fp = await testWeb(async () => {
      const SearchProcessor = await import(
        "~/components/processor/search/element.js"
      );
      const processor = new SearchProcessor.CLASS();

      document.body.append(processor);

      const { tracks } = await import("~/testing/sample/tracks.js");
      await processor.supply({ tracks });

      return processor.supplyFingerprint() ?? null;
    });

    expect(fp).not.toBe(null);
    expect(typeof fp).toBe("string");
  });

  it("supply with same tracks does not change the fingerprint", async () => {
    const [fp1, fp2] = await testWeb(async () => {
      const SearchProcessor = await import(
        "~/components/processor/search/element.js"
      );
      const processor = new SearchProcessor.CLASS();

      document.body.append(processor);

      const { tracks } = await import("~/testing/sample/tracks.js");
      await processor.supply({ tracks });
      const fp1 = processor.supplyFingerprint();

      await processor.supply({ tracks });
      const fp2 = processor.supplyFingerprint();

      return [fp1, fp2];
    });

    expect(fp1).toBe(fp2);
  });

  it("supply removes tracks no longer in the list", async () => {
    const results = await testWeb(async () => {
      const SearchProcessor = await import(
        "~/components/processor/search/element.js"
      );
      const processor = new SearchProcessor.CLASS();

      document.body.append(processor);

      const { trackA, trackB } = await import("~/testing/sample/tracks.js");

      // Supply both tracks then narrow to only trackB
      await processor.supply({ tracks: [trackA, trackB] });
      await processor.supply({ tracks: [trackB] });

      // trackA's artist "Artist" is absent from trackB — must return empty
      return processor.search({ term: "Artist" });
    });

    expect(results).toEqual([]);
  });

  it("supply with empty list clears all tracks from the index", async () => {
    const results = await testWeb(async () => {
      const SearchProcessor = await import(
        "~/components/processor/search/element.js"
      );
      const processor = new SearchProcessor.CLASS();

      document.body.append(processor);

      const { tracks } = await import("~/testing/sample/tracks.js");
      await processor.supply({ tracks });
      await processor.supply({ tracks: [] });

      return processor.search({ term: "Sample" });
    });

    expect(results).toEqual([]);
  });

  it("sorts results by artist alphabetically", async () => {
    const ids = await testWeb(async () => {
      const SearchProcessor = await import(
        "~/components/processor/search/element.js"
      );
      const processor = new SearchProcessor.CLASS();

      document.body.append(processor);

      const testTracks = [
        {
          $type: "sh.diffuse.output.track" as const,
          id: "sort-zebra",
          uri: "diffuse://sort-zebra",
          tags: { artist: "Zebra", title: "Sort Test" },
        },
        {
          $type: "sh.diffuse.output.track" as const,
          id: "sort-apple",
          uri: "diffuse://sort-apple",
          tags: { artist: "Apple", title: "Sort Test" },
        },
        {
          $type: "sh.diffuse.output.track" as const,
          id: "sort-mango",
          uri: "diffuse://sort-mango",
          tags: { artist: "Mango", title: "Sort Test" },
        },
      ];

      await processor.supply({ tracks: testTracks });
      const results = await processor.search({ term: "Sort Test" });
      return results.map((t) => t.id);
    });

    expect(ids).toEqual(["sort-apple", "sort-mango", "sort-zebra"]);
  });

  it("sorts results by track number within the same album", async () => {
    const ids = await testWeb(async () => {
      const SearchProcessor = await import(
        "~/components/processor/search/element.js"
      );
      const processor = new SearchProcessor.CLASS();

      document.body.append(processor);

      const testTracks = [
        {
          $type: "sh.diffuse.output.track" as const,
          id: "track-03",
          uri: "diffuse://track-03",
          tags: { artist: "Band", album: "Album", track: { no: 3 }, title: "C" },
        },
        {
          $type: "sh.diffuse.output.track" as const,
          id: "track-01",
          uri: "diffuse://track-01",
          tags: { artist: "Band", album: "Album", track: { no: 1 }, title: "A" },
        },
        {
          $type: "sh.diffuse.output.track" as const,
          id: "track-02",
          uri: "diffuse://track-02",
          tags: { artist: "Band", album: "Album", track: { no: 2 }, title: "B" },
        },
      ];

      await processor.supply({ tracks: testTracks });
      const results = await processor.search({ term: "Band" });
      return results.map((t) => t.id);
    });

    expect(ids).toEqual(["track-01", "track-02", "track-03"]);
  });
});
