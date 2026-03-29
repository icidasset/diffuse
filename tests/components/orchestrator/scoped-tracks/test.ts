import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";
import { trackA, trackB, tracks } from "~/testing/sample/tracks.js";

describe("components/orchestrator/scoped-tracks", () => {
  it("finds tracks by album", async () => {
    const results = await testWeb(async () => {
      const { supply, search } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      await supply({ tracks });
      return search({ term: tracks[1]?.tags?.album });
    });

    expect(results[0]?.id).toBe(trackB.id);
  });

  it("finds tracks by artist", async () => {
    const results = await testWeb(async () => {
      const { supply, search } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      await supply({ tracks });
      return search({ term: tracks[0]?.tags?.artist });
    });

    expect(results[0]?.id).toBe(trackA.id);
  });

  it("finds tracks by title", async () => {
    const results = await testWeb(async () => {
      const { supply, search } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      await supply({ tracks });
      return search({ term: tracks[1]?.tags?.title });
    });

    expect(results[0]?.id).toBe(trackB.id);
  });

  it("returns empty array when no tracks match the search term", async () => {
    const results = await testWeb(async () => {
      const { supply, search } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      await supply({ tracks });
      return search({ term: "zzz-no-match-zzz" });
    });

    expect(results).toEqual([]);
  });

  it("supplyFingerprint is undefined before first supply", async () => {
    const fp = await testWeb(async () => {
      const { $supplyFingerprint } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );

      return $supplyFingerprint.value ?? null;
    });

    expect(fp).toBe(null);
  });

  it("supplyFingerprint is set after supply", async () => {
    const fp = await testWeb(async () => {
      const { supply, $supplyFingerprint } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      await supply({ tracks });
      return $supplyFingerprint.value ?? null;
    });

    expect(fp).not.toBe(null);
    expect(typeof fp).toBe("string");
  });

  it("supply with same tracks does not change the fingerprint", async () => {
    const [fp1, fp2] = await testWeb(async () => {
      const { supply, $supplyFingerprint } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      await supply({ tracks });
      const fp1 = $supplyFingerprint.value;

      await supply({ tracks });
      const fp2 = $supplyFingerprint.value;

      return [fp1, fp2];
    });

    expect(fp1).toBe(fp2);
  });

  it("supply removes tracks no longer in the list", async () => {
    const results = await testWeb(async () => {
      const { supply, search } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );
      const { trackA, trackB } = await import("~/testing/sample/tracks.js");

      await supply({ tracks: [trackA, trackB] });
      await supply({ tracks: [trackB] });

      return search({ term: "Artist" });
    });

    expect(results).toEqual([]);
  });

  it("supply with empty list clears all tracks from the index", async () => {
    const results = await testWeb(async () => {
      const { supply, search } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      await supply({ tracks });
      await supply({ tracks: [] });

      return search({ term: "Sample" });
    });

    expect(results).toEqual([]);
  });

  it("sorts results by artist alphabetically", async () => {
    const ids = await testWeb(async () => {
      const { supply, search } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );

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

      await supply({ tracks: testTracks });
      const results = await search({ term: "Sort Test" });
      return results.map((t) => t.id);
    });

    expect(ids).toEqual(["sort-apple", "sort-mango", "sort-zebra"]);
  });

  it("sorts results by track number within the same album", async () => {
    const ids = await testWeb(async () => {
      const { supply, search } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );

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

      await supply({ tracks: testTracks });
      const results = await search({ term: "Band" });
      return results.map((t) => t.id);
    });

    expect(ids).toEqual(["track-01", "track-02", "track-03"]);
  });
});
