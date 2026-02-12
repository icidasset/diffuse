import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";
import { trackA, trackB } from "@testing/sample/tracks.js";

describe("components/processor/search", () => {
  it("finds tracks by album", async () => {
    const results = await testWeb(async () => {
      const SearchProcessor = await import(
        "@components/processor/search/element.js"
      );
      const processor = new SearchProcessor.CLASS();

      document.body.append(processor);

      // Add sample tracks to the supply first
      const { tracks } = await import("@testing/sample/tracks.js");
      await processor.supply({ tracks });

      // Search for a specific term
      return processor.search({ term: tracks[1]?.tags?.album });
    });

    expect(results[0]?.id).toBe(trackB.id);
  });

  it("finds tracks by artist", async () => {
    const results = await testWeb(async () => {
      const SearchProcessor = await import(
        "@components/processor/search/element.js"
      );
      const processor = new SearchProcessor.CLASS();

      document.body.append(processor);

      // Add sample tracks to the supply first
      const { tracks } = await import("@testing/sample/tracks.js");
      await processor.supply({ tracks });

      // Search for a specific term
      return processor.search({ term: tracks[0]?.tags?.artist });
    });

    expect(results[0]?.id).toBe(trackA.id);
  });

  it("finds tracks by title", async () => {
    const results = await testWeb(async () => {
      const SearchProcessor = await import(
        "@components/processor/search/element.js"
      );
      const processor = new SearchProcessor.CLASS();

      document.body.append(processor);

      // Add sample tracks to the supply first
      const { tracks } = await import("@testing/sample/tracks.js");
      await processor.supply({ tracks });

      // Search for a specific term
      return processor.search({ term: tracks[1]?.tags?.title });
    });

    expect(results[0]?.id).toBe(trackB.id);
  });
});
