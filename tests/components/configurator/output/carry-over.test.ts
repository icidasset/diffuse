import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/configurator/output carry-over", () => {
  it("copies the default output into an empty selected output", async () => {
    const result = await testWeb(async () => {
      const idb = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const { CLASS } = await import(
        "~/components/configurator/output/element.js"
      );

      const suffix = crypto.randomUUID();

      const def = new idb.CLASS();
      def.id = "carry-over-default";
      def.setAttribute("namespace", `carry-over-default-${suffix}`);

      const selected = new idb.CLASS();
      selected.id = "carry-over-selected";
      selected.setAttribute("namespace", `carry-over-selected-${suffix}`);

      const configurator = new CLASS();
      configurator.setAttribute("default", def.id);
      configurator.append(def, selected);
      document.body.append(configurator);

      await def.tracks.save([{
        $type: "sh.diffuse.output.track",
        id: "t1",
        uri: "https://example.com/t1.mp3",
      }]);
      await def.facets.save([{
        $type: "sh.diffuse.output.facet",
        id: "f1",
        name: "Facet",
      }]);

      await configurator.select(selected.id);

      const deadline = Date.now() + 4_000;
      let tracks = selected.tracks.collection();
      while (
        Date.now() < deadline &&
        !(tracks.state === "loaded" && Array.isArray(tracks.data) &&
          tracks.data.length > 0)
      ) {
        await new Promise((resolve) => setTimeout(resolve, 20));
        tracks = selected.tracks.collection();
      }

      const facets = selected.facets.collection();
      const defaultTracks = def.tracks.collection();

      return {
        selectedTracks: tracks.state === "loaded"
          ? ((tracks.data ?? []) as Array<{ id: string }>).map((t) => t.id)
          : tracks.state,
        selectedFacets: facets.state === "loaded"
          ? ((facets.data ?? []) as Array<{ id: string }>).map((f) => f.id)
          : facets.state,
        defaultTracks: defaultTracks.state === "loaded"
          ? ((defaultTracks.data ?? []) as Array<{ id: string }>).map((t) =>
            t.id
          )
          : defaultTracks.state,
      };
    });

    expect(result?.selectedTracks).toEqual(["t1"]);
    expect(result?.selectedFacets).toEqual(["f1"]);
    // The empty selected output must not have overwritten the local copy.
    expect(result?.defaultTracks).toEqual(["t1"]);
  });

  it("seeds the starting-set facets when the local store has none", async () => {
    const result = await testWeb(async () => {
      const idb = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const { CLASS } = await import(
        "~/components/configurator/output/element.js"
      );

      const suffix = crypto.randomUUID();

      const def = new idb.CLASS();
      def.id = "carry-over-default";
      def.setAttribute("namespace", `carry-over-default-${suffix}`);

      const selected = new idb.CLASS();
      selected.id = "carry-over-selected";
      selected.setAttribute("namespace", `carry-over-selected-${suffix}`);

      const configurator = new CLASS();
      configurator.setAttribute("default", def.id);
      configurator.append(def, selected);
      document.body.append(configurator);

      await def.tracks.save([{
        $type: "sh.diffuse.output.track",
        id: "t1",
        uri: "https://example.com/t1.mp3",
      }]);

      await configurator.select(selected.id);

      const deadline = Date.now() + 4_000;
      let facets = selected.facets.collection();
      while (
        Date.now() < deadline &&
        !(facets.state === "loaded" && Array.isArray(facets.data) &&
          facets.data.length > 0)
      ) {
        await new Promise((resolve) => setTimeout(resolve, 20));
        facets = selected.facets.collection();
      }

      return facets.state === "loaded" ? (facets.data ?? []).length : -1;
    });

    expect(result).toBeGreaterThan(0);
  });

  it("does not carry over into an output that has been initialized", async () => {
    const result = await testWeb(async () => {
      const { markOutputInitialized } = await import(
        "~/common/output.js"
      );
      const idb = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const { CLASS } = await import(
        "~/components/configurator/output/element.js"
      );

      const suffix = crypto.randomUUID();

      const def = new idb.CLASS();
      def.id = "carry-over-default";
      def.setAttribute("namespace", `carry-over-default-${suffix}`);

      const selected = new idb.CLASS();
      selected.id = "carry-over-selected";
      selected.setAttribute("namespace", `carry-over-selected-${suffix}`);

      const configurator = new CLASS();
      configurator.setAttribute("default", def.id);
      configurator.append(def, selected);
      document.body.append(configurator);

      await def.tracks.save([{
        $type: "sh.diffuse.output.track",
        id: "t1",
        uri: "https://example.com/t1.mp3",
      }]);

      // Simulate an output that has been used before (even though it is empty
      // right now), e.g. all its tracks were deleted.
      markOutputInitialized(selected.id);

      await configurator.select(selected.id);

      // Wait for the selected output to load and for the effects to settle.
      const deadline = Date.now() + 4_000;
      let tracks = selected.tracks.collection();
      while (
        Date.now() < deadline && tracks.state !== "loaded"
      ) {
        await new Promise((resolve) => setTimeout(resolve, 20));
        tracks = selected.tracks.collection();
      }
      await new Promise((resolve) => setTimeout(resolve, 100));

      tracks = selected.tracks.collection();
      return tracks.state === "loaded" ? (tracks.data ?? []).length : -1;
    });

    expect(result).toBe(0);
  });
});
