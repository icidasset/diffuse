import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/transformer/output/refiner/initial-contents", () => {
  it("ready returns false when no output is connected", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/refiner/initial-contents/element.js"
      );
      const t = new mod.CLASS();
      return t.ready();
    });

    expect(result).toBe(false);
  });

  it("facets.collection is loading when no output is connected", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/refiner/initial-contents/element.js"
      );
      const t = new mod.CLASS();
      return t.facets.collection().state;
    });

    expect(result).toBe("loading");
  });

  it("facets.collection returns default facets when backing output is empty and not yet initialized", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/refiner/initial-contents/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-idb";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-idb");
      document.body.append(t);

      // Save empty to backing so it transitions to "loaded" state
      await output.facets.save([]);

      // Wait for the IDB flag check in the constructor to resolve
      await new Promise((r) => setTimeout(r, 50));

      const col = t.facets.collection();
      if (col.state !== "loaded") return null;
      return (col.data as unknown[]).length > 0;
    });

    // Should return default facets (non-empty) since not yet initialized
    expect(result).toBe(true);
  });

  it("facets.collection returns empty array after explicit save([]) marks as initialized", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/refiner/initial-contents/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-idb-init";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-idb-init");
      document.body.append(t);

      // Wait for the IDB flag check to resolve
      await new Promise((r) => setTimeout(r, 50));

      // Explicit save marks as initialized (even with empty array)
      await t.facets.save([]);

      const col = t.facets.collection();
      if (col.state !== "loaded") return null;
      return (col.data as unknown[]).length;
    });

    expect(result).toBe(0);
  });

  it("facets.collection returns provided facets after save with data", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/refiner/initial-contents/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-idb-data";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-idb-data");
      document.body.append(t);

      await new Promise((r) => setTimeout(r, 50));

      await t.facets.save([
        {
          $type: "sh.diffuse.output.facet",
          id: "my-facet",
          name: "My Playlist",
        },
      ]);

      const col = t.facets.collection();
      if (col.state !== "loaded") return null;
      return (col.data as Array<{ id: string }>).map((f) => f.id);
    });

    expect(result).toEqual(["my-facet"]);
  });

  it("tracks and playlistItems delegate to backing output unchanged", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/refiner/initial-contents/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-idb-tracks";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-idb-tracks");
      document.body.append(t);

      await t.tracks.save([
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri: "https://example.com/track.mp3",
        },
      ]);

      const col = t.tracks.collection();
      if (col.state !== "loaded") return null;
      return (col.data as Array<{ id: string }>).map((tr) => tr.id);
    });

    expect(result).toEqual(["t1"]);
  });
});
