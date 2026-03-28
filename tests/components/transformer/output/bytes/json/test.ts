import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/transformer/output/bytes/json", () => {
  it("ready returns false when no output is connected", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/bytes/json/element.js"
      );
      const t = new mod.CLASS();
      return t.ready();
    });

    expect(result).toBe(false);
  });

  it("tracks.collection is loading when no output is connected", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/bytes/json/element.js"
      );
      const t = new mod.CLASS();
      return t.tracks.collection().state;
    });

    expect(result).toBe("loading");
  });

  it("tracks save and collection round-trip", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/bytes/json/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-idb";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-idb");
      document.body.append(t);

      await t.tracks.save([
        {
          $type: "sh.diffuse.output.track",
          id: "track-1",
          uri: "https://example.com/track1.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "track-2",
          uri: "https://example.com/track2.mp3",
        },
      ]);

      const col = t.tracks.collection();
      if (col.state !== "loaded") return null;
      return (col.data as Array<{ id: string }>).map((tr) => tr.id);
    });

    expect(result).toEqual(["track-1", "track-2"]);
  });

  it("facets save and collection round-trip", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/bytes/json/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-idb-facets";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-idb-facets");
      document.body.append(t);

      await t.facets.save([
        {
          $type: "sh.diffuse.output.facet",
          id: "f1",
          name: "Favourites",
        },
      ]);

      const col = t.facets.collection();
      if (col.state !== "loaded") return null;
      return (col.data as Array<{ id: string; name: string }>).map((f) => ({
        id: f.id,
        name: f.name,
      }));
    });

    expect(result).toEqual([{ id: "f1", name: "Favourites" }]);
  });

  it("saving empty array results in empty loaded collection", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/bytes/json/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-idb-empty";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-idb-empty");
      document.body.append(t);

      await t.tracks.save([]);

      const col = t.tracks.collection();
      if (col.state !== "loaded") return null;
      return (col.data as unknown[]).length;
    });

    expect(result).toBe(0);
  });
});
