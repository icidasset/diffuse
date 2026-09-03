import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

// Regression: saving a JSON-encoded collection through string/json over
// indexed-db must persist WITHOUT reading back the collection synchronously
// (which triggered a load race that clobbered the just-saved value). Saving one
// collection must also not wipe the others.
describe("string/json save isolation over indexed-db", () => {
  it("saving tracks persists them and does not wipe facets", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/string/json/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "save-isolation-idb";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#save-isolation-idb");
      document.body.append(t);

      // Pre-existing facets.
      await t.facets.save([
        { $type: "sh.diffuse.output.facet", id: "f1", name: "Keep me" },
      ]);

      // Import tracks.
      await t.tracks.save([
        { $type: "sh.diffuse.output.track", id: "t1", uri: "https://a.com/t1.mp3" },
      ]);

      const facets = t.facets.collection();
      const tracks = t.tracks.collection();
      return {
        facets:
          facets.state === "loaded" && Array.isArray(facets.data)
            ? (facets.data as any[]).map((f: any) => f.id)
            : null,
        tracks:
          tracks.state === "loaded" && Array.isArray(tracks.data)
            ? (tracks.data as any[]).map((x: any) => x.id)
            : null,
      };
    });

    expect(result?.facets).toEqual(["f1"]);
    expect(result?.tracks).toEqual(["t1"]);
  });
});