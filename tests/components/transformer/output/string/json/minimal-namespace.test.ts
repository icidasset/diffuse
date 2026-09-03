import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

// Fast save with a NAMESPACED indexed-db (as the orchestrator uses namespace="json").
describe("minimal namespace fast save", () => {
  it("saving tracks fast after facets keeps both (namespaced idb)", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/string/json/element.js"
      );
      const output = new idbMod.CLASS();
      output.id = "ns-idb";
      output.setAttribute("namespace", "json");
      document.body.append(output);
      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#ns-idb");
      document.body.append(t);

      await t.facets.save([
        { $type: "sh.diffuse.output.facet", id: "f1", name: "Keep me" },
      ]);
      await t.tracks.save([
        { $type: "sh.diffuse.output.track", id: "t1", uri: "https://a.com/t1.mp3" },
      ]);
      await new Promise((r) => setTimeout(r, 150));

      const f = t.facets.collection();
      const tr = t.tracks.collection();
      return {
        f: f.state === "loaded" ? JSON.stringify(f.data) : f.state,
        tr: tr.state === "loaded" ? JSON.stringify(tr.data) : tr.state,
      };
    });
    console.log("NS_DIAG", JSON.stringify(result));
    expect(result?.f).toContain("f1");
    expect(result?.tr).toContain("t1");
  });
});