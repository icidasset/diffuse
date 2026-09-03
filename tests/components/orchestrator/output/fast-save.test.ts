import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

// Fast timing (no settle between saves) through the do-output orchestrator.
// Regression target: saving tracks right after facets must not make the
// orchestators' facets read back empty while storage has it.
describe("do-output fast save", () => {
  it("saving tracks fast after facets keeps both", async () => {
    const result = await testWeb(async () => {
      const { CLASS } = await import(
        "~/components/orchestrator/output/element.js"
      );
      const orch = new CLASS();
      document.body.append(orch);
      await customElements.whenDefined("dc-output");
      const output = orch.output;

      await orch.facets.save([
        { $type: "sh.diffuse.output.facet", id: "f1", name: "Keep me" },
      ]);
      await orch.tracks.save([
        { $type: "sh.diffuse.output.track", id: "t1", uri: "https://a.com/t1.mp3" },
      ]);
      await new Promise((r) => setTimeout(r, 100));

      const f = orch.facets.collection();
      const tr = orch.tracks.collection();
      return {
        f: f.state === "loaded" ? JSON.stringify(f.data) : f.state,
        tr: tr.state === "loaded" ? JSON.stringify(tr.data) : tr.state,
      };
    });

    console.log("ORCH_FAST_RESULT", JSON.stringify(result));
    expect(result?.f).toContain("f1");
    expect(result?.tr).toContain("t1");
  });
});