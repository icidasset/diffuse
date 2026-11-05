// @ts-nocheck -- the evaluated browser function intentionally uses untyped JS

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/output/bytes/wnfs/local", () => {
  it("reads collections through the default output", async () => {
    const result = await testWeb(async () => {
      const { CLASS: Orchestrator } = await import(
        "~/components/orchestrator/output/element.js"
      );
      const { readLocalCollection } = await import(
        "~/components/output/bytes/wnfs/local.js"
      );

      const orchestrator = new Orchestrator();
      document.body.append(orchestrator);

      await customElements.whenDefined("dc-output");
      await new Promise((resolve) => setTimeout(resolve, 100));

      const local = /** @type {any} */ (
        document.querySelector("#do-output__dc-output__local")
      );
      const raw = /** @type {any} */ (
        document.querySelector("#do-output__dop-indexed-db__json")
      );

      await local.facets.save([
        {
          $type: "sh.diffuse.output.facet",
          id: "facet-a",
          name: "keep-me",
          kind: "interactive",
        },
      ]);

      const viaDefault = await readLocalCollection("facets");

      // Read the raw storage layer for comparison: it must expose the encoded
      // JSON string, which is why the seed has to read through the default
      // output element instead.
      const deadline = Date.now() + 2_000;
      let rawCol = raw.facets.collection();
      while (rawCol.state !== "loaded" && Date.now() < deadline) {
        await new Promise((resolve) => setTimeout(resolve, 20));
        rawCol = raw.facets.collection();
      }

      orchestrator.remove();

      return {
        ids: viaDefault.map((facet) => facet.id),
        rawType: rawCol.state === "loaded" ? typeof rawCol.data : rawCol.state,
        rawIsArray: rawCol.state === "loaded" && Array.isArray(rawCol.data),
      };
    });

    expect(result.ids).toEqual(["facet-a"]);
    expect(result.rawType).toBe("string");
    expect(result.rawIsArray).toBe(false);
  });
});
