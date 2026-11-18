// @ts-nocheck -- the chain builders and DOM wiring are untyped

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/configurator/output carry-over chains", () => {
  it("carries local data into every record-level output chain", async () => {
    const result = await testWeb(async () => {
      const idb = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const { CLASS: Configurator } = await import(
        "~/components/configurator/output/element.js"
      );

      await import(
        "~/components/transformer/output/bytes/dasl-sync/element.js"
      );

      const track = {
        $type: "sh.diffuse.output.track",
        id: "t1",
        uri: "https://example.com/t1.mp3",
      };
      const facet = {
        $type: "sh.diffuse.output.facet",
        id: "f1",
        name: "Facet",
      };

      /**
       * Every transformer renders its own local cache; only its backing store
       * needs to exist. A raw indexed-db implements the standard output
       * interface, which is all these layers are allowed to rely on.
       */
      const makeRemote = (root, suffix) => {
        const remote = new idb.CLASS();
        remote.id = `remote-${suffix}`;
        remote.setAttribute("namespace", `remote-${suffix}`);
        root.append(remote);
        return remote;
      };

      const builders = [
        ["dasl-sync", (root, suffix) => {
          const remote = makeRemote(root, suffix);
          const option = document.createElement("dtob-dasl-sync");
          option.id = `option-${suffix}`;
          option.setAttribute("namespace", `option-${suffix}`);
          option.setAttribute("output-selector", `#${remote.id}`);
          root.append(option);
          return option;
        }],
      ];

      const outcomes = [];

      for (const [name, build] of builders) {
        const suffix = crypto.randomUUID();
        const root = document.createElement("div");

        const def = new idb.CLASS();
        def.id = `def-${suffix}`;
        def.setAttribute("namespace", `def-${suffix}`);

        const option = await build(root, suffix);

        const configurator = new Configurator();
        configurator.setAttribute("default", def.id);
        configurator.append(def, option);

        root.append(configurator);
        document.body.append(root);

        await def.tracks.save([track]);
        await def.facets.save([facet]);

        try {
          await configurator.select(option.id);

          const deadline = Date.now() + 8_000;
          let col = configurator.tracks.collection();
          while (
            Date.now() < deadline &&
            !(col.state === "loaded" && Array.isArray(col.data) &&
              col.data.length > 0)
          ) {
            await new Promise((resolve) => setTimeout(resolve, 25));
            col = configurator.tracks.collection();
          }

          outcomes.push({
            name,
            tracks: col.state === "loaded"
              ? col.data.map((/** @type {{ id: string }} */ t) => t.id)
              : col.state,
          });
        } catch (err) {
          outcomes.push({
            name,
            error: err instanceof Error ? err.message : String(err),
          });
        }

        root.remove();
      }

      return outcomes;
    });

    expect(result).toEqual([
      { name: "dasl-sync", tracks: ["t1"] },
    ]);
  });
});
