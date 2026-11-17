import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("do-output carry-over", () => {
  it("copies the local store into a newly selected, empty output", async () => {
    const result = await testWeb(async () => {
      const { CLASS: OutputOrchestrator } = await import(
        "~/components/orchestrator/output/element.js"
      );
      const idb = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );

      const suffix = crypto.randomUUID();

      const orch = new OutputOrchestrator();
      orch.setAttribute("namespace", `carry-over-${suffix}`);
      document.body.append(orch);
      await customElements.whenDefined("dc-output");

      const local = document.querySelector(
        "#do-output__dc-output__local",
      ) as unknown as {
        tracks: {
          collection: () => {
            state: "loading" | "loaded" | "error";
            data?: Array<{ id: string }>;
          };
        };
      };
      if (!local) return null;

      await orch.tracks.save([{
        $type: "sh.diffuse.output.track",
        id: "t1",
        uri: "https://example.com/t1.mp3",
      }]);
      await orch.facets.save([{
        $type: "sh.diffuse.output.facet",
        id: "f1",
        name: "Facet",
      }]);

      // An output option that has never been used.
      const selected = new idb.CLASS();
      selected.id = "do-output__dc-output__carry-over";
      selected.setAttribute("label", "Remote");
      selected.setAttribute("namespace", `carry-over-remote-${suffix}`);
      orch.outputConfigurator.append(selected);

      await orch.select(selected.id);

      const deadline = Date.now() + 4_000;
      let tracks = orch.tracks.collection();
      while (
        Date.now() < deadline &&
        !(tracks.state === "loaded" && Array.isArray(tracks.data) &&
          tracks.data.length > 0)
      ) {
        await new Promise((resolve) => setTimeout(resolve, 20));
        tracks = orch.tracks.collection();
      }

      const facets = orch.facets.collection();
      const localTracks = local.tracks.collection();

      return {
        tracks: tracks.state === "loaded"
          ? ((tracks.data ?? []) as Array<{ id: string }>).map((t) => t.id)
          : tracks.state,
        facets: facets.state === "loaded"
          ? ((facets.data ?? []) as Array<{ id: string }>).map((f) => f.id)
          : facets.state,
        localTracks: localTracks.state === "loaded"
          ? ((localTracks.data ?? []) as Array<{ id: string }>).map((t) => t.id)
          : localTracks.state,
      };
    });

    expect(result?.tracks).toEqual(["t1"]);
    expect(result?.facets).toEqual(["f1"]);
    expect(result?.localTracks).toEqual(["t1"]);
  });
});
