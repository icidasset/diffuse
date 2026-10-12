import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/transformer/output/refiner/path-collections", () => {
  // Disconnected defaults

  it("ready returns false when no output is connected", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/refiner/path-collections/element.js"
      );
      const t = new mod.CLASS();
      return t.ready();
    });

    expect(result).toBe(false);
  });

  it("playlistItems.collection is loading when no output is connected", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/refiner/path-collections/element.js"
      );
      const t = new mod.CLASS();
      return t.playlistItems.collection().state;
    });

    expect(result).toBe("loading");
  });

  // Ephemeral item generation

  it("generates one ephemeral item per track from loaded tracks", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/refiner/path-collections/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-idb-paths";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-idb-paths");
      document.body.append(t);

      await t.tracks.save([
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri: "https://example.com/music/track1.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "t2",
          uri: "https://example.com/podcasts/ep1.mp3",
        },
      ]);
      await output.playlistItems.save([]);

      const col = t.playlistItems.collection();
      if (col.state !== "loaded") return null;
      const ephemeral = (col.data as Array<{ ephemeral?: boolean; playlist: string }>)
        .filter((p) => p.ephemeral)
        .map((p) => p.playlist)
        .sort();
      return ephemeral;
    });

    expect(result).toEqual(["music", "podcasts"]);
  });

  it("groups tracks that share the same path part into one playlist", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/refiner/path-collections/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-idb-dedup";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-idb-dedup");
      document.body.append(t);

      await t.tracks.save([
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri: "https://example.com/music/track1.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "t2",
          uri: "https://example.com/music/track2.mp3",
        },
      ]);
      await output.playlistItems.save([]);

      const col = t.playlistItems.collection();
      if (col.state !== "loaded") return null;
      return (col.data as Array<{ ephemeral?: boolean; id: string; playlist: string }>)
        .filter((p) => p.ephemeral)
        .map((p) => [p.id, p.playlist]);
    });

    expect(result).toEqual([
      ["path-collections/t1", "music"],
      ["path-collections/t2", "music"],
    ]);
  });

  it("supports grouping by a configurable path part, percent-decoded", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/refiner/path-collections/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-idb-pathpart";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("path-part", "2");
      t.setAttribute("output-selector", "#test-idb-pathpart");
      document.body.append(t);

      await t.tracks.save([
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri:
            "https://example.com/Primary/techno%20.%20minimal%20.%20acid/album/track.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "t2",
          uri: "https://example.com/Primary/ambient/album/track.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "t3",
          uri: "https://example.com/onlyroot.mp3",
        },
      ]);
      await output.playlistItems.save([]);

      const col = t.playlistItems.collection();
      if (col.state !== "loaded") return null;
      return (col.data as Array<{ ephemeral?: boolean; playlist: string }>)
        .filter((p) => p.ephemeral)
        .map((p) => p.playlist)
        .sort();
    });

    expect(result).toEqual(["ambient", "techno . minimal . acid"]);
  });

  it("ephemeral items do not include tracks with URI that has no path segment", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/refiner/path-collections/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-idb-nopath";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-idb-nopath");
      document.body.append(t);

      // URI with no path segments (root only)
      await t.tracks.save([
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri: "https://example.com/",
        },
      ]);
      await output.playlistItems.save([]);

      const col = t.playlistItems.collection();
      if (col.state !== "loaded") return null;
      return (col.data as Array<{ ephemeral?: boolean }>).filter((p) =>
        p.ephemeral
      ).length;
    });

    expect(result).toBe(0);
  });

  // Runtime rewiring

  it("re-resolves its output when the output-selector is rewired at runtime", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/refiner/path-collections/element.js"
      );

      const outputA = new idbMod.CLASS();
      outputA.id = "test-idb-rewire-a";
      document.body.append(outputA);

      const outputB = new idbMod.CLASS();
      outputB.id = "test-idb-rewire-b";
      document.body.append(outputB);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-idb-rewire-a");
      document.body.append(t);

      await outputA.playlistItems.save([]);
      await outputB.playlistItems.save([]);

      await t.tracks.save([
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri: "https://example.com/music/x.mp3",
        },
      ]);

      const before = t.playlistItems.collection();
      if (before.state !== "loaded") return null;

      t.setAttribute("output-selector", "#test-idb-rewire-b");
      await new Promise((r) => setTimeout(r, 100));

      const after = t.playlistItems.collection();
      if (after.state !== "loaded") return null;

      return { countBefore: before.data.length, countAfter: after.data.length };
    });

    expect(result?.countBefore).toBe(1);
    expect(result?.countAfter).toBe(0);
  });

  // Chain insertion (as done by the "path playlists" prelude)

  it("can be inserted into the output orchestrator's chain manually", async () => {
    const result = await testWeb(async () => {
      const { CLASS } = await import(
        "~/components/transformer/output/refiner/path-collections/element.js"
      );
      const { CLASS: OutputOrchestrator } = await import(
        "~/components/orchestrator/output/element.js"
      );

      const orch = new OutputOrchestrator();
      document.body.append(orch);
      await customElements.whenDefined("dc-output");
      await customElements.whenDefined("dtor-path-collections");

      const entry = orch.output;
      const entrySelector = entry.getAttribute("output-selector");
      if (!entrySelector) return null;

      const transformer = new CLASS();
      transformer.id = "do-output__path-collections";
      transformer.setAttribute("path-part", "2");
      transformer.setAttribute("output-selector", entrySelector);
      entry.after(transformer);
      entry.setAttribute("output-selector", `#${transformer.id}`);

      await orch.tracks.save([
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri:
            "https://example.com/Primary/techno%20.%20minimal%20.%20acid/album/track.mp3",
        },
      ]);
      // Force the playlistItems collection to load before reading it back.
      await orch.playlistItems.save([]);
      await new Promise((r) => setTimeout(r, 100));

      const col = orch.playlistItems.collection();
      const ephemeral = col.state === "loaded"
        ? (col.data as Array<{ ephemeral?: boolean; playlist: string }>)
          .filter((p) => p.ephemeral)
          .map((p) => p.playlist)
        : null;

      return {
        entrySelector: entry.getAttribute("output-selector"),
        transformerSelector: transformer.getAttribute("output-selector"),
        insertedAfterEntry: entry.nextElementSibling === transformer,
        ephemeral,
      };
    });

    expect(result?.entrySelector).toBe("#do-output__path-collections");
    expect(result?.transformerSelector).toBe(
      "#do-output__dtor-initial-contents",
    );
    expect(result?.insertedAfterEntry).toBe(true);
    expect(result?.ephemeral).toEqual(["techno . minimal . acid"]);
  });

  // save filters ephemeral items

  it("save strips items with the path-collections id prefix before writing to the backing output", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/refiner/path-collections/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-idb-save";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-idb-save");
      document.body.append(t);

      await t.playlistItems.save([
        {
          $type: "sh.diffuse.output.playlistItem",
          id: "kept",
          playlist: "Manual",
          criteria: [],
        },
        {
          $type: "sh.diffuse.output.playlistItem",
          id: "kept-ephemeral",
          playlist: "Other",
          criteria: [],
          ephemeral: true,
        },
        {
          $type: "sh.diffuse.output.playlistItem",
          id: "path-collections/t1",
          playlist: "Generated",
          criteria: [],
          ephemeral: true,
        },
        {
          $type: "sh.diffuse.output.playlistItem",
          id: "path-collections/t2",
          playlist: "Generated",
          criteria: [],
        },
      ]);

      const backingCol = output.playlistItems.collection();
      if (backingCol.state !== "loaded") return null;
      return (backingCol.data as Array<{ id: string }>).map((p) => p.id);
    });

    expect(result).toEqual(["kept", "kept-ephemeral"]);
  });
});
