import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/orchestrator/path-collections", () => {
  // Disconnected defaults

  it("ready returns false when no output is connected", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/orchestrator/path-collections/element.js"
      );
      const t = new mod.CLASS();
      return t.ready();
    });

    expect(result).toBe(false);
  });

  it("playlistItems.collection is loading when no output is connected", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/orchestrator/path-collections/element.js"
      );
      const t = new mod.CLASS();
      return t.playlistItems.collection().state;
    });

    expect(result).toBe("loading");
  });

  // Ephemeral item generation

  it("generates one ephemeral item per unique path segment from loaded tracks", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/orchestrator/path-collections/element.js"
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

  it("deduplicates tracks that share the same path segment", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/orchestrator/path-collections/element.js"
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
      return (col.data as Array<{ ephemeral?: boolean; playlist: string }>)
        .filter((p) => p.ephemeral)
        .map((p) => p.playlist);
    });

    expect(result).toEqual(["music"]);
  });

  it("ephemeral items do not include tracks with URI that has no path segment", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/orchestrator/path-collections/element.js"
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

  // save filters ephemeral items

  it("save strips ephemeral items before writing to the backing output", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/orchestrator/path-collections/element.js"
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
          id: "dropped",
          playlist: "Generated",
          criteria: [],
          ephemeral: true,
        },
      ]);

      const backingCol = output.playlistItems.collection();
      if (backingCol.state !== "loaded") return null;
      return (backingCol.data as Array<{ id: string }>).map((p) => p.id);
    });

    expect(result).toEqual(["kept"]);
  });
});
