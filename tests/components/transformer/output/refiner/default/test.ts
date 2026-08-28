import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/transformer/output/refiner/default", () => {
  it("ready returns false when no output is connected", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/refiner/default/element.js"
      );
      const t = new mod.CLASS();
      return t.ready();
    });

    expect(result).toBe(false);
  });

  it("tracks.collection is loading when no output is connected", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/refiner/default/element.js"
      );
      const t = new mod.CLASS();
      return t.tracks.collection().state;
    });

    expect(result).toBe("loading");
  });

  it("non-ephemeral tracks are saved to backing output and appear in collection", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/refiner/default/element.js"
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
      ]);

      const col = t.tracks.collection();
      if (col.state !== "loaded") return null;
      return (col.data as Array<{ id: string }>).map((tr) => tr.id);
    });

    expect(result).toEqual(["track-1"]);
  });

  it("ephemeral tracks appear in transformer collection but not in backing output", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/refiner/default/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-idb-eph";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-idb-eph");
      document.body.append(t);

      await t.tracks.save([
        {
          $type: "sh.diffuse.output.track",
          id: "regular",
          uri: "https://example.com/regular.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "eph",
          uri: "https://example.com/eph.mp3",
          ephemeral: true,
        },
      ]);

      const transformerCol = t.tracks.collection();
      const backingCol = output.tracks.collection();

      if (transformerCol.state !== "loaded") return null;
      if (backingCol.state !== "loaded") return null;

      return {
        transformerIds: (transformerCol.data as Array<{ id: string }>)
          .map((tr) => tr.id)
          .sort(),
        backingIds: (backingCol.data as Array<{ id: string }>)
          .map((tr) => tr.id)
          .sort(),
      };
    });

    expect(result?.transformerIds).toEqual(["eph", "regular"]);
    expect(result?.backingIds).toEqual(["regular"]);
  });

  it("settings.collection delegates to backing output unchanged", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/refiner/default/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-idb-settings";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-idb-settings");
      document.body.append(t);

      await t.settings.save([
        {
          $type: "sh.diffuse.output.setting",
          id: "s1",
          key: "sh.diffuse.input.disabled.uris",
          value: "",
        },
      ]);

      const col = t.settings.collection();
      if (col.state !== "loaded") return null;
      return (col.data as Array<{ id: string }>).map((s) => s.id);
    });

    expect(result).toEqual(["s1"]);
  });

  it("ephemeral playlist items appear in transformer collection but not in backing output", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/refiner/default/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-idb-pl";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-idb-pl");
      document.body.append(t);

      await t.playlistItems.save([
        {
          $type: "sh.diffuse.output.playlistItem",
          id: "pl-1",
          playlist: "My Playlist",
          criteria: [],
        },
        {
          $type: "sh.diffuse.output.playlistItem",
          id: "pl-eph",
          playlist: "Ephemeral Playlist",
          criteria: [],
          ephemeral: true,
        },
      ]);

      const transformerCol = t.playlistItems.collection();
      const backingCol = output.playlistItems.collection();

      if (transformerCol.state !== "loaded") return null;
      if (backingCol.state !== "loaded") return null;

      return {
        transformerIds: (transformerCol.data as Array<{ id: string }>)
          .map((pl) => pl.id)
          .sort(),
        backingIds: (backingCol.data as Array<{ id: string }>)
          .map((pl) => pl.id)
          .sort(),
      };
    });

    expect(result?.transformerIds).toEqual(["pl-1", "pl-eph"]);
    expect(result?.backingIds).toEqual(["pl-1"]);
  });
});
