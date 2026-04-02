import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";
import type { Facet, Track } from "~/definitions/types.d.ts";

describe("components/output/polymorphic/indexed-db", () => {
  it("ready returns true immediately", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const output = new mod.CLASS();
      document.body.append(output);
      return output.ready();
    });

    expect(result).toBe(true);
  });

  it("tracks.collection is loading before any save", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const output = new mod.CLASS();
      document.body.append(output);
      return output.tracks.collection().state;
    });

    expect(result).toBe("loading");
  });

  it("tracks.collection returns loaded state after save", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const output = new mod.CLASS();
      document.body.append(output);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri: "https://example.com/track1.mp3",
        },
      ];

      await output.tracks.save(tracks);
      return output.tracks.collection();
    });

    expect(result.state).toBe("loaded");
    if (result.state === "loaded") {
      expect(result.data).toHaveLength(1);
    }
  });

  it("tracks.save persists data and collection returns it", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const output = new mod.CLASS();
      document.body.append(output);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "track-a",
          uri: "https://example.com/a.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "track-b",
          uri: "https://example.com/b.mp3",
        },
      ];

      await output.tracks.save(tracks);
      const col = output.tracks.collection();
      if (col.state !== "loaded") return [];
      return (col.data as Track[]).map((t: Track) => t.id);
    });

    expect(result).toEqual(["track-a", "track-b"]);
  });

  it("facets.collection returns loaded state after save", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const output = new mod.CLASS();
      document.body.append(output);

      const facets: Facet[] = [
        {
          $type: "sh.diffuse.output.facet",
          id: "f1",
          name: "Favourites",
        },
      ];

      await output.facets.save(facets);
      return output.facets.collection();
    });

    expect(result.state).toBe("loaded");
  });

  it("settings.collection returns loaded state after save", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const output = new mod.CLASS();
      document.body.append(output);

      await output.settings.save([]);
      return output.settings.collection();
    });

    expect(result.state).toBe("loaded");
  });

  it("playlistItems.collection returns loaded state after save", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const output = new mod.CLASS();
      document.body.append(output);

      await output.playlistItems.save([]);
      return output.playlistItems.collection();
    });

    expect(result.state).toBe("loaded");
  });

  it("saving empty tracks array results in empty loaded collection", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const output = new mod.CLASS();
      document.body.append(output);

      await output.tracks.save([]);
      const col = output.tracks.collection();
      if (col.state !== "loaded") return null;
      return (col.data as Track[]).length;
    });

    expect(result).toBe(0);
  });

  it("save overwrites previous data", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const output = new mod.CLASS();
      document.body.append(output);

      await output.tracks.save([
        {
          $type: "sh.diffuse.output.track" as const,
          id: "first",
          uri: "https://example.com/first.mp3",
        },
      ]);

      await output.tracks.save([
        {
          $type: "sh.diffuse.output.track" as const,
          id: "second",
          uri: "https://example.com/second.mp3",
        },
      ]);

      const col = output.tracks.collection();
      if (col.state !== "loaded") return [];
      return (col.data as Track[]).map((t: Track) => t.id);
    });

    expect(result).toEqual(["second"]);
  });
});
