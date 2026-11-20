import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

// NOTE: The `testWeb` callback is serialized and evaluated inside the browser,
// so it can't reference module-scope values.

describe("components/transformer/output/bundles", () => {
  // Disconnected defaults

  it("collection is loading when no output is connected", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/bundles/element.js"
      );
      const t = new mod.CLASS();
      return t.playlistItems.collection().state;
    });

    expect(result).toBe("loading");
  });

  it("ready returns false when no output is connected", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/bundles/element.js"
      );
      const t = new mod.CLASS();
      return t.ready();
    });

    expect(result).toBe(false);
  });

  // Automatic bundling

  it("bundles playlist items and tracks without configuration", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/bundles/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-bundles-auto";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-bundles-auto");
      document.body.append(t);

      await t.playlistItems.save([
        {
          $type: "sh.diffuse.output.playlistItem",
          id: "a",
          playlist: "Mix",
          criteria: [],
        },
        {
          $type: "sh.diffuse.output.playlistItem",
          id: "b",
          playlist: "Other",
          criteria: [],
        },
        {
          $type: "sh.diffuse.output.playlistItem",
          id: "c",
          playlist: "Mix",
          criteria: [],
        },
      ]);

      await t.tracks.save([
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri: "https://example.com/music/a.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "t2",
          uri: "s3://bucket/b.flac",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "t3",
          uri: "https://example.com/music/c.mp3",
        },
      ]);

      const playlists = t.playlistItems.collection();
      const tracks = t.tracks.collection();
      if (playlists.state !== "loaded" || tracks.state !== "loaded") {
        return null;
      }

      const summarize = (
        bundles: Array<{ key: string; items: Array<{ id: string }> }>,
      ) =>
        bundles.map((bundle) => ({
          key: bundle.key,
          ids: bundle.items.map((item) => item.id),
        }));

      return {
        playlists: summarize(
          playlists.data as Array<{
            key: string;
            items: Array<{ id: string }>;
          }>,
        ),
        tracks: summarize(
          tracks.data as Array<{
            key: string;
            items: Array<{ id: string }>;
          }>,
        ),
      };
    });

    expect(result).toEqual({
      playlists: [
        { key: "Mix", ids: ["a", "c"] },
        { key: "Other", ids: ["b"] },
      ],
      tracks: [
        { key: "https", ids: ["t1", "t3"] },
        { key: "s3", ids: ["t2"] },
      ],
    });
  });

  it("groups tracks by the source of the input that owns them", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/bundles/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-bundles-source";
      document.body.append(output);

      const provider = document.createElement("div") as any;
      provider.id = "test-bundles-source-provider";
      provider.inputs = () => ({
        https: {
          SCHEME: "https",
          sources: (tracks: Array<{ uri: string }>) =>
            tracks.map((track) => {
              const host = new URL(track.uri).host;
              return { label: host, uri: `https://${host}` };
            }),
        },
        s3: {
          SCHEME: "s3",
          sources: (tracks: Array<{ uri: string }>) =>
            tracks.map((track) => {
              const [origin, query = ""] = track.uri.split("?");
              const bucket =
                new URLSearchParams(query).get("bucketName") ?? "";
              const base = origin.split("/").slice(0, 3).join("/");
              return { label: bucket, uri: `${base}?bucketName=${bucket}` };
            }),
        },
      });
      document.body.append(provider);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-bundles-source");
      t.setAttribute("input-selector", "#test-bundles-source-provider");
      document.body.append(t);

      await t.tracks.save([
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri: "https://a.test/1.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "t2",
          uri: "https://b.test/2.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "t3",
          uri: "https://a.test/3.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "t4",
          uri: "s3://key:secret@host/one.mp3?bucketName=one",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "t5",
          uri: "s3://key:secret@host/two.mp3?bucketName=two",
        },
      ]);

      const col = t.tracks.collection();
      if (col.state !== "loaded") return null;

      const bundles = col.data as Array<{
        id: string;
        key: string;
        items: Array<{ id: string }>;
      }>;

      return {
        bundles: bundles.map((bundle) => ({
          key: bundle.key,
          ids: bundle.items.map((item) => item.id),
        })),
        uniqueIds: new Set(bundles.map((bundle) => bundle.id)).size ===
          bundles.length,
      };
    });

    expect(result).toEqual({
      bundles: [
        { key: "https://a.test", ids: ["t1", "t3"] },
        { key: "https://b.test", ids: ["t2"] },
        { key: "s3://key:secret@host?bucketName=one", ids: ["t4"] },
        { key: "s3://key:secret@host?bucketName=two", ids: ["t5"] },
      ],
      uniqueIds: true,
    });
  });

  it("gives bundles the same id as the AT Protocol record key", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/bundles/element.js"
      );
      const common = await import(
        "~/components/supplement/atproto-publish/common.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-bundles-rkey";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-bundles-rkey");
      document.body.append(t);

      await t.playlistItems.save([
        {
          $type: "sh.diffuse.output.playlistItem",
          id: "a",
          playlist: "Mix",
          criteria: [],
        },
      ]);

      const col = t.playlistItems.collection();
      if (col.state !== "loaded") return null;

      const bundle = (col.data as Array<{ id: string; key: string }>)[0];
      return bundle.id === common.playlistRkey(bundle.key);
    });

    expect(result).toBe(true);
  });

  it("flattens bundles when saving", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/bundles/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-bundles-save";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-bundles-save");
      document.body.append(t);

      await t.playlistItems.save([
        {
          id: "mix",
          key: "Mix",
          items: [
            {
              $type: "sh.diffuse.output.playlistItem",
              id: "a",
              playlist: "Mix",
              criteria: [],
            },
          ],
        },
        {
          id: "other",
          key: "Other",
          items: [
            {
              $type: "sh.diffuse.output.playlistItem",
              id: "b",
              playlist: "Other",
              criteria: [],
            },
          ],
        },
      ]);

      const col = output.playlistItems.collection();
      if (col.state !== "loaded") return null;

      return (col.data as Array<{ id: string }>).map((item) => item.id);
    });

    expect(result).toEqual(["a", "b"]);
  });

  // Collections without a bundle rule

  it("leaves collections without a grouping rule flat", async () => {
    const result = await testWeb(async () => {
      const idbMod = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const mod = await import(
        "~/components/transformer/output/bundles/element.js"
      );

      const output = new idbMod.CLASS();
      output.id = "test-bundles-passthrough";
      document.body.append(output);

      const t = new mod.CLASS();
      t.setAttribute("output-selector", "#test-bundles-passthrough");
      document.body.append(t);

      await t.settings.save([
        {
          $type: "sh.diffuse.output.setting",
          id: "s1",
          key: "theme",
          value: "blur",
        },
      ]);

      const col = t.settings.collection();
      if (col.state !== "loaded") return null;

      return (col.data as Array<{ id: string }>).map((item) => item.id);
    });

    expect(result).toEqual(["s1"]);
  });
});
