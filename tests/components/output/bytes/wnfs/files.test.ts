// @ts-nocheck -- the evaluated browser function intentionally uses untyped JS

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/output/bytes/wnfs/files", () => {
  it("stores bundles as files, preserves order, and reads legacy files", async () => {
    const result = await testWeb(async () => {
      const { FileSystem, Path } = await import(
        "~/components/output/bytes/wnfs/root-tree.js"
      );
      const { readCollection, writeCollection } = await import(
        "~/components/output/bytes/wnfs/files.js"
      );
      const { byField, byUriScheme } = await import(
        "~/common/bundles.js"
      );
      const { encodeJsonCollection } = await import("~/common/lens.js");

      class MemoryBlockstore {
        blocks = new Map();

        async get(cid) {
          const block = this.blocks.get(cid.toString());
          if (!block) throw new Error(`missing block ${cid}`);
          return block;
        }

        async has(cid) {
          return this.blocks.has(cid.toString());
        }

        async put(cid, block) {
          this.blocks.set(cid.toString(), block);
          return cid;
        }

        async delete(cid) {
          this.blocks.delete(cid.toString());
        }
      }

      const blockstore = new MemoryBlockstore();
      const fs = await FileSystem.create({ blockstore });

      await fs.assignIdentifier("did:key:z6MktestAccount");

      const keyPair = await crypto.subtle.generateKey(
        {
          name: "RSA-OAEP",
          modulusLength: 2048,
          publicExponent: new Uint8Array([1, 0, 1]),
          hash: "SHA-256",
        },
        true,
        ["encrypt", "decrypt"],
      );

      const publicJwk = await crypto.subtle.exportKey(
        "jwk",
        keyPair.publicKey,
      );
      const binary = atob(publicJwk.n.replace(/-/g, "+").replace(/_/g, "/"));
      const publicKey = Uint8Array.from(
        binary,
        (char) => char.charCodeAt(0),
      );

      await fs.registerExchangeKey("wnfs-firkin", publicKey);
      await fs.createPrivateNode({
        path: Path.root(),
        exchangeKeyPair: { publicKey, privateKey: keyPair.privateKey },
      });
      await fs.ensureDirectory(Path.priv("diffuse-output"));

      const playlist = (id, name) => ({
        $type: "sh.diffuse.output.playlistItem",
        id,
        playlist: name,
        criteria: [],
      });

      const items = [
        playlist("a", "Mix"),
        playlist("b", "Other"),
        playlist("c", "Mix"),
      ];

      // The previous layout: one file per collection.
      await fs.write(
        Path.priv("diffuse-output", "playlistItems"),
        "bytes",
        encodeJsonCollection([items[0]], "playlistItems", true),
      );

      const legacy = await readCollection(fs, "playlistItems");

      // Writing bundles migrates the collection to a directory.
      await writeCollection(fs, "playlistItems", items, byField("playlist"));

      const bundled = await readCollection(fs, "playlistItems");
      const bundledNames = (await fs.ls(
        Path.priv("diffuse-output", "playlistItems"),
      )).map((entry) => entry.name).filter((name) => name !== "__manifest")
        .sort();

      // Saving fewer bundles removes the stale ones.
      await writeCollection(
        fs,
        "playlistItems",
        [items[0], items[2]],
        byField("playlist"),
      );

      const after = await readCollection(fs, "playlistItems");
      const afterNames = (await fs.ls(
        Path.priv("diffuse-output", "playlistItems"),
      )).map((entry) => entry.name).filter((name) => name !== "__manifest")
        .sort();

      // Writing the same data again must not rewrite any bundle.
      const unchangedBefore = (await fs.calculateDataRoot()).toString();
      await writeCollection(
        fs,
        "playlistItems",
        [items[0], items[2]],
        byField("playlist"),
      );
      const unchangedAfter = (await fs.calculateDataRoot()).toString();

      // Tracks are one file per URI scheme here (the output uses the richer
      // input source when one is available).
      const tracks = [
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri: "https://a.test/1.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "t2",
          uri: "s3://bucket/2.flac",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "t3",
          uri: "https://a.test/3.mp3",
        },
      ];

      await writeCollection(fs, "tracks", tracks, byUriScheme());

      const trackNames = (await fs.ls(
        Path.priv("diffuse-output", "tracks"),
      )).map((entry) => entry.name).filter((name) => name !== "__manifest")
        .sort();
      const readTracks = await readCollection(fs, "tracks");

      // Collections without a selector stay single files.
      await writeCollection(fs, "facets", [{
        $type: "sh.diffuse.output.facet",
        id: "f1",
        name: "Test",
      }]);

      const readFacets = await readCollection(fs, "facets");

      return {
        legacy: legacy.map((item) => item.id),
        bundled: bundled.map((item) => item.id),
        bundledNames,
        after: after.map((item) => item.id),
        afterNames,
        unchanged: unchangedBefore === unchangedAfter,
        trackNames,
        tracks: readTracks.map((item) => item.id),
        facets: readFacets.map((item) => item.id),
      };
    });

    expect(result.legacy).toEqual(["a"]);
    expect(result.bundled).toEqual(["a", "b", "c"]);
    expect(result.bundledNames.length).toBe(2);
    expect(result.after).toEqual(["a", "c"]);
    expect(result.afterNames.length).toBe(1);
    expect(result.unchanged).toBe(true);
    expect(result.trackNames.length).toBe(2);
    expect(result.tracks).toEqual(["t1", "t2", "t3"]);
    expect(result.facets).toEqual(["f1"]);
  });
});
