// @ts-nocheck -- the evaluated browser function intentionally uses untyped JS

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/output/bytes/wnfs/browser bundle", () => {
  it("creates, writes and reloads a WNFS", async () => {
    const result = await testWeb(async () => {
      const { FileSystem, Path, WnfsFirkinRootTree } = await import(
        "~/components/output/bytes/wnfs/root-tree.js"
      );

      class MemoryBlockstore {
        blocks = new Map();
        codecs = new Map();

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
          this.codecs.set(cid.toString(), cid.code);
          return cid;
        }

        async delete(cid) {
          this.blocks.delete(cid.toString());
        }
      }

      const blockstore = new MemoryBlockstore();

      const fs = await FileSystem.create({
        blockstore,
        rootTreeClass: WnfsFirkinRootTree,
      });

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

      const publicJwk = await crypto.subtle.exportKey("jwk", keyPair.publicKey);
      const binary = atob(publicJwk.n.replace(/-/g, "+").replace(/_/g, "/"));
      const publicKey = Uint8Array.from(binary, (char) => char.charCodeAt(0));

      await fs.registerExchangeKey("wnfs-firkin", publicKey);

      const { shareId } = await fs.createPrivateNode({
        path: Path.root(),
        exchangeKeyPair: { publicKey, privateKey: keyPair.privateKey },
      });

      await fs.ensureDirectory(Path.priv("diffuse-output"));

      const { dataRoot } = await fs.write(
        Path.priv("diffuse-output", "tracks"),
        "bytes",
        new TextEncoder().encode("hello"),
      );

      const codecs = [...new Set([...blockstore.codecs.values()])].sort(
        (a, b) => a - b,
      );

      const fs2 = await FileSystem.fromCID(dataRoot, {
        blockstore,
        rootTreeClass: WnfsFirkinRootTree,
      });

      await fs2.mountPrivateNode({
        path: Path.root(),
        exchangeKeyPair: { publicKey, privateKey: keyPair.privateKey },
        shareId,
      });

      const read = await fs2.read(
        Path.priv("diffuse-output", "tracks"),
        "bytes",
      );

      return { codecs, text: new TextDecoder().decode(read) };
    });

    expect(result.codecs).toEqual([0x55, 0x70, 0x71]);
    expect(result.text).toBe("hello");
  });
});
