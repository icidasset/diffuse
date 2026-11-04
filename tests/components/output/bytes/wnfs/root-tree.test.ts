import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { CID } from "multiformats/cid";
import { base64url } from "iso-base/rfc4648";
import { FileSystem, Path } from "@wnfs-wg/nest";

import { DiffuseStorageRootTree } from "~/components/output/bytes/wnfs/root-tree.js";

import type { Pair } from "interface-blockstore";

class MemoryBlockstore {
  blocks = new Map<string, Uint8Array>();
  codecs = new Map<string, number>();

  get(cid: CID): Uint8Array {
    const block = this.blocks.get(cid.toString());
    if (!block) throw new Error(`missing block ${cid}`);
    return block;
  }

  has(cid: CID): boolean {
    return this.blocks.has(cid.toString());
  }

  put(cid: CID, block: Uint8Array): CID {
    this.blocks.set(cid.toString(), block);
    this.codecs.set(cid.toString(), cid.code);
    return cid;
  }

  delete(cid: CID): void {
    this.blocks.delete(cid.toString());
  }

  async *putMany(source: AsyncIterable<Pair> | Iterable<Pair>) {
    for await (const { cid, block } of source) {
      this.put(cid, block);
      yield cid;
    }
  }

  async *getMany(source: AsyncIterable<CID> | Iterable<CID>) {
    for await (const cid of source) {
      yield { cid, block: this.get(cid) };
    }
  }

  async *deleteMany(source: AsyncIterable<CID> | Iterable<CID>) {
    for await (const cid of source) {
      this.delete(cid);
      yield cid;
    }
  }

  async *getAll() {
    for (const [key, block] of this.blocks) {
      yield { cid: CID.parse(key), block };
    }
  }
}

describe("components/output/bytes/wnfs/root-tree", () => {
  it("writes a WNFS that can be read back with only supported codecs", async () => {
    const blockstore = new MemoryBlockstore();

    const fs = await FileSystem.create({
      blockstore: blockstore as never,
      rootTreeClass: DiffuseStorageRootTree as never,
    });

    await fs.assignIdentifier("did:key:z6MktestAccount");

    const keyPair = (await crypto.subtle.generateKey(
      {
        name: "RSA-OAEP",
        modulusLength: 2048,
        publicExponent: new Uint8Array([1, 0, 1]),
        hash: "SHA-256",
      },
      true,
      ["encrypt", "decrypt"],
    )) as CryptoKeyPair;

    const publicJwk = await crypto.subtle.exportKey("jwk", keyPair.publicKey);
    if (!publicJwk.n) throw new Error("Expected an RSA modulus");
    const publicKey = base64url.decode(publicJwk.n);

    await fs.registerExchangeKey("diffuse-storage", publicKey);

    const { shareId } = await fs.createPrivateNode({
      path: Path.root(),
      exchangeKeyPair: { publicKey, privateKey: keyPair.privateKey },
    });

    await fs.ensureDirectory(Path.priv("diffuse-output"));

    const bytes = new TextEncoder().encode("hello cbor container");
    const { dataRoot } = await fs.write(
      Path.priv("diffuse-output", "tracks"),
      "bytes",
      bytes,
    );

    const codecs = new Set(blockstore.codecs.values());
    expect([...codecs].sort((a, b) => a - b)).toEqual([0x55, 0x70, 0x71]);

    const fs2 = await FileSystem.fromCID(CID.parse(dataRoot.toString()), {
      blockstore: blockstore as never,
      rootTreeClass: DiffuseStorageRootTree as never,
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

    expect(new TextDecoder().decode(read)).toBe("hello cbor container");
  });
});
