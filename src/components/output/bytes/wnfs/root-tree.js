import * as DagPB from "@ipld/dag-pb";
import * as JsonCodec from "multiformats/codecs/json";
import * as Raw from "multiformats/codecs/raw";
import { CID } from "multiformats/cid";
import { sha256 } from "multiformats/hashes/sha2";

import { BasicRootTree } from "@wnfs-wg/nest";

// Re-exported so tests (and consumers) can build the same file system without
// importing the package entry point directly.
export { FileSystem, Path } from "@wnfs-wg/nest";

/**
 * @import { Blockstore } from "interface-blockstore"
 * @import { CID as CIDType } from "multiformats/cid"
 */

/**
 * A root tree that behaves exactly like `BasicRootTree` except that it stores
 * the share tally as a raw block instead of a `multiformats/codecs/json`
 * block.
 *
 * The Firkin CID policy accepts raw (`0x55`), dag-pb (`0x70`) and
 * dag-cbor (`0x71`) blocks. It does not accept the JSON codec (`0x0200`) that
 * `BasicRootTree` uses for the tally, so we store those bytes as raw instead.
 * `BasicRootTree.fromCID` only decodes the tally bytes, so it can still read
 * every root this class writes (which is what `mountPrivateNode` relies on
 * internally when it loads a share to self).
 */
export class DiffuseStorageRootTree extends BasicRootTree {
  /** @type {Blockstore} */
  #blockstore;

  /**
   * @param {{
   *   blockstore: Blockstore
   *   did: string | undefined
   *   exchangeRoot: any
   *   privateForest: any
   *   publicRoot: any
   *   shareCounter: number
   *   unix: any
   *   version: string
   * }} args
   */
  constructor(args) {
    super(args);
    this.#blockstore = args.blockstore;
  }

  /**
   * @override
   * @param {Blockstore} blockstore
   */
  static async create(blockstore) {
    return fromBase(await BasicRootTree.create(blockstore), blockstore);
  }

  /**
   * @override
   * @param {Blockstore} blockstore
   * @param {CIDType} cid
   */
  static async fromCID(blockstore, cid) {
    return fromBase(
      await BasicRootTree.fromCID(blockstore, cid),
      blockstore,
    );
  }

  /**
   * @param {{
   *   did?: string | undefined
   *   exchangeRoot?: any
   *   privateForest?: any
   *   publicRoot?: any
   *   shareCounter?: number
   * }} overrides
   */
  #with(overrides) {
    return new DiffuseStorageRootTree({
      blockstore: this.#blockstore,
      did: "did" in overrides ? overrides.did : this.did(),
      exchangeRoot: "exchangeRoot" in overrides
        ? overrides.exchangeRoot
        : this.exchangeRoot(),
      privateForest: "privateForest" in overrides
        ? overrides.privateForest
        : this.privateForest(),
      publicRoot: "publicRoot" in overrides
        ? overrides.publicRoot
        : this.publicRoot(),
      shareCounter: "shareCounter" in overrides
        ? /** @type {number} */ (overrides.shareCounter)
        : this.shareCounter(),
      unix: EMPTY_UNIX,
      version: "0.0.0",
    });
  }

  /**
   * @override
   * @param {string} did
   */
  replaceDID(did) {
    return Promise.resolve(this.#with({ did }));
  }

  /**
   * @override
   * @param {any} dir
   */
  replaceExchangeRoot(dir) {
    return Promise.resolve(this.#with({ exchangeRoot: dir }));
  }

  /**
   * @override
   * @param {any} forest
   * @param {any} _modifications
   */
  replacePrivateForest(forest, _modifications) {
    return Promise.resolve(this.#with({ privateForest: forest }));
  }

  /**
   * @override
   * @param {any} dir
   * @param {any} _modifications
   */
  replacePublicRoot(dir, _modifications) {
    return Promise.resolve(this.#with({ publicRoot: dir }));
  }

  /** @override */
  increaseShareCounter() {
    return Promise.resolve(
      this.#with({ shareCounter: this.shareCounter() + 1 }),
    );
  }

  /** @override */
  clone() {
    return this.#with({});
  }

  /** @override */
  async store() {
    const wnfsStore = wnfsBlockstore(this.#blockstore);

    const exchangeRoot = await this.exchangeRoot().store(wnfsStore);
    const privateForest = await this.privateForest().store(wnfsStore);
    const publicRoot = await this.publicRoot().store(wnfsStore);
    const tally = await storeBlock(
      JsonCodec.encode(this.shareCounter()),
      Raw.code,
      this.#blockstore,
    );

    /** @type {{ Name: string; Hash: CIDType }[]} */
    const links = [
      { Name: "exchange", Hash: CID.decode(exchangeRoot) },
      { Name: "private", Hash: CID.decode(privateForest) },
      { Name: "public", Hash: CID.decode(publicRoot) },
      { Name: "tally", Hash: tally },
    ];

    const did = this.did();
    if (did !== undefined) {
      const didCid = await storeBlock(
        Raw.encode(new TextEncoder().encode(did)),
        Raw.code,
        this.#blockstore,
      );
      links.push({ Name: "did", Hash: didCid });
    }

    const node = DagPB.createNode(
      new Uint8Array([8, 1]),
      /** @type {any} */ (links),
    );
    return await storeBlock(DagPB.encode(node), DagPB.code, this.#blockstore);
  }
}

/**
 * @param {BasicRootTree} base
 * @param {Blockstore} blockstore
 */
function fromBase(base, blockstore) {
  return new DiffuseStorageRootTree({
    blockstore,
    did: base.did(),
    exchangeRoot: base.exchangeRoot(),
    privateForest: base.privateForest(),
    publicRoot: base.publicRoot(),
    shareCounter: base.shareCounter(),
    unix: EMPTY_UNIX,
    version: "0.0.0",
  });
}

/** @type {any} */
const EMPTY_UNIX = { Links: [] };

/**
 * Adapt an `interface-blockstore` blockstore to the WNFS blockstore shape.
 *
 * @param {Blockstore} blockstore
 */
export function wnfsBlockstore(blockstore) {
  return {
    /** @param {Uint8Array} cid */
    async getBlock(cid) {
      return await blockstore.get(CID.decode(cid));
    },

    /** @param {Uint8Array} cid */
    async hasBlock(cid) {
      return await blockstore.has(CID.decode(cid));
    },

    /**
     * @param {Uint8Array} cid
     * @param {Uint8Array} bytes
     */
    async putBlockKeyed(cid, bytes) {
      await blockstore.put(CID.decode(cid), bytes);
    },

    /**
     * @param {Uint8Array} bytes
     * @param {number} codec
     */
    async putBlock(bytes, codec) {
      const cid = CID.createV1(codec, await sha256.digest(bytes));
      await blockstore.put(cid, bytes);
      return cid.bytes;
    },
  };
}

/**
 * @param {Uint8Array} bytes
 * @param {number} code
 * @param {Blockstore} blockstore
 */
async function storeBlock(bytes, code, blockstore) {
  const cid = CID.createV1(code, await sha256.digest(bytes));
  await blockstore.put(cid, bytes);
  return cid;
}
