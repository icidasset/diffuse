import * as IDB from "idb-keyval";
import { CID } from "multiformats/cid";

import { IDB_BLOCK_PREFIX } from "./constants.js";

/**
 * @import { Firkin } from "@tokono.ma/firkin"
 * @import { Blockstore, Pair } from "interface-blockstore"
 * @import { AwaitIterable } from "interface-store"
 */

/**
 * A WNFS blockstore backed by a Firkin account.
 *
 * Blocks are immutable, so they are cached in memory and in IndexedDB. Reads
 * prefer a single CAR Mirror pull (`/dag/pull`) of the WNFS root and fall back
 * to individual `/blob` reads. `put` only queues blocks locally; `flush(root)`
 * uploads them with `/dag/push`, preserving their real (codec-bearing) CIDs.
 *
 * @implements {Blockstore}
 */
export class FirkinBlockstore {
  /** @type {Firkin} */
  #rb;

  /** @type {Map<string, Uint8Array>} */
  #memory = new Map();

  /** @type {Map<string, { cid: CID; bytes: Uint8Array }>} */
  #dirty = new Map();

  /** @type {Set<string>} */
  #known = new Set();

  /** @type {Set<string>} */
  #cached = new Set();

  /** @type {Set<Promise<unknown>>} */
  #cacheWrites = new Set();

  /** @type {Promise<unknown>} */
  #flushing = Promise.resolve();

  /** @type {string | undefined} */
  #root;

  /** Whether the whole-DAG pull was already attempted for the current root. */
  #prefetchAttempted = false;

  /**
   * @param {Firkin} rb
   * @param {string} [root] - The WNFS data root, used for `/dag/pull`.
   */
  constructor(rb, root) {
    this.#rb = rb;
    this.#root = root;
  }

  /** @param {string | undefined} root */
  setRoot(root) {
    this.#root = root;
    this.#prefetchAttempted = false;
  }

  // LIFECYCLE

  async open() {}

  async close() {
    await Promise.all(this.#cacheWrites);
  }

  // CORE

  /**
   * @param {CID} cid
   * @param {any} [_options]
   */
  async get(cid, _options) {
    const key = cid.toString();

    const memory = this.#memory.get(key);
    if (memory) return memory;

    const cached = await IDB.get(this.#idbKey(key));
    if (cached instanceof Uint8Array) {
      this.#memory.set(key, cached);
      this.#cached.add(key);
      return cached;
    }

    // Prefer a single CAR Mirror pull of the whole DAG over many `/blob`
    // requests. If it fails (or the root is unknown), fall back to `/blob`.
    if (this.#root && !this.#prefetchAttempted) {
      await this.#prefetch();
      const pulled = this.#memory.get(key);
      if (pulled) return pulled;
    }

    const bytes = await this.#rb.get(key);
    this.#memory.set(key, bytes);
    this.#known.add(key);
    this.#remember(key, bytes);
    return bytes;
  }

  /**
   * @param {CID} cid
   * @param {any} [_options]
   */
  async has(cid, _options) {
    const key = cid.toString();

    if (this.#memory.has(key) || this.#known.has(key)) return true;

    try {
      await this.get(cid);
      return true;
    } catch {
      return false;
    }
  }

  /**
   * @param {CID} cid
   * @param {Uint8Array} bytes
   * @param {any} [_options]
   */
  put(cid, bytes, _options) {
    const key = cid.toString();

    this.#memory.set(key, bytes);
    this.#remember(key, bytes);

    if (!this.#known.has(key)) {
      this.#dirty.set(key, { cid, bytes });
    }

    return cid;
  }

  /**
   * Local-only removal. Blocks are content-addressed and shared, so this never
   * detaches the blob from the Firkin account.
   *
   * @param {CID} cid
   * @param {any} [_options]
   */
  async delete(cid, _options) {
    const key = cid.toString();
    this.#memory.delete(key);
    this.#dirty.delete(key);
    await IDB.del(this.#idbKey(key));
  }

  // BLOCK CACHE ADAPTER

  /**
   * Read a cached block without touching the network and without triggering a
   * DAG prefetch. Used by the SDK's pull-side Bloom/completeness checks.
   *
   * @param {string} cid
   * @returns {Promise<Uint8Array | undefined>}
   */
  async peek(cid) {
    const memory = this.#memory.get(cid);
    if (memory) return memory;

    const cached = await IDB.get(this.#idbKey(cid));
    if (cached instanceof Uint8Array) {
      this.#memory.set(cid, cached);
      this.#cached.add(cid);
      return cached;
    }

    return undefined;
  }

  /**
   * Cache a block that the server already has (e.g. from a `/dag/pull` CAR).
   *
   * @param {string} cid
   * @param {Uint8Array} bytes
   */
  seed(cid, bytes) {
    this.#memory.set(cid, bytes);
    this.#known.add(cid);
    this.#remember(cid, bytes);
  }

  /** @returns {Promise<string[]>} */
  async keys() {
    const prefix = `${IDB_BLOCK_PREFIX}/`;
    const stored = (await IDB.keys())
      .filter((key) => typeof key === "string" && key.startsWith(prefix))
      .map((key) => /** @type {string} */ (key).slice(prefix.length));

    return [...new Set([...this.#memory.keys(), ...stored])];
  }

  // BULK

  /**
   * @param {AwaitIterable<Pair>} source
   * @param {any} [options]
   */
  async *putMany(source, options) {
    for await (const { cid, block } of source) {
      await this.put(cid, block, options);
      yield cid;
    }
  }

  /**
   * @param {AwaitIterable<CID>} source
   * @param {any} [options]
   */
  async *getMany(source, options) {
    for await (const cid of source) {
      yield { cid, block: await this.get(cid, options) };
    }
  }

  /**
   * @param {AwaitIterable<CID>} source
   * @param {any} [options]
   */
  async *deleteMany(source, options) {
    for await (const cid of source) {
      await this.delete(cid, options);
      yield cid;
    }
  }

  /** @param {any} [_options] */
  async *getAll(_options) {
    for (const [key, block] of this.#memory) {
      yield { cid: CID.parse(key), block };
    }
  }

  // UPLOAD

  /**
   * Upload every queued block. Calls are serialized so a flush started while
   * another one is running picks up the blocks queued in the meantime.
   *
   * @param {string | undefined} [root] - The current WNFS data root, used as
   *   the CAR root for `/dag/push`.
   */
  flush(root) {
    const run = async () => {
      while (this.#dirty.size > 0) {
        const entries = [...this.#dirty.values()];
        this.#dirty.clear();

        try {
          await this.#push(root, entries);
        } catch (err) {
          for (const { cid, bytes } of entries) {
            this.#dirty.set(cid.toString(), { cid, bytes });
          }
          throw err;
        }

        for (const { cid } of entries) {
          this.#known.add(cid.toString());
        }
      }
    };

    const next = this.#flushing.then(run, run);
    this.#flushing = next.catch(() => {});
    return next;
  }

  /**
   * @param {string | undefined} root
   * @param {{ cid: CID; bytes: Uint8Array }[]} entries
   */
  async #push(root, entries) {
    for (const { cid } of entries) {
      if (cid.code === 0x0200) {
        throw new Error(
          "WNFS produced a JSON-codec block, which this Firkin policy rejects",
        );
      }
    }

    // Prefer one CAR Mirror push rooted at the WNFS root. The SDK requires
    // the root block to be part of the pushed graph, so seed it from memory
    // when the flush loop starts a new round after the root was uploaded.
    if (root && typeof this.#rb.dagPush === "function") {
      const blocks = new Map(
        entries.map(({ cid, bytes }) => [cid.toString(), bytes]),
      );

      if (!blocks.has(root)) {
        const rootBytes = this.#memory.get(root);
        if (rootBytes) blocks.set(root, rootBytes);
      }

      if (blocks.has(root)) {
        try {
          await this.#rb.dagPush(root, blocks);
          return;
        } catch (err) {
          console.warn(
            "CAR Mirror push failed; falling back to individual blob uploads",
            err,
          );
        }
      }
    }

    // Fallback: upload each block individually.
    for (let i = 0; i < entries.length; i += 6) {
      const batch = entries.slice(i, i + 6);

      await Promise.all(
        batch.map(async ({ cid, bytes }) => {
          const result = await this.#rb.upload(bytes, { codec: cid.code });

          if (result.cid !== cid.toString()) {
            throw new Error(
              `Firkin CID mismatch: expected ${cid}, got ${result.cid}`,
            );
          }
        }),
      );
    }
  }

  // 🛠️

  /** Pull the whole WNFS DAG once, then serve blocks from the cache. */
  async #prefetch() {
    if (!this.#root || this.#prefetchAttempted) return;
    this.#prefetchAttempted = true;

    try {
      const blocks = await this.#rb.dagPull(this.#root);

      for (const [cid, bytes] of blocks) {
        this.#memory.set(cid, bytes);
        this.#known.add(cid);
        this.#remember(cid, bytes);
      }
    } catch (err) {
      console.warn(
        "CAR Mirror pull failed; falling back to individual blob reads",
        err,
      );
    }
  }

  /**
   * @param {string} key
   * @param {Uint8Array} bytes
   */
  #remember(key, bytes) {
    // `#known` means the server has the block; it must not suppress the local
    // IndexedDB write. Without this, blocks fetched/pulled from Firkin
    // Bearer would never be cached and every page load would re-pull the DAG.
    if (this.#cached.has(key)) return;
    this.#cached.add(key);

    const promise = IDB.set(this.#idbKey(key), bytes).catch(() => {
      this.#cached.delete(key);
    });
    this.#cacheWrites.add(promise);
    promise.finally(() => this.#cacheWrites.delete(promise));
  }

  /** @param {string} key */
  #idbKey(key) {
    return `${IDB_BLOCK_PREFIX}/${key}`;
  }
}
