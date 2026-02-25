import * as IDB from "idb-keyval";
import { decode, encode } from "@atcute/cbor";
import deepDiff from "@fry69/deep-diff";

import "@components/output/polymorphic/indexed-db/element.js";

import * as CID from "@common/cid.js";
import { computed, signal, untracked } from "@common/signal.js";
import { compareTimestamps } from "@common/utils.js";
import { OutputTransformer } from "../../base.js";
import { IDB_PREFIX } from "./constants.js";

/**
 * @import { Signal, SignalReader } from "@common/signal.d.ts";
 * @import { Container } from "./types.d.ts"
 */

/**
 * @extends {OutputTransformer<Uint8Array>}
 */
class DaslBytesSyncOutputTransformer extends OutputTransformer {
  constructor() {
    super();

    const remote = this.base();

    /**
     * @template {{ id: string; updatedAt: string }} T
     * @param {SignalReader<Uint8Array | undefined>} localCollection
     * @param {SignalReader<Uint8Array | undefined>} remoteCollection
     */
    const state = (localCollection, remoteCollection) => {
      /**
       * @typedef {{ container: Container<T> | { local: Container<T>; merged: { signal: SignalReader<Container<T> | undefined>; promise: Promise<Container<T>> } }; diverged: boolean; local: boolean; remote: boolean; }} State
       */

      const sig = signal(
        /** @type {State} */ ({
          container: {
            cid: undefined,
            data: [],
            inventory: { current: {}, removed: [] },
          },
          diverged: false,
          local: false,
          remote: false,
        }),
        { eager: true },
      );

      /** @returns {State} */
      const determine = () => {
        const lb = localCollection();
        const rb = remote.ready() ? remoteCollection() : undefined;

        /** @type {Container<T> | undefined} */
        const l = lb ? decode(lb) : undefined;

        /** @type {Container<T> | undefined} */
        const r = rb ? decode(rb) : undefined;

        if (!r) {
          return l
            ? {
              container: l,
              diverged: remote.ready(),
              local: false,
              remote: remote.ready(),
            }
            : {
              container: {
                cid: undefined,
                data: [],
                inventory: { current: {}, removed: [] },
              },
              diverged: false,
              local: false,
              remote: false,
            };
        } else if (!l) {
          return { container: r, diverged: true, local: true, remote: false };
        }

        const diverged = this.hasDiverged({ local: l, remote: r });
        const mergedSignal = signal(
          /** @type {Container<T> | undefined} */ (undefined),
        );

        /**
         * @type {State["container"]}
         */
        let container = r;

        if (diverged.local || diverged.remote) {
          const promise = this.merge(l, r).then((c) => {
            console.log("Merged:", c);
            mergedSignal.set(c);
            return c;
          });

          container = {
            local: l,
            merged: { promise, signal: mergedSignal.get },
          };
        }

        return {
          container,
          diverged: diverged.local || diverged.remote,
          local: diverged.local,
          remote: diverged.remote,
        };
      };

      this.effect(() => {
        const result = determine();
        const current = untracked(sig.get);

        const newCID = "merged" in result.container
          ? undefined // handle async case separately
          : result.container.cid;

        const currentCID = "merged" in current.container
          ? undefined
          : current.container.cid;

        // Skip if both are non-merged and CIDs match
        if (
          newCID !== undefined && currentCID !== undefined &&
          newCID === currentCID
        ) {
          return;
        }

        // For the non-merged common case, set synchronously
        if (!("merged" in result.container)) {
          sig.set(result);
          return;
        }

        // Only go async for the merge case
        result.container.merged.promise.then(async (merged) => {
          const cur = untracked(sig.get);
          const curCID = "merged" in cur.container
            ? (await cur.container.merged.promise).cid
            : cur.container.cid;
          if (merged.cid !== curCID) {
            sig.set(result);
          }
        });
      });

      return sig.get;
    };

    // Local
    const local = {
      facets: this.local("facets"),
      playlistItems: this.local("playlistItems"),
      themes: this.local("themes"),
      tracks: this.local("tracks"),
    };

    // Container signals
    const facets = state(
      local.facets.get,
      remote.facets.collection,
    );

    const playlistItems = state(
      local.playlistItems.get,
      remote.playlistItems.collection,
    );

    const themes = state(
      local.themes.get,
      remote.themes.collection,
    );

    const tracks = state(
      local.tracks.get,
      remote.tracks.collection,
    );

    // Output manager
    this.facets = this.managerProp(
      { save: this.putLocalFn("facets", local.facets) },
      remote.facets,
      facets,
    );

    this.playlistItems = this.managerProp(
      { save: this.putLocalFn("playlistItems", local.playlistItems) },
      remote.playlistItems,
      playlistItems,
    );

    this.themes = this.managerProp(
      { save: this.putLocalFn("themes", local.themes) },
      remote.themes,
      themes,
    );

    this.tracks = this.managerProp(
      { save: this.putLocalFn("tracks", local.tracks) },
      remote.tracks,
      tracks,
    );

    this.ready = () => true;

    // Effects
    // this.effect(async () => {
    //   if (remote.facets.state() !== "loaded") return;
    //   const s = facets();
    //   if (s.diverged) {
    //     const bytes = this.save(
    //       "merged" in s.container
    //         ? await s.container.merged.promise
    //         : s.container,
    //     );
    //     local.facets.set(bytes);
    //     this.putLocal("facets", bytes);
    //     if (s.remote) remote.facets.save(bytes);
    //   }
    // });

    // this.effect(async () => {
    //   if (remote.playlistItems.state() !== "loaded") return;
    //   const s = playlistItems();
    //   if (s.diverged) {
    //     const bytes = this.save(
    //       "merged" in s.container
    //         ? await s.container.merged.promise
    //         : s.container,
    //     );
    //     local.playlistItems.set(bytes);
    //     this.putLocal("playlistItems", bytes);
    //     if (s.remote) remote.playlistItems.save(bytes);
    //   }
    // });

    // this.effect(async () => {
    //   if (remote.themes.state() !== "loaded") return;
    //   const s = themes();
    //   if (s.diverged) {
    //     const bytes = this.save(
    //       "merged" in s.container
    //         ? await s.container.merged.promise
    //         : s.container,
    //     );
    //     local.themes.set(bytes);
    //     this.putLocal("themes", bytes);
    //     if (s.remote) remote.themes.save(bytes);
    //   }
    // });

    // this.effect(async () => {
    //   if (remote.tracks.state() !== "loaded") return;
    //   const s = tracks();
    //   if (s.diverged) {
    //     const bytes = this.save(
    //       "merged" in s.container
    //         ? await s.container.merged.promise
    //         : s.container,
    //     );
    //     local.tracks.set(bytes);
    //     this.putLocal("tracks", bytes);
    //     if (s.remote) remote.tracks.save(bytes);
    //   }
    // });
  }

  // DATA FUNCTIONS

  /**
   * @template {{ id: string; updatedAt: string }} T
   * @param {{ previous: Container<T>, collection: T[] }} _
   * @returns {Promise<Container<T>>}
   */
  async updateContainer({ previous, collection }) {
    const inventory = previous.inventory;

    const collIds = collection.map(({ id }) => id);

    const currSet = new Set(Object.keys(inventory.current));
    const collSet = new Set(collIds);

    const newSet = collSet.difference(currSet);
    const remSet = currSet.difference(collSet);

    const alreadyRemoved = new Set(inventory.removed);
    const allRemoved = alreadyRemoved.union(remSet);

    /** @type {Record<string, string>} */
    const current = { ...inventory.current };

    remSet.forEach((id) => {
      delete current[id];
    });

    /** @type Promise<void>[] */
    const promises = [];

    collection.forEach((a) => {
      if (!newSet.has(a.id)) return;

      // Item is new, calculate CID and add it to the `current` dictionary
      const encoded = encode(a);

      promises.push((async () => {
        const cid = await CID.create(0x71, encoded);
        current[a.id] = cid;
      })());
    });

    await Promise.all(promises);

    const newInventory = {
      current,
      removed: Array.from(allRemoved),
    };

    return {
      // TODO: Do we need this? Too big of a perf penalty?
      cid: await CID.create(0x71, encode(newInventory)),
      data: collection,
      inventory: newInventory,
    };
  }

  /**
   * @template {{ id: string; updatedAt: string }} T
   * @param {{ local: Container<T>, remote: Container<T> }} _
   * @returns {{ local: boolean, remote: boolean }} Which store needs updating?
   */
  hasDiverged({ local, remote }) {
    const diverged = local.cid !== remote.cid;

    if (!diverged) {
      return {
        local: false,
        remote: false,
      };
    }

    // TODO: Could be improved.
    //       We might not need to save on both ends.
    return {
      local: true,
      remote: true,
    };
  }

  /**
   * @template {{ id: string; updatedAt: string }} T
   * @param {Container<T>} a
   * @param {Container<T>} b
   * @returns {Promise<Container<T>>}
   */
  async merge(a, b) {
    console.log("MERGE", a, b);

    const removedA = new Set(a.inventory.removed);
    const removedB = new Set(b.inventory.removed);
    const allRemoved = removedA.union(removedB);

    const currentA = a.inventory.current;
    const currentB = b.inventory.current;

    const mapA = new Map(a.data.map((item) => [item.id, item]));
    const mapB = new Map(b.data.map((item) => [item.id, item]));

    // Combine all known ids from both sides
    const allIds = new Set([
      ...Object.keys(currentA),
      ...Object.keys(currentB),
    ]);

    /** @type {Record<string, string>} */
    const current = {};

    /** @type {T[]} */
    const data = [];

    // Construct `current` and `data`
    /** @type {Promise<void>[]} */
    const cidPromises = [];

    for (const id of allIds) {
      if (allRemoved.has(id)) continue;

      if (id in currentA && id in currentB) {
        const itemA = mapA.get(id);
        const itemB = mapB.get(id);

        if (!itemA || !itemB) {
          console.warn("Should have found item but didn't!");
          continue;
        }

        const isANewerThanB = itemA.updatedAt && itemB.updatedAt
          ? compareTimestamps(itemA.updatedAt, itemB.updatedAt) > 0
          : false;

        const newestItem = isANewerThanB ? itemA : itemB;
        const oldItem = isANewerThanB ? itemB : itemA;

        /** @type {T} */
        const mergedItem = { ...oldItem };

        deepDiff.applyDiff(newestItem, mergedItem);

        data.push(mergedItem);

        cidPromises.push(
          CID.create(0x71, encode(mergedItem)).then((cid) => {
            current[id] = cid;
          }),
        );
      } else {
        const item = mapA.get(id) ?? mapB.get(id);

        if (item) {
          data.push(item);
          current[id] = currentA[id] ?? currentB[id];
        }
      }
    }

    await Promise.all(cidPromises);

    // New inventory
    const updatedInventory = { current, removed: Array.from(allRemoved) };

    return {
      cid: await CID.create(0x71, encode(updatedInventory)),
      data,
      inventory: updatedInventory,
    };
  }

  /**
   * @template {{ id: string; updatedAt: string }} T
   * @param {Container<T>} container
   * @returns {Uint8Array}
   */
  save(container) {
    return encode(container);
  }

  // OUTPUT MANAGER FUNCTIONS

  /**
   * @template {{ id: string; updatedAt: string }} T
   * @param {{ save: (bytes: Uint8Array) => Promise<void> | void }} local
   * @param {{ collection: SignalReader<Uint8Array | undefined>, reload: () => Promise<void>, save: (bytes: Uint8Array) => Promise<void>, state: SignalReader<"loading" | "loaded" | "sleeping"> }} remote
   * @param {SignalReader<{ container: Container<T> | { local: Container<T>; merged: { signal: SignalReader<Container<T> | undefined>; promise: Promise<Container<T>> } }}>} container
   * @returns {{ collection: SignalReader<T[]>, reload: () => Promise<void>, save: (items: T[]) => Promise<void>, state: SignalReader<"loading" | "loaded" | "sleeping"> }}
   */
  managerProp(local, remote, container) {
    return {
      collection: computed(() => {
        const c = container().container;

        if ("merged" in c) {
          return c.merged.signal()?.data ?? c.local?.data;
        }

        return c.data;
      }),
      reload: remote.reload,
      save: async (/** @type {T[]} */ newItems) => {
        let c = container().container;

        if ("merged" in c) {
          c = await c.merged.promise;
        }

        const adjustedContainer = await this.updateContainer({
          collection: newItems,
          previous: c,
        });

        console.log("Save:", newItems);
        const bytes = this.save(adjustedContainer);

        console.log("Bytes:", bytes);
        await local.save(bytes);
      },
      state: computed(() => "loaded"),
    };
  }

  // INDEXED-DB

  /**
   * @param {string} name
   */
  local(name) {
    const s = signal(/** @type {Uint8Array | undefined} */ (undefined), {
      eager: true,
    });

    this.getLocal(name).then(s.set);

    return s;
  }

  /**
   * @param {string} name
   * @returns {Promise<Uint8Array | undefined>}
   */
  getLocal(name) {
    return IDB.get(`${IDB_PREFIX}/${this.#cat(name)}`);
  }

  /** @param {string} name; @param {Uint8Array} data  */
  putLocal(name, data) {
    return IDB.set(`${IDB_PREFIX}/${this.#cat(name)}`, data);
  }

  /**
   * @param {string} name
   * @param {Signal<Uint8Array | undefined>} signal
   */
  putLocalFn =
    (name, signal) => /** @param {Uint8Array} data */ async (data) => {
      signal.value = data;
      await this.putLocal(name, data);
    };

  // 🛠️

  get namespace() {
    return this.hasAttribute("namespace")
      ? this.getAttribute("namespace") + "/"
      : "";
  }

  /** @param {string} name */
  #cat(name) {
    return `${this.namespace}${name}`;
  }
}

export default DaslBytesSyncOutputTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = DaslBytesSyncOutputTransformer;
export const NAME = "dtob-dasl-sync";

customElements.define(NAME, CLASS);
