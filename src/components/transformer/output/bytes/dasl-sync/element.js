import { decode, encode } from "@atcute/cbor";
import { ifDefined } from "lit-html/directives/if-defined.js";
import deepDiff from "@fry69/deep-diff";

import "~/components/output/polymorphic/indexed-db/element.js";

import * as CID from "~/common/cid.js";
import { diff, strictEquality } from "~/common/compare.js";
import { computed, signal } from "~/common/signal.js";
import { compareTimestamps } from "~/common/utils.js";
import { OutputTransformer } from "../../base.js";

/**
 * @import { SignalReader } from "~/common/signal.d.ts";
 * @import { RenderArg } from "~/common/element.d.ts"
 * @import { OutputElement } from "~/components/output/types.d.ts"
 *
 * @import { Container } from "./types.d.ts"
 */

/** @type {Container<any>} */
const EMPTY = {
  cid: undefined,
  data: [],
  inventory: { current: {}, removed: [] },
};

/**
 * @extends {OutputTransformer<Uint8Array>}
 */
class DaslBytesSyncOutputTransformer extends OutputTransformer {
  static NAME = "diffuse/transformer/output/bytes/dasl-sync";

  constructor() {
    super();

    const remote = this.base();
    const local = this.#localOutput.get;

    /**
     * @template {{ id: string; updatedAt: string }} T
     * @param {string} kind
     * @param {SignalReader<Uint8Array | undefined>} localCollection
     * @param {SignalReader<Uint8Array | undefined>} remoteCollection
     * @param {SignalReader<"loading" | "loaded" | "sleeping">} remoteState
     * @param {{ saveLocal: (bytes: Uint8Array) => Promise<void>; saveRemote: (bytes: Uint8Array) => Promise<void> }} sync
     */
    const state = (
      kind,
      localCollection,
      remoteCollection,
      remoteState,
      { saveLocal, saveRemote },
    ) => {
      const container = signal(
        /** @type {Container<T>} */ (EMPTY),
        { compare: strictEquality },
      );

      const isReady = signal(false);
      const merging = signal({ isBusy: false, lastCID: "" }, {
        compare: diff,
      });

      this.effect(() => {
        if (!isReady.value) return;
        if (merging.value.isBusy) return;

        const lb = localCollection();
        const rb = remote.ready() ? remoteCollection() : undefined;
        const rs = remoteState();

        /** @type {Container<T> | undefined} */
        const l = lb ? decode(lb) : undefined;

        /** @type {Container<T> | undefined} */
        const r = rb && rs === "loaded" ? decode(rb) : undefined;

        if (!r) {
          if (l) {
            container.value = l;

            if (remote.ready() && rs === "loaded") {
              this.isLeader().then((isLeader) => {
                if (!isLeader) return;
                const bytes = this.save(l);
                saveRemote(bytes);
              });
            }
          }
        } else if (!l) {
          container.value = r;

          this.isLeader().then((isLeader) => {
            if (!isLeader) return;
            const bytes = this.save(r);
            saveLocal(bytes);
          });
        } else if (
          rs === "loaded" && this.hasDiverged({ local: l, remote: r })
        ) {
          // Async merge
          this.isLeader().then((isLeader) => {
            if (!isLeader) return;

            merging.value = { isBusy: true, lastCID: merging.value.lastCID };

            this.merge(l, r).then(async (c) => {
              container.value = c;

              if (c.cid === merging.value.lastCID) return;

              const bytes = this.save(c);

              if (c.cid !== l.cid) {
                await saveLocal(bytes);
              }

              if (remote.ready() && rs === "loaded" && c.cid !== r.cid) {
                await saveRemote(bytes);
              }

              merging.value = { isBusy: false, lastCID: c.cid ?? "" };
            });
          });
        } else {
          container.value = l;
        }
      });

      return computed(() => {
        if (!isReady.get()) isReady.value = true;
        return container.get();
      });
    };

    // Container signals
    const facets = state(
      "facets",
      computed(() => local()?.facets.collection()),
      remote.facets.collection,
      remote.facets.state,
      {
        saveLocal: async (v) => local()?.facets.save(v),
        saveRemote: remote.facets.save,
      },
    );

    const playlistItems = state(
      "playlistItems",
      computed(() => local()?.playlistItems.collection()),
      remote.playlistItems.collection,
      remote.playlistItems.state,
      {
        saveLocal: async (v) => local()?.playlistItems.save(v),
        saveRemote: remote.playlistItems.save,
      },
    );

    const themes = state(
      "themes",
      computed(() => local()?.themes.collection()),
      remote.themes.collection,
      remote.themes.state,
      {
        saveLocal: async (v) => local()?.themes.save(v),
        saveRemote: remote.themes.save,
      },
    );

    const tracks = state(
      "tracks",
      computed(() => local()?.tracks.collection()),
      remote.tracks.collection,
      remote.tracks.state,
      {
        saveLocal: async (v) => local()?.tracks.save(v),
        saveRemote: remote.tracks.save,
      },
    );

    // Output manager
    this.facets = this.managerProp(
      { save: async (v) => local()?.facets.save(v) },
      remote.facets,
      facets,
    );

    this.playlistItems = this.managerProp(
      { save: async (v) => local()?.playlistItems.save(v) },
      remote.playlistItems,
      playlistItems,
    );

    this.themes = this.managerProp(
      { save: async (v) => local()?.themes.save(v) },
      remote.themes,
      themes,
    );

    this.tracks = this.managerProp(
      { save: async (v) => local()?.tracks.save(v) },
      remote.tracks,
      tracks,
    );

    this.ready = () => true;
  }

  // SIGNALS

  #localOutput = signal(
    /** @type {OutputElement<any> | undefined} */ (undefined),
  );

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      this.broadcast(this.identifier, {});
    }

    super.connectedCallback();

    /** @type {OutputElement<any> | null} */
    const local = this.root().querySelector("dop-indexed-db");
    if (!local) throw new Error("Can't find local output");

    customElements.whenDefined(local.localName).then(() => {
      this.#localOutput.value = local;
    });
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
   */
  hasDiverged({ local, remote }) {
    return local.cid !== remote.cid;
  }

  /**
   * @template {{ id: string; updatedAt: string }} T
   * @param {Container<T>} a
   * @param {Container<T>} b
   * @returns {Promise<Container<T>>}
   */
  async merge(a, b) {
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
          console.warn("Should have found both items but didn't!");
          continue;
        }

        // Items are identical, no merge or CID recomputation needed
        if (currentA[id] === currentB[id]) {
          data.push(itemA);
          current[id] = currentA[id];
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
   * @param {SignalReader<Container<T>>} container
   * @returns {{ collection: SignalReader<T[]>, reload: () => Promise<void>, save: (items: T[]) => Promise<void>, state: SignalReader<"loading" | "loaded" | "sleeping"> }}
   */
  managerProp(local, remote, container) {
    return {
      collection: computed(() => {
        return container().data;
      }),
      reload: remote.reload,
      save: async (/** @type {T[]} */ newItems) => {
        const adjustedContainer = await this.updateContainer({
          collection: newItems,
          previous: container(),
        });

        const bytes = this.save(adjustedContainer);
        await local.save(bytes);
      },
      state: computed(() => {
        if (container().cid) return "loaded";
        return "loading";
      }),
    };
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    return html`
      <dop-indexed-db
        group="${ifDefined(this.getAttribute(`group`))}"
        namespace="${ifDefined(this.getAttribute(`namespace`))}"
      ></dop-indexed-db>
    `;
  }
}

export default DaslBytesSyncOutputTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = DaslBytesSyncOutputTransformer;
export const NAME = "dtob-dasl-sync";

customElements.define(NAME, CLASS);
