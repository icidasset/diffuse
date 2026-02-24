import { Temporal } from "@js-temporal/polyfill";
import { ifDefined } from "lit-html/directives/if-defined.js";
import { decode, encode } from "@atcute/cbor";
import deepDiff from "@fry69/deep-diff";

import "@components/output/polymorphic/indexed-db/element.js";

import * as CID from "@common/cid.js";
import { computed, signal } from "@common/signal.js";
import { OutputTransformer } from "../../base.js";

/**
 * @import { RenderArg } from "@common/element.d.ts"
 * @import { SignalReader } from "@common/signal.d.ts";
 * @import { OutputElement } from "@components/output/types.d.ts";
 *
 * @import { Container } from "./types.d.ts"
 */

/**
 * @extends {OutputTransformer<Uint8Array>}
 */
class DaslBytesSyncOutputTransformer extends OutputTransformer {
  constructor() {
    super();

    const remote = this.base();
    const local = this.#localOutput.get;

    /**
     * @template {{ id: string; updatedAt: string }} T
     * @param {SignalReader<Uint8Array | undefined>} localCollection
     * @param {SignalReader<Uint8Array | undefined>} remoteCollection
     * @returns {SignalReader<{ container: Container<T> | { local: Container<T>; merged: { signal: SignalReader<Container<T> | undefined>; promise: Promise<Container<T>> } }; diverged: boolean; local: boolean; remote: boolean; }>}
     */
    const state = (localCollection, remoteCollection) => {
      return computed(() => {
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
         * @type {Container<T> | { local: Container<T>; merged: { signal: SignalReader<Container<T> | undefined>; promise: Promise<Container<T>> } }}
         */
        let container = r;

        if (diverged.local || diverged.remote) {
          const promise = this.merge(l, r).then((c) => {
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
      });
    };

    // Container signals
    const facets = state(
      computed(() => local()?.facets?.collection()),
      remote.facets.collection,
    );

    const playlistItems = state(
      computed(() => local()?.playlistItems?.collection()),
      remote.playlistItems.collection,
    );

    const themes = state(
      computed(() => local()?.themes?.collection()),
      remote.themes.collection,
    );

    const tracks = state(
      computed(() => local()?.tracks?.collection()),
      remote.tracks.collection,
    );

    this.facets = this.managerProp(
      computed(() => local()?.facets),
      remote.facets,
      facets,
    );

    this.playlistItems = this.managerProp(
      computed(() => local()?.playlistItems),
      remote.playlistItems,
      playlistItems,
    );

    this.themes = this.managerProp(
      computed(() => local()?.themes),
      remote.themes,
      themes,
    );

    this.tracks = this.managerProp(
      computed(() => local()?.tracks),
      remote.tracks,
      tracks,
    );

    this.ready = () => true;

    // Effects
    this.effect(() => {
      const l = local();
      if (!l) return;

      this.effect(async () => {
        if (remote.facets.state() !== "loaded") return;
        const s = facets();
        if (s.diverged) {
          const bytes = this.save(
            "merged" in s.container
              ? await s.container.merged.promise
              : s.container,
          );
          if (l && s.local) l.facets.save(bytes);
          if (s.remote) remote.facets.save(bytes);
        }
      });

      this.effect(async () => {
        if (remote.playlistItems.state() !== "loaded") return;
        const s = playlistItems();
        if (s.diverged) {
          const bytes = this.save(
            "merged" in s.container
              ? await s.container.merged.promise
              : s.container,
          );
          if (l && s.local) l.playlistItems.save(bytes);
          if (s.remote) remote.playlistItems.save(bytes);
        }
      });

      this.effect(async () => {
        if (remote.themes.state() !== "loaded") return;
        const s = themes();
        if (s.diverged) {
          const bytes = this.save(
            "merged" in s.container
              ? await s.container.merged.promise
              : s.container,
          );
          if (l && s.local) l.themes.save(bytes);
          if (s.remote) remote.themes.save(bytes);
        }
      });

      this.effect(async () => {
        if (remote.tracks.state() !== "loaded") return;
        const s = tracks();
        if (s.diverged) {
          const bytes = this.save(
            "merged" in s.container
              ? await s.container.merged.promise
              : s.container,
          );
          if (l && s.local) l.tracks.save(bytes);
          if (s.remote) remote.tracks.save(bytes);
        }
      });
    });
  }

  // SIGNALS

  #localOutput = signal(
    /** @type {OutputElement<Uint8Array | undefined> | undefined} */ (undefined),
  );

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    /** @type {OutputElement<Uint8Array | undefined> | null} */
    const local = this.root().querySelector("dop-indexed-db");
    if (!local) throw new Error("Can't find local output");

    // When defined
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
    for await (const id of allIds) {
      if (allRemoved.has(id)) continue;

      if (id in currentA && id in currentB) {
        const itemA = mapA.get(id);
        const itemB = mapB.get(id);

        if (!itemA || !itemB) {
          console.warn("Should have found item but didn't!");
          continue;
        }

        const isANewerThanB = Temporal.ZonedDateTime.compare(
          Temporal.ZonedDateTime.from(itemA.updatedAt),
          Temporal.ZonedDateTime.from(itemB.updatedAt),
        );

        const newestItem = isANewerThanB ? itemA : itemB;
        const oldItem = isANewerThanB ? itemB : itemA;

        /** @type {T} */
        const mergedItem = { ...oldItem };

        deepDiff.applyDiff(newestItem, mergedItem);

        const cid = await CID.create(0x71, encode(mergedItem));

        data.push(mergedItem);
        current[id] = cid;
      } else {
        const item = mapA.get(id) ?? mapB.get(id);

        if (item) {
          data.push(item);
          current[id] = currentA[id] ?? currentB[id];
        }
      }
    }

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
   * @param {SignalReader<{ collection: SignalReader<Uint8Array | undefined>, reload: () => Promise<void>, save: (bytes: Uint8Array) => Promise<void>, state: SignalReader<"loading" | "loaded" | "sleeping"> } | undefined>} local
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

        const bytes = this.save(adjustedContainer);

        await local()?.save(bytes);
      },
      state: computed(() => local()?.state() ?? "sleeping"),
    };
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    return html`
      <dop-indexed-db
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
