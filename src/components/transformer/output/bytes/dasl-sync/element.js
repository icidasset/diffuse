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
     * @template {{ id: string }} T
     * @param {SignalReader<Uint8Array | undefined>} localCollection
     * @param {SignalReader<Uint8Array | undefined>} remoteCollection
     * @returns {SignalReader<{ container: Container<T>; diverged: boolean; local: boolean; remote: boolean; }>}
     */
    const state = (localCollection, remoteCollection) =>
      computed(() => {
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
                cid: "",
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

        return {
          container: diverged.local || diverged.remote
            ? /* this.merge(l, r) */ l
            : r,
          diverged: diverged.local || diverged.remote,
          local: diverged.local,
          remote: diverged.remote,
        };
      });

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

    this.facets = undefined;
    this.playlistItems = undefined;
    this.themes = undefined;
    this.tracks = undefined;

    this.ready = () => true;

    // Effects
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

  // 🛠️

  /**
   * @template {{ id: string }} T
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
   * @template {{ id: string }} T
   * @param {{ previous: Container<T> | undefined, collection: T[] }} _
   * @returns {Promise<Container<T>>}
   */
  async save({ previous, collection }) {
    const inventory = previous?.inventory ?? { current: {}, removed: [] };

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
export const NAME = "dtob-automerge";

customElements.define(NAME, CLASS);
