import { decode, encode } from "@atcute/cbor";
import { ifDefined } from "lit-html/directives/if-defined.js";
import deepDiff from "@fry69/deep-diff";

import "~/components/output/polymorphic/indexed-db/element.js";

import * as CID from "~/common/cid.js";
import { diff, strictEquality } from "~/common/compare.js";
import { computed, signal } from "~/common/signal.js";
import { compareTimestamps } from "~/common/temporal.js";
import { OutputTransformer } from "../../base.js";
import { defineElement } from "~/common/element.js";

/**
 * @import { SignalReader } from "~/common/signal.d.ts";
 * @import { RenderArg } from "~/common/element.d.ts"
 * @import { OutputElement } from "@specs/components/output/types.d.ts"
 *
 * @import { Container } from "@specs/components/transformer/output/bytes/dasl-sync/types.d.ts"
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
     * @param {SignalReader<{ state: "loading" } | { state: "loaded"; data: Uint8Array | undefined } | { state: "error" }>} localCollection
     * @param {SignalReader<{ state: "loading" } | { state: "loaded"; data: Uint8Array | undefined } | { state: "error" }>} remoteCollection
     * @param {{ saveLocal: (bytes: Uint8Array) => Promise<void>; saveRemote: (bytes: Uint8Array) => Promise<void> }} sync
     */
    const state = (
      kind,
      localCollection,
      remoteCollection,
      { saveLocal, saveRemote },
    ) => {
      const container = signal(
        /** @type {Container<T>} */ (EMPTY),
        { compare: strictEquality },
      );

      const isReady = signal(false);
      const merging = signal(
        { isBusy: false, lastCID: "", lastRemoteCID: "" },
        { compare: diff },
      );

      this.effect(() => {
        if (!isReady.value) return;
        if (merging.value.isBusy) return;

        const lc = localCollection();
        const rc = remote.ready() ? remoteCollection() : undefined;

        const lb = lc?.state === "loaded" ? lc.data : undefined;
        const rb = rc?.state === "loaded" ? rc.data : undefined;
        const rs = rc?.state;

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
          // If we've already reconciled this exact (local, remote) pair and it
          // made no progress (e.g. the remote write failed, or the merge
          // resolved to the container we already hold), don't re-enter the
          // merge. Re-merging here only toggles `merging`'s busy flag, which
          // re-runs this effect and spins the reactive sync loop, freezing the
          // page. Wait until local or remote actually changes before trying
          // again.
          const prev = merging.value;
          if (prev.lastCID === (l.cid ?? "") &&
              prev.lastRemoteCID === (r.cid ?? "")) {
            return;
          }

          // Async merge
          this.isLeader().then((isLeader) => {
            if (!isLeader) return;

            merging.value = {
              isBusy: true,
              lastCID: prev.lastCID,
              lastRemoteCID: prev.lastRemoteCID,
            };

            this.merge(l, r).then(async (c) => {
              /**
               * Remote cid after the (possible) push. Stays at the pre-merge
               * remote cid when there's nothing to push or the push fails, so
               * the effect-level guard above knows this reconcile was already
               * attempted and won't retry it in a loop.
               */
              let resolvedRemoteCID = r.cid ?? "";
              let reconciledCID = c.cid ?? "";

              try {
                container.value = c;

                const bytes = this.save(c);

                if (c.cid !== l.cid) {
                  await saveLocal(bytes);
                }

                if (remote.ready() && rs === "loaded" && c.cid !== r.cid) {
                  await saveRemote(bytes);
                  resolvedRemoteCID = c.cid ?? "";
                }
              } catch (err) {
                console.error("Merge failed:", err);
              } finally {
                merging.value = {
                  isBusy: false,
                  lastCID: reconciledCID,
                  lastRemoteCID: resolvedRemoteCID,
                };
              }
            }).catch((err) => {
              // Merge() itself rejected (before producing a reconciled
              // container). Reset the busy flag so the effect isn't stuck, and
              // record the exact (local, remote) pair we attempted so a
              // persistent failure is skipped rather than spinning forever.
              console.error("Merge failed:", err);
              merging.value = {
                isBusy: false,
                lastCID: l.cid ?? "",
                lastRemoteCID: r.cid ?? "",
              };
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
      computed(() => local()?.facets.collection() ?? { state: "loading" }),
      remote.facets.collection,
      {
        saveLocal: async (v) => local()?.facets.save(v),
        saveRemote: remote.facets.save,
      },
    );

    const playlistItems = state(
      "playlistItems",
      computed(() =>
        local()?.playlistItems.collection() ?? { state: "loading" }
      ),
      remote.playlistItems.collection,
      {
        saveLocal: async (v) => local()?.playlistItems.save(v),
        saveRemote: remote.playlistItems.save,
      },
    );

    const settings = state(
      "settings",
      computed(() => local()?.settings.collection() ?? { state: "loading" }),
      remote.settings.collection,
      {
        saveLocal: async (v) => local()?.settings.save(v),
        saveRemote: remote.settings.save,
      },
    );

    const tracks = state(
      "tracks",
      computed(() => local()?.tracks.collection() ?? { state: "loading" }),
      remote.tracks.collection,
      {
        saveLocal: async (v) => local()?.tracks.save(v),
        saveRemote: remote.tracks.save,
      },
    );

    // Output manager
    this.facets = this.managerProp(
      { save: async (v) => local()?.facets.save(v) },
      remote.facets,
      remote.ready,
      facets,
    );

    this.playlistItems = this.managerProp(
      { save: async (v) => local()?.playlistItems.save(v) },
      remote.playlistItems,
      remote.ready,
      playlistItems,
    );

    this.settings = this.managerProp(
      { save: async (v) => local()?.settings.save(v) },
      remote.settings,
      remote.ready,
      settings,
    );

    this.tracks = this.managerProp(
      { save: async (v) => local()?.tracks.save(v) },
      remote.tracks,
      remote.ready,
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

        const hasAUpdatedAt = Boolean(itemA.updatedAt);
        const hasBUpdatedAt = Boolean(itemB.updatedAt);

        // The side that carries an `updatedAt` is the one that was edited, so
        // it should win over a pristine copy that never recorded a change (e.g.
        // a legacy seeded/starting-set facet persisted before it gained a
        // timestamp). Only when both carry one do we compare clocks.
        const isANewerThanB = (hasAUpdatedAt && !hasBUpdatedAt)
          ? true
          : (!hasAUpdatedAt && hasBUpdatedAt)
          ? false
          : hasAUpdatedAt && hasBUpdatedAt
          ? compareTimestamps(itemA.updatedAt, itemB.updatedAt) > 0
          : false;

        const newestItem = isANewerThanB ? itemA : itemB;
        const oldItem = isANewerThanB ? itemB : itemA;

        /** @type {T} */
        const mergedItem = { ...oldItem };

        deepDiff.applyDiff(mergedItem, newestItem);

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
   * @param {{ collection: SignalReader<{ state: "loading" } | { state: "loaded"; data: Uint8Array | undefined } | { state: "error" }>, reload: () => Promise<void>, save: (bytes: Uint8Array) => Promise<void> }} remote
   * @param {SignalReader<boolean>} remoteReady
   * @param {SignalReader<Container<T>>} container
   * @returns {{ collection: SignalReader<{ state: "loading" } | { state: "loaded"; data: T[] } | { state: "error" }>, reload: () => Promise<void>, save: (items: T[]) => Promise<void> }}
   */
  managerProp(local, remote, remoteReady, container) {
    return {
      collection: computed(() => {
        const c = container();

        if (c.cid === undefined && remoteReady() && remote.collection().state === "loading") {
          return { state: "loading" };
        }

        return { state: "loaded", data: c.data };
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

defineElement(NAME, CLASS);
