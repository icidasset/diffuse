import { ifDefined } from "lit-html/directives/if-defined.js";

import "~/components/output/polymorphic/indexed-db/element.js";

import { computed, signal } from "~/common/signal.js";
import { OutputTransformer } from "../../base.js";
import { defineElement } from "~/common/element.js";
import * as Output from "~/common/output.js";

/**
 * @import {OutputElement} from "@specs/components/output/types.d.ts"
 */

const COLLECTIONS = /** @type {const} */ ([
  "facets",
  "playlistItems",
  "settings",
  "tracks",
]);

const STORAGE_PREFIX = "diffuse/transformer/output/raw/atproto-space-sync";

/**
 * Wraps an AT Protocol space output with a local IndexedDB cache and keeps the
 * two in sync via union merge by record `id` (using `updatedAt` as tiebreaker).
 *
 * Unlike the public AT Protocol sync transformer there is no firehose or
 * revision check — spaces have neither a relay nor a simple `rev`, so we
 * re-sync on remote-ready and on every save.
 *
 * @extends {OutputTransformer<null>}
 */
class ATProtoSpaceOutputSyncTransformer extends OutputTransformer {
  static NAME = "diffuse/transformer/output/raw/atproto-space-sync";

  #localOutput = signal(
    /** @type {OutputElement<any> | undefined} */ (undefined),
  );

  #syncing = false;

  constructor() {
    super();

    const remote = this.base();
    const local = this.#localOutput.get;

    for (const name of COLLECTIONS) {
      /** @ts-ignore */
      this[name] = {
        collection: computed(() => {
          const l = local();
          if (!l) return { state: "loading" };
          const c = l[name].collection();
          if (c.state === "loading") return c;
          if (c.state === "error") return c;
          return { state: "loaded", data: c.data ?? [] };
        }),
        reload: async () => {
          await this.#sync();
        },
        save: async (/** @type {any} */ newData) => {
          const l = local();
          if (!l) return;

          console.log("[atproto-space-sync] save", name, {
            count: Array.isArray(newData) ? newData.length : newData,
            remoteReady: remote.ready(),
          });

          const newIds = new Set(newData.map((/** @type {any} */ r) => r.id));

          // Update tombstones in one pass: add for records removed from local,
          // remove for records being (re-)added so fixed-ID records can be
          // recreated after deletion without the tombstone blocking them.
          const tombstones = this.#getTombstones(name);
          let tombstonesChanged = false;

          const existing = l[name].collection();
          const existingArr =
            existing.state === "loaded" && Array.isArray(existing.data)
              ? existing.data
              : [];

          for (const record of existingArr) {
            if (!newIds.has(record.id) && !tombstones.has(record.id)) {
              tombstones.add(record.id);
              tombstonesChanged = true;
            }
          }

          for (const record of newData) {
            if (tombstones.has(record.id)) {
              tombstones.delete(record.id);
              tombstonesChanged = true;
            }
          }

          if (tombstonesChanged) {
            localStorage.setItem(
              `${STORAGE_PREFIX}/tombstones/${name}`,
              JSON.stringify([...tombstones]),
            );
          }

          this.#markDirty();
          await l[name].save(newData);

          if (remote.ready()) {
            // Merge with any records added remotely since the last sync so we
            // don't accidentally overwrite them with our local-only view.
            const remoteCol = remote[name].collection();
            const dataForRemote =
              remoteCol.state === "loaded" && Array.isArray(remoteCol.data)
                ? this.#mergeRecords(
                  name,
                  newData,
                  /** @type {typeof newData} */ (remoteCol.data),
                )
                : newData;

            remote[name].save(dataForRemote).then(() => {
              this.#clearDirty();
            }).catch((err) => {
              console.error(err);
            });
          }
        },
      };
    }

    this.ready = () => true;

    // Sync when the remote output becomes ready.
    this.effect(() => {
      const l = local();
      if (!l) return;

      this.effect(async () => {
        if (!remote.ready()) return;
        if (!(await this.isLeader())) return;
        this.#sync();
      });
    });
  }

  // SYNC

  /**
   * @param {readonly string[]} collections
   */
  async #sync(collections = COLLECTIONS) {
    if (this.#syncing) return;
    this.#syncing = true;

    try {
      const l = this.#localOutput.get();
      const remote = this.base();

      if (!l || !remote.ready()) return;

      /** @type {Record<string, any>} */
      const lAny = l;
      /** @type {Record<string, any>} */
      const remoteAny = remote;

      // Fetch remote data for the affected collections.
      for (const name of collections) {
        await remoteAny[name].reload();
      }

      // Await the local collections to settle, rather than reading a possibly
      // still-"loading" local cache as empty.
      const localCollections = await Promise.all(
        collections.map((name) => Output.data(lAny[name])),
      );

      const localHasData = localCollections.some(
        (data) => Array.isArray(data) && data.length > 0,
      );

      // Temporary diagnostics for the empty-dashboard bug.
      console.log("[atproto-space-sync] #sync", {
        ready: remote.ready(),
        dirty: this.#isDirty(),
        localHasData,
        local: Object.fromEntries(
          collections.map((name, i) => [
            name,
            Array.isArray(localCollections[i])
              ? localCollections[i].length
              : localCollections[i],
          ]),
        ),
        remote: Object.fromEntries(
          collections.map((name) => {
            const c = remoteAny[name].collection();
            return [
              name,
              c.state === "loaded" && Array.isArray(c.data)
                ? c.data.length
                : c.state,
            ];
          }),
        ),
      });

      if (!localHasData && !this.#isDirty()) {
        // Local is empty and clean — just pull remote.
        for (const name of collections) {
          const remoteCol = remoteAny[name].collection();
          if (
            remoteCol.state === "loaded" &&
            Array.isArray(remoteCol.data) &&
            remoteCol.data.length > 0
          ) {
            this.#trackIds(name, remoteCol.data);
            await lAny[name].save(remoteCol.data);
          }
        }
      } else {
        // Union merge.
        for (const name of collections) {
          const localCol = lAny[name].collection();
          const remoteCol = remoteAny[name].collection();

          const localArr =
            localCol.state === "loaded" && Array.isArray(localCol.data)
              ? localCol.data
              : [];
          const remoteArr =
            remoteCol.state === "loaded" && Array.isArray(remoteCol.data)
              ? remoteCol.data
              : [];

          const merged = this.#mergeRecords(name, localArr, remoteArr);

          await lAny[name].save(merged);

          if (this.#differFromRemote(merged, remoteArr)) {
            await remoteAny[name].save(merged);
          }

          this.#trackIds(name, merged);
        }
      }

      this.#clearDirty();
    } catch (err) {
      console.warn("Sync failed:", err);
    } finally {
      this.#syncing = false;
    }
  }

  /**
   * Union merge two record arrays by `id`.
   *
   * - Records only in local → keep (unless tombstoned)
   * - Records only in remote → keep (unless tombstoned)
   * - Records in both → pick the one with the later `updatedAt`, local wins on
   *   missing/equal timestamps
   * - Records whose id is in the tombstone set are excluded
   *
   * @template {Record<string, any> & { id: string }} T
   * @param {string} collection
   * @param {T[]} localArr
   * @param {T[]} remoteArr
   * @returns {T[]}
   */
  #mergeRecords(collection, localArr, remoteArr) {
    const tombstones = this.#getTombstones(collection);
    const knownIds = this.#getKnownIds(collection);
    const remoteIds = new Set(remoteArr.map((r) => r.id));

    /** @type {Map<string, T>} */
    const merged = new Map();

    // A space has no `rev`/firehose to distinguish "remote changed" from
    // "remote never written to". An empty remote is therefore ambiguous: it
    // could mean the space was just created, or a write failed silently — not
    // that the user deleted everything remotely. Only trust the
    // "deleted remotely" heuristic when the remote actually has records to
    // compare against; otherwise keep local records and let them be pushed up.
    const remoteAuthoritative = remoteArr.length > 0;

    for (const record of localArr) {
      if (tombstones.has(record.id)) continue;
      if (
        remoteAuthoritative &&
        knownIds.has(record.id) &&
        !remoteIds.has(record.id)
      ) {
        continue;
      }
      merged.set(record.id, record);
    }

    for (const record of remoteArr) {
      if (tombstones.has(record.id)) continue;

      // If this id was previously known but is absent from local, it may have
      // been deleted locally. Only apply this heuristic when localArr is
      // non-empty; an empty localArr could mean the cache was cleared rather
      // than the user deleting everything.
      if (
        localArr.length > 0 &&
        knownIds.has(record.id) &&
        !merged.has(record.id)
      ) {
        continue;
      }

      const existing = merged.get(record.id);

      if (!existing) {
        merged.set(record.id, record);
      } else {
        const lt = existing.updatedAt;
        const rt = record.updatedAt;
        if (lt && rt && rt > lt) {
          merged.set(record.id, record);
        }
      }
    }

    return [...merged.values()];
  }

  /**
   * @param {Array<{ id: string, updatedAt?: string }>} merged
   * @param {Array<{ id: string, updatedAt?: string }>} remote
   * @returns {boolean}
   */
  #differFromRemote(merged, remote) {
    if (merged.length !== remote.length) return true;
    const remoteMap = new Map(remote.map((r) => [r.id, r.updatedAt]));
    return merged.some((r) => remoteMap.get(r.id) !== r.updatedAt);
  }

  // DIRTY FLAG

  #markDirty() {
    localStorage.setItem(`${STORAGE_PREFIX}/dirty`, "1");
  }

  #clearDirty() {
    localStorage.removeItem(`${STORAGE_PREFIX}/dirty`);
  }

  /** @returns {boolean} */
  #isDirty() {
    return localStorage.getItem(`${STORAGE_PREFIX}/dirty`) === "1";
  }

  // TOMBSTONES & KNOWN IDS

  /**
   * @param {string} collection
   * @returns {Set<string>}
   */
  #getTombstones(collection) {
    const raw = localStorage.getItem(
      `${STORAGE_PREFIX}/tombstones/${collection}`,
    );
    return raw ? new Set(JSON.parse(raw)) : new Set();
  }

  /**
   * @param {string} collection
   * @returns {Set<string>}
   */
  #getKnownIds(collection) {
    const raw = localStorage.getItem(`${STORAGE_PREFIX}/known/${collection}`);
    return raw ? new Set(JSON.parse(raw)) : new Set();
  }

  /**
   * Record all ids from a data array as known.
   *
   * @param {string} collection
   * @param {Array<{ id: string }>} data
   */
  #trackIds(collection, data) {
    const known = this.#getKnownIds(collection);
    for (const record of data) {
      known.add(record.id);
    }
    localStorage.setItem(
      `${STORAGE_PREFIX}/known/${collection}`,
      JSON.stringify([...known]),
    );
  }

  // LIFECYCLE

  /** @override */
  connectedCallback() {
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

  // RENDER

  /**
   * @param {import("~/common/element.d.ts").RenderArg} _
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

export default ATProtoSpaceOutputSyncTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ATProtoSpaceOutputSyncTransformer;
export const NAME = "dtor-atproto-space-sync";

defineElement(NAME, CLASS);
