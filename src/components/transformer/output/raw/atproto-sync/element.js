import { ifDefined } from "lit-html/directives/if-defined.js";

import "~/components/output/polymorphic/indexed-db/element.js";

import { computed, signal } from "~/common/signal.js";
import { OutputTransformer } from "../../base.js";
import { defineElement } from "~/common/element.js";
import * as Output from "~/common/output.js";

/**
 * @import { RenderArg } from "~/common/element.d.ts"
 * @import { OutputElement } from "~/components/output/types.d.ts"
 * @import { ATProtoOutputElement } from "~/components/output/raw/atproto/types.d.ts"
 */

const COLLECTIONS = /** @type {const} */ ([
  "facets",
  "playlistItems",
  "settings",
  "tracks",
]);

const STORAGE_PREFIX = "diffuse/transformer/output/atproto-sync";

/**
 * Wraps an AT Protocol output with a local IndexedDB cache.
 *
 * Uses the repo `rev` (revision) from the AT Protocol to skip
 * unnecessary fetches when nothing changed remotely.
 *
 * When both local and remote have diverged, performs a union merge
 * by record `id`: records from both sides are combined, with
 * `updatedAt` used as a tiebreaker for conflicts on the same id
 * (falling back to local wins).
 *
 * Maintains a per-collection tombstone set of deleted record ids
 * so that records deleted on one side are not re-introduced by
 * the other during merge.
 *
 * @extends {OutputTransformer<null>}
 */
class ATProtoOutputSyncTransformer extends OutputTransformer {
  static NAME = "diffuse/transformer/output/raw/atproto-sync";

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
          return { state: "loaded", data: c.data ?? [] };
        }),
        reload: async () => {
          await this.#sync();
        },
        save: async (/** @type {any} */ newData) => {
          const l = local();
          if (!l) return;

          const newIds = new Set(newData.map((/** @type {any} */ r) => r.id));

          // Update tombstones in one pass: add for records deleted from local,
          // remove for records being (re-)added — so that fixed-ID records can
          // be recreated after deletion without the tombstone blocking them.
          const tombstones = this.#getTombstones(name);
          let tombstonesChanged = false;

          const oldCol = await Output.data(l[name]);
          if (oldCol && Array.isArray(oldCol.data)) {
            for (const record of oldCol.data) {
              if (!newIds.has(record.id) && !tombstones.has(record.id)) {
                tombstones.add(record.id);
                tombstonesChanged = true;
              }
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

          // Update known ids
          this.#trackIds(name, newData);

          await l[name].save(newData);

          if (remote.ready()) {
            remote[name].save(newData).then(() => {
              const rev = this.#atproto()?.rev();
              if (rev) this.#storeRev(rev);
              this.#clearDirty();
            }).catch((err) => {
              console.error(err);
            });
          } else {
            this.#markDirty();
          }
        },
      };
    }

    this.ready = () => true;

    // Sync when remote becomes ready
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

  // SIGNALS

  #localOutput = signal(
    /** @type {OutputElement<any> | undefined} */ (undefined),
  );

  #syncing = false;

  /**
   * @returns {ATProtoOutputElement | undefined}
   */
  #atproto() {
    return /** @type {any} */ (this.output.signal());
  }

  // SYNC

  async #sync() {
    if (this.#syncing) return;
    this.#syncing = true;

    try {
      const l = this.#localOutput.get();
      const remote = this.base();
      const atproto = this.#atproto();
      if (!l || !atproto || !remote.ready()) return;

      const remoteRev = await atproto.getLatestCommit();
      if (!remoteRev) return;

      const localRev = this.#getStoredRev();
      const dirty = this.#isDirty();

      if (localRev === remoteRev && !dirty) {
        return;
      }

      // Fetch remote data
      for (const name of COLLECTIONS) {
        await remote[name].reload();
      }

      const localCollections = await Promise.all(
        COLLECTIONS.map((name) => Output.data(l[name])),
      );

      const localHasData = localCollections.some(
        (data) => Array.isArray(data) && data.length > 0,
      );

      if (!localHasData && !dirty) {
        // Local is empty and clean — just pull remote
        for (const name of COLLECTIONS) {
          const remoteCol = remote[name].collection();
          if (
            remoteCol.state === "loaded" && Array.isArray(remoteCol.data) &&
            remoteCol.data.length > 0
          ) {
            this.#trackIds(name, remoteCol.data);
            await l[name].save(remoteCol.data);
          }
        }
      } else {
        // Union merge
        for (const name of COLLECTIONS) {
          const localCol = l[name].collection();
          const remoteCol = remote[name].collection();
          const localArr =
            localCol.state === "loaded" && Array.isArray(localCol.data)
              ? localCol.data
              : [];
          const remoteArr =
            remoteCol.state === "loaded" && Array.isArray(remoteCol.data)
              ? remoteCol.data
              : [];

          const merged = this.#mergeRecords(name, localArr, remoteArr);

          this.#trackIds(name, merged);
          await l[name].save(merged);
          await remote[name].save(merged);
        }
      }

      this.#storeRev(atproto.rev());
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
   * - Records in both → pick the one with the later `updatedAt`,
   *   falling back to local wins
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

    // Start with local records
    for (const record of localArr) {
      if (tombstones.has(record.id)) continue;
      // If previously synced but now absent from remote, it was deleted remotely.
      if (knownIds.has(record.id) && !remoteIds.has(record.id)) continue;
      merged.set(record.id, record);
    }

    // Merge remote records
    for (const record of remoteArr) {
      if (tombstones.has(record.id)) continue;

      // If this id was previously known but is absent from local,
      // it was deleted locally — skip it.
      if (knownIds.has(record.id) && !merged.has(record.id)) continue;

      const existing = merged.get(record.id);

      if (!existing) {
        merged.set(record.id, record);
      } else {
        // Both sides have this record — pick by updatedAt
        const lt = existing.updatedAt;
        const rt = record.updatedAt;
        if (lt && rt && rt > lt) {
          merged.set(record.id, record);
        }
      }
    }

    return [...merged.values()];
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

  // REV & DIRTY FLAG

  /** @returns {string | null} */
  #getStoredRev() {
    return localStorage.getItem(`${STORAGE_PREFIX}/rev`);
  }

  /** @param {string | null} rev */
  #storeRev(rev) {
    if (rev) {
      localStorage.setItem(`${STORAGE_PREFIX}/rev`, rev);
    }
  }

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

  // LIFECYCLE

  /** @override */
  connectedCallback() {
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

export default ATProtoOutputSyncTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ATProtoOutputSyncTransformer;
export const NAME = "dtor-atproto-sync";

defineElement(NAME, CLASS);
