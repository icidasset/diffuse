import { ifDefined } from "lit-html/directives/if-defined.js";

import "@components/output/polymorphic/indexed-db/element.js";

import { computed, signal } from "@common/signal.js";
import { OutputTransformer } from "../../base.js";

/**
 * @import { RenderArg } from "@common/element.d.ts"
 * @import { OutputElement } from "@components/output/types.d.ts"
 * @import { ATProtoOutputElement } from "@components/output/raw/atproto/types.d.ts"
 */

const COLLECTIONS = /** @type {const} */ ([
  "facets",
  "playlistItems",
  "themes",
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
  constructor() {
    super();

    const remote = this.base();
    const local = this.#localOutput.get;

    for (const name of COLLECTIONS) {
      /** @ts-ignore */
      this[name] = {
        collection: computed(() => {
          const l = local();
          if (!l) return [];
          const data = l[name].collection();
          return Array.isArray(data) ? data : [];
        }),
        reload: async () => {
          await this.#sync();
        },
        save: async (/** @type {any} */ newData) => {
          const l = local();
          if (!l) return;

          // Track deletions: any id present in local but absent in
          // newData has been deleted by the user.
          const oldData = l[name].collection();
          if (Array.isArray(oldData)) {
            const newIds = new Set(newData.map((/** @type {any} */ r) => r.id));
            for (const record of oldData) {
              if (!newIds.has(record.id)) {
                this.#addTombstone(name, record.id);
              }
            }
          }

          // Update known ids
          this.#trackIds(name, newData);

          await l[name].save(newData);

          if (remote.ready()) {
            await remote[name].save(newData);
            const rev = this.#atproto()?.rev();
            if (rev) this.#storeRev(rev);
            this.#clearDirty();
          } else {
            this.#markDirty();
          }
        },
        state: computed(() => local()?.[name].state() ?? "sleeping"),
      };
    }

    this.ready = () => true;

    // Sync when remote becomes ready
    this.effect(() => {
      const l = local();
      if (!l) return;

      this.effect(() => {
        if (!remote.ready()) return;
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

      const localHasData = COLLECTIONS.some((name) => {
        const data = l[name].collection();
        return Array.isArray(data) && data.length > 0;
      });

      if (!localHasData && !dirty) {
        // Local is empty and clean — just pull remote
        for (const name of COLLECTIONS) {
          const remoteData = remote[name].collection();
          if (Array.isArray(remoteData) && remoteData.length > 0) {
            this.#trackIds(name, remoteData);
            await l[name].save(remoteData);
          }
        }
      } else {
        // Union merge
        for (const name of COLLECTIONS) {
          const localData = l[name].collection();
          const remoteData = remote[name].collection();
          const localArr = Array.isArray(localData) ? localData : [];
          const remoteArr = Array.isArray(remoteData) ? remoteData : [];

          const merged = this.#mergeRecords(name, localArr, remoteArr);

          this.#trackIds(name, merged);
          await l[name].save(merged);
          await remote[name].save(merged);
        }
      }

      this.#storeRev(atproto.rev());
      this.#clearDirty();
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

    /** @type {Map<string, T>} */
    const merged = new Map();

    // Start with local records
    for (const record of localArr) {
      if (!tombstones.has(record.id)) {
        merged.set(record.id, record);
      }
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
   * @param {string} id
   */
  #addTombstone(collection, id) {
    const tombstones = this.#getTombstones(collection);
    tombstones.add(id);
    localStorage.setItem(
      `${STORAGE_PREFIX}/tombstones/${collection}`,
      JSON.stringify([...tombstones]),
    );
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

customElements.define(NAME, CLASS);
