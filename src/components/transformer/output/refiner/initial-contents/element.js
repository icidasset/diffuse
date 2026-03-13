import * as IDB from "idb-keyval";

import { batch, computed, signal, untracked } from "~/common/signal.js";
import { OutputTransformer } from "../../base.js";
import { STARTING_SET } from "~/common/facets/constants.js";

/**
 * @import {OutputManagerDeputy} from "~/components/output/types.d.ts"
 */

const IDB_KEY =
  "diffuse/transformer/output/refiner/initial-contents/initialized";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @extends {OutputTransformer}
 */
class InitialContentsTransformer extends OutputTransformer {
  static NAME = "diffuse/transformer/output/refiner/initial-contents";

  #flagLoaded = signal(false);
  #initialized = signal(false);

  constructor() {
    super();

    const base = this.base();

    // Load flag from IDB; gate collection() until resolved to prevent
    // a flash of defaults for a user who has previously cleared their collection.
    IDB.get(IDB_KEY).then((v) => {
      batch(() => {
        this.#initialized.value = !!v;
        this.#flagLoaded.value = true;
      });
    });

    /** @type {OutputManagerDeputy} */
    const manager = {
      facets: {
        ...base.facets,
        collection: computed(() => {
          if (!this.#flagLoaded.get()) return { state: "loading" };

          const col = base.facets.collection();
          if (col.state !== "loaded") return col;

          if (col.data.length > 0) {
            // Set the flag the first time we observe non-empty data.
            // Covers data arriving from another device via sync.
            // untracked read avoids a circular dependency; the write still
            // notifies subscribers and queues a re-evaluation.
            if (!untracked(() => this.#initialized.value)) {
              this.#initialized.value = true;
              IDB.set(IDB_KEY, true); // fire-and-forget
            }
            return { state: "loaded", data: col.data };
          }

          // Tracked read: keeps the computed subscribed to #initialized
          // so it re-runs if save() sets it to true with an empty array.
          if (this.#initialized.get()) {
            return { state: "loaded", data: col.data };
          }

          return { state: "loaded", data: STARTING_SET };
        }),

        save: async (newFacets) => {
          // Set the flag on any explicit save (covers the case where the
          // user's first action is removing a default from the list).
          if (!this.#initialized.value) {
            this.#initialized.value = true;
            IDB.set(IDB_KEY, true); // fire-and-forget
          }
          await base.facets.save(newFacets);
        },
      },

      playlistItems: base.playlistItems,
      themes: base.themes,
      tracks: base.tracks,
      ready: base.ready,
    };

    this.facets = manager.facets;
    this.playlistItems = manager.playlistItems;
    this.themes = manager.themes;
    this.tracks = manager.tracks;
    this.ready = manager.ready;
  }
}

export default InitialContentsTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = InitialContentsTransformer;
export const NAME = "dtor-initial-contents";

customElements.define(NAME, CLASS);
