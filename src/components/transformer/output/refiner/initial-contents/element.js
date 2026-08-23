import { computed } from "~/common/signal.js";
import { OutputTransformer } from "../../base.js";
import { defineElement } from "~/common/element.js";

import {
  STARTING_SET_URIS,
} from "~/common/facets/constants.js";
import { buildFacets } from "~/common/facets/utils.js";

/**
 * @import {OutputManagerDeputy} from "@specs/components/output/types.d.ts"
 * @import {OutputConfiguratorElement} from "@specs/components/configurator/output/types.d.ts"
 */

// Per-output "initialized" flag. Stored in localStorage keyed by the output id
// so it is scoped per bucket/output, but deliberately kept OUT of the output's
// own data: writing into a syncing output's collections (e.g. Dropbox via
// dtob-dasl-sync) flows through its merge/sync pipeline, which is reactive
// feedback that loops and freezes the page. localStorage is local-only control
// state, so it's safe.
const STORAGE_KEY =
  "diffuse/transformer/output/refiner/initial-contents/initialized";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @extends {OutputTransformer}
 */
class InitialContentsTransformer extends OutputTransformer {
  static NAME = "diffuse/transformer/output/refiner/initial-contents";

  constructor() {
    super();

    const base = this.base();

    // Mark initialized the first time real (non-empty) data is observed,
    // covering data arriving from another device via sync.
    this.effect(() => {
      const col = base.facets.collection();
      if (col.state !== "loaded" || col.data.length === 0) return;
      this.#markInitialized();
    });

    /** @type {OutputManagerDeputy} */
    const manager = {
      facets: {
        ...base.facets,
        collection: computed(() => {
          const col = base.facets.collection();
          if (col.state !== "loaded") return col;

          if (col.data.length > 0) {
            return { state: "loaded", data: col.data };
          }

          if (this.#isInitialized()) {
            return { state: "loaded", data: col.data };
          }

          // Determine starting set
          return { state: "loaded", data: buildFacets(STARTING_SET_URIS) };
        }),

        save: async (newFacets) => {
          // Persist the facets first so the resulting (non-empty) data is
          // visible before the initialized flag flips, avoiding a flash of
          // the empty collection while a fresh output pops its starting set.
          await base.facets.save(newFacets);
          this.#markInitialized();
        },
      },

      playlistItems: base.playlistItems,
      settings: base.settings,
      tracks: base.tracks,
      ready: base.ready,
    };

    this.facets = manager.facets;
    this.playlistItems = manager.playlistItems;
    this.settings = manager.settings;
    this.tracks = manager.tracks;
    this.ready = manager.ready;
  }

  // METHODS

  /**
   * The id of the currently selected output, or "local" when no custom
   * output is selected (data lives in the default/local storage).
   */
  #selectedId() {
    const output = /** @type {OutputConfiguratorElement | undefined} */ (
      this.output.signal()
    );
    return output?.selected()?.id ?? "local";
  }

  /**
   * Whether the current output is initialized.
   * @returns {boolean}
   */
  #isInitialized() {
    const id = this.#selectedId();
    try {
      const map = /** @type {Record<string, boolean>} */ (
        JSON.parse(localStorage.getItem(STORAGE_KEY) ?? "{}")
      );
      return !!map?.[id];
    } catch {
      return false;
    }
  }

  /**
   * Mark the current output as initialized by updating its flag in the
   * in-memory map and persisting to localStorage.
   */
  #markInitialized() {
    const id = this.#selectedId();
    let map = /** @type {Record<string, boolean>} */ ({});
    try {
      map = /** @type {Record<string, boolean>} */ (
        JSON.parse(localStorage.getItem(STORAGE_KEY) ?? "{}")
      ) ?? {};
    } catch {}
    if (map[id]) return;

    map[id] = true;
    localStorage.setItem(STORAGE_KEY, JSON.stringify(map));
  }
}

export default InitialContentsTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = InitialContentsTransformer;
export const NAME = "dtor-initial-contents";

defineElement(NAME, CLASS);