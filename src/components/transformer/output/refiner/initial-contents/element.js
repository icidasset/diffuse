import { computed } from "~/common/signal.js";
import { OutputTransformer } from "../../base.js";
import { defineElement } from "~/common/element.js";

import {
  STARTING_SET_URIS,
} from "~/common/facets/constants.js";
import { buildFacets } from "~/common/facets/utils.js";
import { isOutputInitialized, markOutputInitialized } from "~/common/output.js";

/**
 * @import {OutputManagerDeputy} from "@specs/components/output/types.d.ts"
 * @import {OutputConfiguratorElement} from "@specs/components/configurator/output/types.d.ts"
 */

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
      if (col.state !== "loaded") return;
      const data = Array.isArray(col.data) ? col.data : [];
      if (data.length === 0) return;
      markOutputInitialized(this.#selectedId());
    });

    /** @type {OutputManagerDeputy} */
    const manager = {
      collaborations: base.collaborations,
      facets: {
        ...base.facets,
        collection: computed(() => {
          const col = base.facets.collection();
          if (col.state !== "loaded") return col;

          // An empty backing output reports "loaded" with no data yet
          // (e.g. an indexed-db output returns undefined for a missing
          // key). Treat that as an empty collection.
          const data = Array.isArray(col.data) ? col.data : [];
          if (data.length > 0) {
            return { state: "loaded", data };
          }

          if (isOutputInitialized(this.#selectedId())) {
            return { state: "loaded", data };
          }

          // Determine starting set
          return { state: "loaded", data: buildFacets(STARTING_SET_URIS) };
        }),

        save: async (newFacets) => {
          // Persist the facets first so the resulting (non-empty) data is
          // visible before the initialized flag flips, avoiding a flash of
          // the empty collection while a fresh output pops its starting set.
          await base.facets.save(newFacets);
          markOutputInitialized(this.#selectedId());
        },
      },

      playlistItems: base.playlistItems,
      settings: base.settings,
      tracks: base.tracks,
      ready: base.ready,
    };

    this.collaborations = manager.collaborations;
    this.facets = manager.facets;
    this.playlistItems = manager.playlistItems;
    this.settings = manager.settings;
    this.tracks = manager.tracks;
    this.ready = manager.ready;
  }

  // METHODS

  /**
   * The id of the current output, used to scope the "initialized" flag per
   * output. When connected to an output configurator this is the id of the
   * selected sub-output; when connected directly to an output element it is
   * that element's own id. Falls back to "local" (the default/local storage).
   */
  #selectedId() {
    const output = /** @type {OutputConfiguratorElement | undefined} */ (
      this.output.signal()
    );
    if (!output) return "local";
    return typeof output.selected === "function"
      ? output.selected()?.id ?? "local"
      : output.id || "local";
  }
}

export default InitialContentsTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = InitialContentsTransformer;
export const NAME = "dtor-initial-contents";

defineElement(NAME, CLASS);
