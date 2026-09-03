import { computed } from "~/common/signal.js";
import { OutputTransformer } from "../../base.js";
import {
  decodeJsonCollection,
  saveJsonCollection,
} from "~/common/lens.js";
import { defineElement } from "~/common/element.js";

/**
 * @import { OutputManagerDeputy } from "@specs/components/output/types.d.ts"
 * @import { Facet, PlaylistItem, Setting, Track } from "~/definitions/types.d.ts"
 * @import { CollectionName } from "~/common/self-describing.js"
 */

/**
 * @extends {OutputTransformer<string>}
 */
class JsonStringOutputTransformer extends OutputTransformer {
  constructor() {
    super();

    const base = this.base();

    /** @type {OutputManagerDeputy} */
    const manager = {
      facets: {
        ...base.facets,
        collection: computed(() => {
          const col = base.facets.collection();
          if (col.state !== "loaded") return col;
          /** @type {Facet[]} */
          const data = decodeJsonCollection(col.data, "facets");
          return { state: "loaded", data };
        }),
        save: async (newFacets) => {
          await base.facets.save(
            /** @type {string} */ (
              await saveJsonCollection(newFacets, "facets", null)
            ),
          );
        },
      },
      playlistItems: {
        ...base.playlistItems,
        collection: computed(() => {
          const col = base.playlistItems.collection();
          if (col.state !== "loaded") return col;
          /** @type {PlaylistItem[]} */
          const data = decodeJsonCollection(col.data, "playlistItems");
          return { state: "loaded", data };
        }),
        save: async (newPlaylistItems) => {
          await base.playlistItems.save(
            /** @type {string} */ (
              await saveJsonCollection(newPlaylistItems, "playlistItems", null)
            ),
          );
        },
      },
      settings: {
        ...base.settings,
        collection: computed(() => {
          const col = base.settings.collection();
          if (col.state !== "loaded") return col;
          /** @type {Setting[]} */
          const data = decodeJsonCollection(col.data, "settings");
          return { state: "loaded", data };
        }),
        save: async (newSettings) => {
          await base.settings.save(
            /** @type {string} */ (
              await saveJsonCollection(newSettings, "settings", null)
            ),
          );
        },
      },
      tracks: {
        ...base.tracks,
        collection: computed(() => {
          const col = base.tracks.collection();
          if (col.state !== "loaded") return col;
          /** @type {Track[]} */
          const data = decodeJsonCollection(col.data, "tracks");
          return { state: "loaded", data };
        }),
        save: async (newTracks) => {
          await base.tracks.save(
            /** @type {string} */ (
              await saveJsonCollection(newTracks, "tracks", null)
            ),
          );
        },
      },

      // Other
      ready: base.ready,
    };

    // Assign manager properties to class
    this.facets = manager.facets;
    this.playlistItems = manager.playlistItems;
    this.settings = manager.settings;
    this.tracks = manager.tracks;
    this.ready = manager.ready;
  }
}

export default JsonStringOutputTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = JsonStringOutputTransformer;
export const NAME = "dtos-json";

defineElement(NAME, CLASS);