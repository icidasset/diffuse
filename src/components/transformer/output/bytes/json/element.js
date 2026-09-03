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
 */

/**
 * @extends {OutputTransformer<Uint8Array>}
 */
class JsonBytesOutputTransformer extends OutputTransformer {
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
            /** @type {Uint8Array} */ (
              await saveJsonCollection(newFacets, "facets", null, true)
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
            /** @type {Uint8Array} */ (
              await saveJsonCollection(newPlaylistItems, "playlistItems", null, true)
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
            /** @type {Uint8Array} */ (
              await saveJsonCollection(newSettings, "settings", null, true)
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
            /** @type {Uint8Array} */ (
              await saveJsonCollection(newTracks, "tracks", null, true)
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

export default JsonBytesOutputTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = JsonBytesOutputTransformer;
export const NAME = "dtob-json";

defineElement(NAME, CLASS);