import { computed } from "~/common/signal.js";
import { OutputTransformer } from "../../base.js";

/**
 * @import { OutputManagerDeputy } from "~/components/output/types.d.ts"
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
          return {
            state: "loaded",
            data: typeof col.data === "string" ? parseArray(col.data) : [],
          };
        }),
        save: async (newFacets) => {
          const json = JSON.stringify(newFacets);
          await base.facets.save(json);
        },
      },
      playlistItems: {
        ...base.playlistItems,
        collection: computed(() => {
          const col = base.playlistItems.collection();
          if (col.state !== "loaded") return col;
          return {
            state: "loaded",
            data: typeof col.data === "string" ? parseArray(col.data) : [],
          };
        }),
        save: async (newPlaylistItems) => {
          const json = JSON.stringify(newPlaylistItems);
          await base.playlistItems.save(json);
        },
      },
      settings: {
        ...base.settings,
        collection: computed(() => {
          const col = base.settings.collection();
          if (col.state !== "loaded") return col;
          return {
            state: "loaded",
            data: typeof col.data === "string" ? parseArray(col.data) : [],
          };
        }),
        save: async (newSettings) => {
          const json = JSON.stringify(newSettings);
          await base.settings.save(json);
        },
      },
      tracks: {
        ...base.tracks,
        collection: computed(() => {
          const col = base.tracks.collection();
          if (col.state !== "loaded") return col;
          return {
            state: "loaded",
            data: typeof col.data === "string" ? parseArray(col.data) : [],
          };
        }),
        save: async (newTracks) => {
          const json = JSON.stringify(newTracks);
          await base.tracks.save(json);
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

/**
 * @param {string} json
 */
function parseArray(json) {
  try {
    return JSON.parse(json);
  } catch (err) {
    console.error(err);
    return [];
  }
}

export default JsonStringOutputTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = JsonStringOutputTransformer;
export const NAME = "dtos-json";

customElements.define(NAME, CLASS);
