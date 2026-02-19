import { computed } from "@common/signal.js";
import { OutputTransformer } from "../../base.js";

/**
 * @import { OutputManagerDeputy } from "@components/output/types.d.ts"
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
          const json = base.facets.collection();
          return typeof json === "string" ? parseArray(json) : [];
        }),
        save: async (newFacets) => {
          const json = JSON.stringify(newFacets);
          await base.facets.save(json);
        },
      },
      playlistItems: {
        ...base.playlistItems,
        collection: computed(() => {
          const json = base.playlistItems.collection();
          return typeof json === "string" ? parseArray(json) : [];
        }),
        save: async (newPlaylistItems) => {
          const json = JSON.stringify(newPlaylistItems);
          await base.playlistItems.save(json);
        },
      },
      themes: {
        ...base.themes,
        collection: computed(() => {
          const json = base.themes.collection();
          return typeof json === "string" ? parseArray(json) : [];
        }),
        save: async (newThemes) => {
          const json = JSON.stringify(newThemes);
          await base.themes.save(json);
        },
      },
      tracks: {
        ...base.tracks,
        collection: computed(() => {
          const json = base.tracks.collection();
          return typeof json === "string" ? parseArray(json) : [];
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
    this.themes = manager.themes;
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
