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
    };

    // Assign manager properties to class
    this.facets = manager.facets;
    this.tracks = manager.tracks;
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
