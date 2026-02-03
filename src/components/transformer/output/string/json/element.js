import { computed } from "@common/signal.js";
import { OutputTransformer } from "../../base.js";

/**
 * @import { OutputManagerDeputy } from "@components/output/types.d.ts"
 * @import { Track } from "@definitions/types.d.ts"
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
      constituents: {
        ...base.constituents,
        collection: computed(() => {
          const json = base.constituents.collection();
          return typeof json === "string" ? parseArray(json) : [];
        }),
        save: async (newConstituents) => {
          const json = JSON.stringify(newConstituents);
          await base.constituents.save(json);
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
    this.constituents = manager.constituents;
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
