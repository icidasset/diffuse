import { computed } from "@common/signal.js";
import { OutputTransformer } from "../../base.js";

/**
 * @import { OutputManager } from "../../../../output/types.d.ts"
 * @import { Track } from "@definitions/types.d.ts"
 */

/**
 * @extends {OutputTransformer<string>}
 */
class JsonStringOutputTransformer extends OutputTransformer {
  constructor() {
    super();

    const base = this.base();

    /** @type {OutputManager<Track[]>} */
    const manager = {
      tracks: {
        ...base.tracks,
        collection: computed(() => {
          let json = base.tracks.collection();
          if (typeof json !== "string") json = "[]"

          // Try parsing JSON
          try {
            return JSON.parse(json);
          } catch (err) {
            console.error(
              "components/transformer/output/string/json: Failed to parse JSON",
            );
            return [];
          }
        }),
        save: async (newTracks) => {
          const json = JSON.stringify(newTracks);
          await base.tracks.save(json);
        },
      },
    };

    // Assign manager properties to class
    this.tracks = manager.tracks;
  }
}

export default JsonStringOutputTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = JsonStringOutputTransformer;
export const NAME = "dtos-json";

customElements.define(NAME, CLASS);
