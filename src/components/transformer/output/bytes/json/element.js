import { computed } from "@common/signal.js";
import { OutputTransformer } from "../../base.js";

/**
 * @import { OutputManagerDeputy } from "@components/output/types.d.ts"
 * @import { Track } from "@definitions/types.d.ts"
 */

/**
 * @extends {OutputTransformer<Uint8Array>}
 */
class JsonStringOutputTransformer extends OutputTransformer {
  constructor() {
    super();

    const base = this.base();

    /** @type {OutputManagerDeputy} */
    const manager = {
      tracks: {
        ...base.tracks,
        collection: computed(() => {
          let data = base.tracks.collection();

          let json;

          if (data instanceof Uint8Array) {
            const decoder = new TextDecoder();
            json = decoder.decode(data);
          }

          if (typeof data !== "string") json = "[]";
          else json = data;

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
          const encoder = new TextEncoder();
          const bytes = encoder.encode(json);
          await base.tracks.save(bytes);
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
