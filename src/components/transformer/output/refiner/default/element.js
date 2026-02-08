import { computed } from "@common/signal.js";
import { OutputTransformer } from "../../base.js";

/**
 * @import { OutputManagerDeputy } from "../../../../output/types.d.ts"
 */

/**
 * @extends {OutputTransformer}
 */
class DefaultOutputRefinerTransformer extends OutputTransformer {
  constructor() {
    super();

    const base = this.base();

    /** @type {OutputManagerDeputy} */
    const manager = {
      constituents: {
        ...base.constituents,
        collection: computed(() => {
          return base.constituents.collection() ?? [];
        }),
      },
      tracks: {
        ...base.tracks,
        collection: computed(() => {
          return base.tracks.collection() ?? [];
        }),
        save: async (newTracks) => {
          const filtered = newTracks.filter((t) => !t.ephemeral);
          await base.tracks.save(filtered);
        },
      },
    };

    // Assign manager properties to class
    this.constituents = manager.constituents;
    this.tracks = manager.tracks;
  }
}

export default DefaultOutputRefinerTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = DefaultOutputRefinerTransformer;
export const NAME = "dtor-default";

customElements.define(NAME, CLASS);
