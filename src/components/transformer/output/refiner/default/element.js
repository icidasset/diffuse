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
      facets: {
        ...base.facets,
        collection: computed(() => {
          return base.facets.collection() ?? [];
        }),
      },
      themes: {
        ...base.themes,
        collection: computed(() => {
          return base.themes.collection() ?? [];
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
    this.facets = manager.facets;
    this.themes = manager.themes;
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
