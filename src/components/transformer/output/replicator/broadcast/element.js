import { computed } from "@common/signal.js";
import { OutputTransformer } from "../../base.js";

/**
 * @import { OutputManagerDeputy } from "../../../../output/types.d.ts"
 */

/**
 * @extends {OutputTransformer}
 */
class BroadcastOutputReplicatorTransformer extends OutputTransformer {
  static NAME = "diffuse/transformer/output/replicator/broadcast";

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
      playlistItems: {
        ...base.playlistItems,
        collection: computed(() => {
          return base.playlistItems.collection() ?? [];
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

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(this.nameWithGroup, {
        saveFacets: { strategy: "replicate", fn: this.facets.save },
        savePlaylistItems: {
          strategy: "replicate",
          fn: this.playlistItems.save,
        },
        saveThemes: { strategy: "replicate", fn: this.themes.save },
        saveTracks: { strategy: "replicate", fn: this.tracks.save },
      });

      if (actions) {
        this.facets.save = actions.saveFacets;
        this.playlistItems.save = actions.savePlaylistItems;
        this.themes.save = actions.saveThemes;
        this.tracks.save = actions.saveTracks;
      }
    }

    // Super
    super.connectedCallback();
  }
}

export default BroadcastOutputReplicatorTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = BroadcastOutputReplicatorTransformer;
export const NAME = "dtor-broadcast";

customElements.define(NAME, CLASS);
