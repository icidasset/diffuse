import { computed } from "~/common/signal.js";
import { OutputTransformer } from "../../base.js";
import { defineElement } from "~/common/element.js";

/**
 * @import { OutputManagerDeputy } from "~/components/output/types.d.ts"
 * @import { Facet, PlaylistItem, Setting, Track } from "~/definitions/types.d.ts"
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
      facets: {
        ...base.facets,
        collection: computed(() => {
          const col = base.facets.collection();
          if (col.state !== "loaded") return col;
          /** @type {Facet[]} */
          const data = parseArray(col.data);
          return { state: "loaded", data };
        }),
        save: async (newFacets) => {
          const json = JSON.stringify(newFacets);
          const encoder = new TextEncoder();
          const bytes = encoder.encode(json);
          await base.facets.save(bytes);
        },
      },
      playlistItems: {
        ...base.playlistItems,
        collection: computed(() => {
          const col = base.playlistItems.collection();
          if (col.state !== "loaded") return col;
          /** @type {PlaylistItem[]} */
          const data = parseArray(col.data);
          return { state: "loaded", data };
        }),
        save: async (newPlaylistItems) => {
          const json = JSON.stringify(newPlaylistItems);
          const encoder = new TextEncoder();
          const bytes = encoder.encode(json);
          await base.playlistItems.save(bytes);
        },
      },
      settings: {
        ...base.settings,
        collection: computed(() => {
          const col = base.settings.collection();
          if (col.state !== "loaded") return col;
          /** @type {Setting[]} */
          const data = parseArray(col.data);
          return { state: "loaded", data };
        }),
        save: async (newSettings) => {
          const json = JSON.stringify(newSettings);
          const encoder = new TextEncoder();
          const bytes = encoder.encode(json);
          await base.settings.save(bytes);
        },
      },
      tracks: {
        ...base.tracks,
        collection: computed(() => {
          const col = base.tracks.collection();
          if (col.state !== "loaded") return col;
          /** @type {Track[]} */
          const data = parseArray(col.data);
          return { state: "loaded", data };
        }),
        save: async (newTracks) => {
          const json = JSON.stringify(newTracks);
          const encoder = new TextEncoder();
          const bytes = encoder.encode(json);
          await base.tracks.save(bytes);
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
 * @param {Uint8Array | string | undefined} data
 */
function parseArray(data) {
  let json;

  if (data instanceof Uint8Array) {
    const decoder = new TextDecoder();
    json = decoder.decode(data);
  } else if (data === undefined) {
    return [];
  } else {
    json = data;
  }

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
export const NAME = "dtob-json";

defineElement(NAME, CLASS);
