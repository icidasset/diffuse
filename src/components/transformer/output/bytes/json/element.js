import { computed } from "@common/signal.js";
import { OutputTransformer } from "../../base.js";

/**
 * @import { OutputManagerDeputy } from "@components/output/types.d.ts"
 * @import { Facet, Track } from "@definitions/types.d.ts"
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
          const data = base.facets.collection();
          /** @type {Facet[]} */
          const c = parseArray(data);
          return c;
        }),
        save: async (newFacets) => {
          const json = JSON.stringify(newFacets);
          const encoder = new TextEncoder();
          const bytes = encoder.encode(json);
          await base.facets.save(bytes);
        },
      },
      tracks: {
        ...base.tracks,
        collection: computed(() => {
          const data = base.tracks.collection();
          /** @type {Track[]} */
          const c = parseArray(data);
          return c;
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
export const NAME = "dtos-json";

customElements.define(NAME, CLASS);
