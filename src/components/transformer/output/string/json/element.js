import { DiffuseElement, query } from "@common/element.js";
import { computed, signal } from "@common/signal.js";

/**
 * @import { OutputElement, OutputManager } from "../../../../output/types.d.ts"
 * @import { Track } from "@common/types.d.ts"
 */

class JsonStringOutputTransformer extends DiffuseElement {
  constructor() {
    super();

    /** @type {OutputElement<string>} */
    this.output = query(this, "output-selector");

    // whenDefined signal
    const $defined = signal(false);

    customElements.whenDefined(this.output.localName).then(
      () => $defined.value = true,
    );

    /** @type {OutputManager<Track[]>} */
    const manager = {
      tracks: {
        collection: computed(() => {
          const json = $defined.value ? this.output.tracks?.collection() : [];

          // In addition to the above, Some polymorphic outputs
          // use an empty array as the default return value.
          if (Array.isArray(json)) return json;

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
        reload: () => this.output.tracks.reload(),
        save: async (newTracks) => {
          const json = JSON.stringify(newTracks);

          await customElements.whenDefined(this.output.localName);
          await this.output.tracks.save(json);
        },
        state: computed(() => this.output.tracks.state()),
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
