import { DiffuseElement, query } from "@common/element.js";
import { computed, signal } from "@common/signal.js";

/**
 * @import { OutputElement, OutputManager } from "../../../../output/types.d.ts"
 * @import { Track } from "@definitions/types.d.ts"
 */

class JsonStringOutputTransformer extends DiffuseElement {
  constructor() {
    super();

    /** @type {OutputManager<Track[]>} */
    const manager = {
      tracks: {
        collection: computed(() => {
          const json = this.#defined.value
            ? this.output?.tracks?.collection() ?? []
            : [];

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
        reload: () => this.output?.tracks?.reload() ?? Promise.resolve(),
        save: async (newTracks) => {
          const json = JSON.stringify(newTracks);

          if (!this.output) return;

          await customElements.whenDefined(this.output.localName);
          await this.output.tracks.save(json);
        },
        state: computed(() => this.output?.tracks?.state() ?? "loading"),
      },
    };

    // Assign manager properties to class
    this.tracks = manager.tracks;
  }

  /** @type {OutputElement<string> | undefined} */
  output = undefined;

  // SIGNALS

  #defined = signal(false);

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    /** @type {OutputElement<string>} */
    const output = query(this, "output-selector");
    this.output = output;

    // When defined
    customElements.whenDefined(this.output.localName).then(
      () => this.#defined.value = true,
    );
  }
}

export default JsonStringOutputTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = JsonStringOutputTransformer;
export const NAME = "dtos-json";

customElements.define(NAME, CLASS);
