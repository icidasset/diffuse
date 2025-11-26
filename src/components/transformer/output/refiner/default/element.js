import { DiffuseElement, query } from "@common/element.js";
import { computed, signal } from "@common/signal.js";

/**
 * @import { OutputElement, OutputManager } from "../../../../output/types.d.ts"
 * @import { Track } from "@definitions/types.d.ts"
 */

class DefaultOutputRefinerTransformer extends DiffuseElement {
  constructor() {
    super();

    /** @type {OutputManager<Track[]>} */
    const manager = {
      tracks: {
        collection: computed(() => {
          return this.#defined.value
            ? this.output?.tracks?.collection() ?? []
            : [];
        }),
        reload: () => this.output?.tracks?.reload() ?? Promise.resolve(),
        save: async (newTracks) => {
          const filtered = newTracks.filter((t) => !t.ephemeral);

          if (!this.output) return;

          await customElements.whenDefined(this.output.localName);
          await this.output.tracks.save(filtered);
        },
        state: computed(() => this.output?.tracks.state() ?? "loading"),
      },
    };

    // Assign manager properties to class
    this.tracks = manager.tracks;
  }

  /** @type {OutputElement<Track[]> | undefined} */
  output = undefined;

  // SIGNALS

  #defined = signal(false);

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    /** @type {OutputElement<Track[]>} */
    const output = query(this, "output-selector");
    this.output = output;

    // When defined
    customElements.whenDefined(this.output.localName).then(
      () => this.#defined.value = true,
    );
  }
}

export default DefaultOutputRefinerTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = DefaultOutputRefinerTransformer;
export const NAME = "dtor-default";

customElements.define(NAME, CLASS);
