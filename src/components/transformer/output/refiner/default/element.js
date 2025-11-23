import { DiffuseElement, query } from "@common/element.js";
import { computed, signal } from "@common/signal.js";

/**
 * @import { OutputElement, OutputManager } from "../../../../output/types.d.ts"
 * @import { Track } from "@definitions/types.d.ts"
 */

class DefaultOutputRefinerTransformer extends DiffuseElement {
  constructor() {
    super();

    /** @type {OutputElement<Track[]>} */
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
          return $defined.value ? this.output.tracks?.collection() : [];
        }),
        reload: () => this.output.tracks.reload(),
        save: async (newTracks) => {
          const filtered = newTracks.filter((t) => !t.ephemeral);

          await customElements.whenDefined(this.output.localName);
          await this.output.tracks.save(filtered);
        },
        state: computed(() => this.output.tracks.state()),
      },
    };

    // Assign manager properties to class
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
