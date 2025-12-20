import { DiffuseElement } from "@common/element.js";
import { computed, signal } from "@common/signal.js";

/**
 * @import {ProxiedActions} from "@common/worker.d.ts"
 * @import {Track} from "@definitions/types.d.ts"
 * @import {OutputManager, OutputElement} from "@components/output/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {OutputManager<Track[]>}
 */
class OutputConfigurator extends DiffuseElement {
  static NAME = "diffuse/configurator/output";
  static WORKER_URL = "components/configurator/output/worker.js";

  constructor() {
    super();

    /** @type {OutputManager<Track[]>} */
    const manager = {
      tracks: {
        collection: computed(() => {
          return this.#memory.tracks.value;
        }),
        reload: async () => {},
        save: async (newTracks) => {
          this.#memory.tracks.value = newTracks;
        },
        state: () => "loaded",
      },
    };

    // Assign manager properties to class
    this.tracks = manager.tracks;
  }

  // SIGNALS

  #memory = {
    tracks: signal(/** @type {Track[]} */ ([])),
  };

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();
  }
}

export default OutputConfigurator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = OutputConfigurator;
export const NAME = "dc-output";

customElements.define(NAME, CLASS);
