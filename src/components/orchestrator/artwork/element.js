import { DiffuseElement, query } from "~/common/element.js";

/**
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 * @import {Actions} from "~/components/artwork/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class ArtworkOrchestrator extends DiffuseElement {
  static NAME = "diffuse/orchestrator/artwork";
  static WORKER_URL = "components/orchestrator/artwork/worker.js";

  constructor() {
    super();

    /** @type {ProxiedActions<Actions>} */
    const p = this.workerProxy();

    this.get = p.get;
  }

  // LIFECYCLE

  /** @override */
  async connectedCallback() {
    super.connectedCallback();

    /** @type {import("~/components/configurator/artwork/element.js").CLASS} */
    this.artworkConfigurator = query(this, "artwork-selector");

    await customElements.whenDefined(this.artworkConfigurator.localName);
  }

  // WORKERS

  /**
   * @override
   */
  dependencies() {
    if (!this.artworkConfigurator) {
      throw new Error("Artwork configurator element not defined yet");
    }

    return {
      artwork: this.artworkConfigurator,
    };
  }
}

export default ArtworkOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ArtworkOrchestrator;
export const NAME = "do-artwork";

customElements.define(NAME, ArtworkOrchestrator);
