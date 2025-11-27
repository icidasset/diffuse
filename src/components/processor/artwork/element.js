import { DiffuseElement } from "@common/element.js";
import { workerProxy } from "@common/worker.js";

/**
 * @import {ProxiedActions} from "@common/worker.d.ts"
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<Actions>}
 */
class ArtworkProcessor extends DiffuseElement {
  static NAME = "diffuse/processor/artwork";
  static WORKER_URL = "components/processor/artwork/worker.js";

  constructor() {
    super();

    /** @type {ProxiedActions<Actions>} */
    const p = workerProxy(this.workerLink);

    this.artwork = p.artwork;
    this.supply = p.supply;
  }
}

export default ArtworkProcessor;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ArtworkProcessor;
export const NAME = "dp-artwork";

customElements.define(NAME, ArtworkProcessor);
