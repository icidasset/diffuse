import { DiffuseElement } from "@common/element.js";
import { use } from "@common/worker.js";

/**
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {Actions}
 */
class ArtworkProcessor extends DiffuseElement {
  constructor() {
    super();

    // Setup worker
    const name = `diffuse/processor/metadata/${this.group}`;
    const url = new URL("./worker.js", import.meta.url);
    const worker = new Worker(url, { name, type: "module" });

    // Worker proxy
    this.artwork = use("artwork", worker);
    this.supply = use("supply", worker);
  }
}

export default ArtworkProcessor;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ArtworkProcessor;
export const NAME = "dp-artwork";

customElements.define(NAME, ArtworkProcessor);
