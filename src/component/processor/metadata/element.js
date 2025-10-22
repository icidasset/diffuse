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
class MetadataProcessor extends DiffuseElement {
  constructor() {
    super();

    // Group
    const group = crypto.randomUUID();

    // Setup worker
    const name = `diffuse/processor/metadata/${group}`;
    const url = new URL("./worker.js", import.meta.url);
    const worker = new Worker(url, { name, type: "module" });

    // Worker proxy
    this.supply = use("supply", worker);
  }
}

export default MetadataProcessor;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = MetadataProcessor;
export const NAME = "dp-metadata";

customElements.define(NAME, MetadataProcessor);
