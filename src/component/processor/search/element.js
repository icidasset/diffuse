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
class SearchProcessor extends DiffuseElement {
  constructor() {
    super();

    // Setup worker
    const name = `diffuse/processor/search/${this.group}`;
    const url = "/component/processor/search/worker.js";
    const worker = new Worker(url, { name, type: "module" });

    // Worker proxy
    this.search = use("search", worker);
    this.supply = use("supply", worker);
  }
}

export default SearchProcessor;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = SearchProcessor;
export const NAME = "dp-search";

customElements.define(NAME, SearchProcessor);
