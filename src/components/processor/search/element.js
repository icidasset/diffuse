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
    const url = "/components/processor/search/worker.js";

    let port;

    if (this.hasAttribute("group")) {
      const worker = new SharedWorker(url, { name, type: "module" });
      port = worker.port;
      port.start();
    } else {
      const worker = new Worker(url, { name, type: "module" });
      port = worker;
    }

    // Worker proxy
    this.search = use("search", port);
    this.supply = use("supply", port);
  }
}

export default SearchProcessor;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = SearchProcessor;
export const NAME = "dp-search";

customElements.define(NAME, SearchProcessor);
