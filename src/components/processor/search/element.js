import { DiffuseElement } from "@common/element.js";
import { workerProxy } from "@common/worker.js";

/**
 * @import {ProxiedActions} from "@common/worker.d.ts";
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<Actions>}
 */
class SearchProcessor extends DiffuseElement {
  static NAME = "diffuse/processor/search";
  static WORKER_URL = "components/processor/search/worker.js";

  constructor() {
    super();

    /** @type {ProxiedActions<Actions>} */
    const p = workerProxy(this.workerLink);

    this.search = p.search;
    this.supply = p.supply;
  }
}

export default SearchProcessor;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = SearchProcessor;
export const NAME = "dp-search";

customElements.define(NAME, SearchProcessor);
