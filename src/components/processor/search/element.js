import { DiffuseElement } from "@common/element.js";

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
    const p = this.workerProxy();

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
