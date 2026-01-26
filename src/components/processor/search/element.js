import { DiffuseElement } from "@common/element.js";
import { signal } from "@common/signal.js";
import { listen } from "@common/worker.js";

/**
 * @import {ProxiedActions} from "@common/worker.d.ts";
 * @import {Actions, State} from "./types.d.ts"
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

    /** @type {ProxiedActions<Actions & State>} */
    this.proxy = this.workerProxy();

    this.search = this.proxy.search;
    this.supply = this.proxy.supply;
  }

  // SIGNALS

  #supplyFingerprint = signal(/** @type {string | undefined} */ (undefined));

  // STATE

  supplyFingerprint = this.#supplyFingerprint.get;

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    // Sync data with worker
    const link = this.workerLink();

    // Listen for remote data changes
    listen("supplyFingerprint", this.#supplyFingerprint.set, link);

    // Fetch current data state
    this.proxy.supplyFingerprint().then(this.#supplyFingerprint.set);
  }
}

export default SearchProcessor;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = SearchProcessor;
export const NAME = "dp-search";

customElements.define(NAME, SearchProcessor);
