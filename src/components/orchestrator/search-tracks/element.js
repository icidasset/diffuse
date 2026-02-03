import { BroadcastableDiffuseElement, query } from "@common/element.js";

/**
 * @import {Track} from "@definitions/types.d.ts"
 * @import {ProxiedActions} from "@common/worker.d.ts"
 * @import {InputElement} from "@components/input/types.d.ts"
 * @import {OutputElement} from "@components/output/types.d.ts"
 *
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Fill the search supply automatically with
 * tracks whenever they have been loaded,
 * or the tracks collection changes.
 */
class SearchTracksOrchestrator extends BroadcastableDiffuseElement {
  static NAME = "diffuse/orchestrator/search-tracks";
  static WORKER_URL = "components/orchestrator/search-tracks/worker.js";

  /** @type {ProxiedActions<Actions>} */
  #proxy;

  constructor() {
    super();
    this.#proxy = this.workerProxy({
      forceNew: {
        dependencies: {
          input: true,
        },
      },
    });
  }

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      this.broadcast(this.nameWithGroup, {});
    }

    // Super
    super.connectedCallback();

    /** @type {InputElement} */
    const input = query(this, "input-selector");

    /** @type {OutputElement} */
    const output = query(this, "output-selector");

    /** @type {import("@components/processor/search/element.js").CLASS} */
    const search = query(this, "search-processor-selector");

    // Assign to self
    this.input = input;
    this.output = output;
    this.search = search;

    // When defined
    await customElements.whenDefined(this.output.localName);

    // Watch tracks collection
    this.effect(async () => {
      const collection = output.tracks.collection();
      if ((await this.isLeader()) === false) return;
      this.#proxy.supplyAvailable(collection);
    });
  }

  // WORKERS

  /**
   * @override
   */
  dependencies() {
    if (!this.input) throw new Error("Input element not defined yet");
    if (!this.search) throw new Error("Search element not defined yet");

    return {
      input: this.input,
      search: this.search,
    };
  }
}

export default SearchTracksOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = SearchTracksOrchestrator;
export const NAME = "do-search-tracks";

customElements.define(NAME, SearchTracksOrchestrator);
