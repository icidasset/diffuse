import {
  callWorkerWithProvisions,
  DiffuseElement,
  provisionWorkers,
  query,
  terminateProvisions,
  workerProxy,
} from "@common/element.js";

/**
 * @import {Track} from "@definitions/types.d.ts"
 * @import {ProvisionedWorkers} from "@common/element.d.ts"
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
class SearchTracksOrchestrator extends DiffuseElement {
  static NAME = "diffuse/orchestrator/search-tracks";
  static WORKER_URL = "components/orchestrator/search-tracks/worker.js";

  /** @type {ProxiedActions<Actions>} */
  #proxy;

  /** @type {Promise<ProvisionedWorkers<"input" | "search">> | undefined} */
  #workers = undefined;

  constructor() {
    super();
    this.#proxy = workerProxy(this.workerLink);
  }

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();

    /** @type {InputElement} */
    const input = query(this, "input-selector");

    /** @type {OutputElement<Track[]>} */
    const output = query(this, "output-selector");

    /** @type {import("@components/processor/search/element.js").CLASS} */
    const search = query(this, "search-processor-selector");

    // Assign to self
    this.input = input;
    this.output = output;
    this.search = search;

    // Create new workers
    this.#workers = provisionWorkers({ input, search });

    // When defined
    await customElements.whenDefined(this.output.localName);

    // Watch tracks collection
    this.effect(() => {
      const tracks = output.tracks.collection().filter((t) =>
        t.kind !== "placeholder"
      );

      this.supplyAvailable(tracks);
    });
  }

  /**
   * @override
   */
  async disconnectedCallback() {
    super.disconnectedCallback();
    terminateProvisions(await this.#workers);
  }

  // 🚛

  /**
   * @param {Track[]} cachedTracks
   */
  async supplyAvailable(cachedTracks) {
    return await callWorkerWithProvisions(
      this.#workers,
      this.#proxy.supplyAvailable,
      { tracks: cachedTracks },
    );
  }
}

export default SearchTracksOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = SearchTracksOrchestrator;
export const NAME = "do-search-tracks";

customElements.define(NAME, SearchTracksOrchestrator);
