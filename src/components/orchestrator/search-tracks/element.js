import { DiffuseElement, query } from "@common/element.js";

/**
 * @import {InputElement, OutputElement, Track} from "@components/core/types.d.ts"
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
  constructor() {
    super();

    /** @type {InputElement} */
    this.input = query(this, "input-selector");

    /** @type {OutputElement} */
    this.output = query(this, "output-selector");

    /** @type {import("@components/processor/search/element.js").CLASS} */
    this.search = query(this, "search-processor-selector");
  }

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();

    // When defined
    await customElements.whenDefined(this.output.localName);

    // Watch tracks collection
    this.effect(() => {
      const tracks = this.output.tracks.collection();
      this.supplyAvailable(tracks);
    });
  }

  // 🚛

  /**
   * @param {Track[]} cachedTracks
   */
  async supplyAvailable(cachedTracks) {
    const groups = await this.input.groupConsult(cachedTracks);

    /** @type {Track[]} */
    let availableTracks = [];

    Object.values(groups).forEach((value) => {
      if (value.available === false) return;
      availableTracks = availableTracks.concat(value.tracks);
    }, []);

    // Set pool
    await this.search.supply(availableTracks);
  }
}

export default SearchTracksOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = SearchTracksOrchestrator;
export const NAME = "do-search-tracks";

customElements.define(NAME, SearchTracksOrchestrator);
