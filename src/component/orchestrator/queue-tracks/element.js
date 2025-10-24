import { DiffuseElement, query } from "@common/element.js";

/**
 * @import {InputElement, OutputElement, Track} from "@component/core/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Fill the queue automatically with tracks
 * whenever tracks have been loaded,
 * or the tracks collection changes.
 */
class QueueTracksOrchestrator extends DiffuseElement {
  constructor() {
    super();

    /** @type {InputElement} */
    this.input = query(this, "input-selector");

    /** @type {OutputElement} */
    this.output = query(this, "output-selector");

    /** @type {import("@component/engine/queue/element.js").CLASS} */
    this.queue = query(this, "queue-engine-selector");
  }

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();

    // Wait until defined
    await customElements.whenDefined(this.output.localName);

    // ...
    this.effect(() => {
      const tracks = this.output.tracks.collection();
      this.poolAvailable(tracks);
    });
  }

  // 🌊

  /**
   * @param {Track[]} cachedTracks
   */
  async poolAvailable(cachedTracks) {
    const groups = await this.input.groupConsult(cachedTracks);

    /** @type {Track[]} */
    let availableTracks = [];

    Object.values(groups).forEach((value) => {
      if (value.available === false) return;
      availableTracks = availableTracks.concat(value.tracks);
    }, []);

    // Set pool
    await this.queue.pool(availableTracks);
  }
}

export default QueueTracksOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = QueueTracksOrchestrator;
export const NAME = "do-queue-tracks";

customElements.define(NAME, QueueTracksOrchestrator);
