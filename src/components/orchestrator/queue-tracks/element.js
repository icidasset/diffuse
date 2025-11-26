import { DiffuseElement, query } from "@common/element.js";
import { untracked } from "@common/signal.js";

/**
 * @import {Track} from "@definitions/types.d.ts"
 * @import {InputElement} from "@components/input/types.d.ts"
 * @import {OutputElement} from "@components/output/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Update the queue pool whenever
 * tracks have been loaded,
 * or the tracks collection changes.
 */
class QueueTracksOrchestrator extends DiffuseElement {
  constructor() {
    super();

    /** @type {InputElement} */
    this.input = query(this, "input-selector");

    /** @type {OutputElement<Track[]>} */
    this.output = query(this, "output-selector");

    /** @type {import("@components/engine/queue/element.js").CLASS} */
    this.queue = query(this, "queue-engine-selector");
  }

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();

    // When defined
    await customElements.whenDefined(this.input.localName);
    await customElements.whenDefined(this.output.localName);

    // Watch tracks collection
    this.effect(() => {
      const tracks = this.output.tracks.collection().filter((t) =>
        t.kind !== "placeholder"
      );

      untracked(() => this.poolAvailable(tracks));
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
