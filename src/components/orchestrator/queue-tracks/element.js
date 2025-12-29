import { BroadcastableDiffuseElement, query } from "@common/element.js";
import { untracked } from "@common/signal.js";

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
 * Update the queue pool whenever
 * tracks have been loaded,
 * or the tracks collection changes.
 */
class QueueTracksOrchestrator extends BroadcastableDiffuseElement {
  static NAME = "diffuse/orchestrator/queue-tracks";
  static WORKER_URL = "components/orchestrator/queue-tracks/worker.js";

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

    /** @type {OutputElement<Track[]>} */
    const output = query(this, "output-selector");

    /** @type {import("@components/engine/queue/element.js").CLASS} */
    const queue = query(this, "queue-engine-selector");

    // Assign to self
    this.input = input;
    this.output = output;
    this.queue = queue;

    // When defined
    await customElements.whenDefined(this.input.localName);
    await customElements.whenDefined(this.output.localName);
    await customElements.whenDefined(this.queue.localName);

    // Watch tracks collection
    this.effect(() => {
      const tracks = output.tracks.collection();

      this.isLeader().then((isLeader) => {
        if (!isLeader) return;
        untracked(() => this.#proxy.poolAvailable(tracks));
      });
    });

    // 🌸
  }

  // WORKERS

  /**
   * @override
   */
  dependencies() {
    if (!this.input) throw new Error("Input element not defined yet");
    if (!this.queue) throw new Error("Queue element not defined yet");

    return {
      input: this.input,
      queue: this.queue,
    };
  }
}

export default QueueTracksOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = QueueTracksOrchestrator;
export const NAME = "do-queue-tracks";

customElements.define(NAME, QueueTracksOrchestrator);
