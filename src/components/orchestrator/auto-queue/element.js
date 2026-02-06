import { BroadcastableDiffuseElement, query } from "@common/element.js";
import { untracked } from "@common/signal.js";

/**
 * @import {ProxiedActions} from "@common/worker.d.ts"
 * @import {InputElement} from "@components/input/types.d.ts"
 * @import {OutputElement} from "@components/output/types.d.ts"
 * @import RepeatShuffleEngine from "@components/engine/repeat-shuffle/element.js"
 *
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Update the queue pool whenever tracks have been loaded,
 * or the tracks collection changes.
 *
 * At the same time,
 */
class AutoTracksOrchestrator extends BroadcastableDiffuseElement {
  static NAME = "diffuse/orchestrator/auto-queue";
  static WORKER_URL = "components/orchestrator/auto-queue/worker.js";

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

    /** @type {import("@components/engine/queue/element.js").CLASS} */
    const queue = query(this, "queue-engine-selector");

    /** @type {RepeatShuffleEngine} */
    const repeatShuffle = query(this, "repeat-shuffle-engine-selector");

    // Assign to self
    this.input = input;
    this.output = output;
    this.queue = queue;
    this.repeatShuffle = repeatShuffle;

    // When defined
    await customElements.whenDefined(input.localName);
    await customElements.whenDefined(output.localName);
    await customElements.whenDefined(queue.localName);
    await customElements.whenDefined(repeatShuffle.localName);

    // Watch tracks collection
    this.effect(() => {
      const tracks = output.tracks.collection();

      this.isLeader().then((isLeader) => {
        if (!isLeader) return;
        untracked(() => this.#proxy.poolAvailable({ tracks }));
      });
    });

    // Automatically fill queue
    this.effect(() => {
      const trigger = queue.now();
      const _other_trigger = queue.supplyFingerprint();

      this.isLeader().then((isLeader) => {
        if (!isLeader) return;

        queue.fill({ amount: 10, shuffled: repeatShuffle.shuffle() });

        // Insert now-playing track if there's none
        if (!trigger) queue.shift();
      });
    });

    // TODO: Clear non-manual items from the queue
    //       when 'shuffle' gets turned off or on.
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

export default AutoTracksOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = AutoTracksOrchestrator;
export const NAME = "do-auto-queue";

customElements.define(NAME, CLASS);
