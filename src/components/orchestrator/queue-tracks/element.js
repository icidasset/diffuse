import {
  callWorkerWithProvisions,
  DiffuseElement,
  query,
  terminateProvisions,
  whenElementsDefined,
  workerProxy,
  workerTunnel,
} from "@common/element.js";
import { untracked } from "@common/signal.js";

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
 * Update the queue pool whenever
 * tracks have been loaded,
 * or the tracks collection changes.
 */
class QueueTracksOrchestrator extends DiffuseElement {
  static NAME = "diffuse/orchestrator/queue-tracks";
  static WORKER_URL = "components/orchestrator/queue-tracks/worker.js";

  /** @type {ProxiedActions<Actions>} */
  #proxy;

  /** @type {Promise<ProvisionedWorkers<"input" | "queue">> | undefined} */
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

    /** @type {import("@components/engine/queue/element.js").CLASS} */
    const queue = query(this, "queue-engine-selector");

    // Assign to self
    this.input = input;
    this.output = output;
    this.queue = queue;

    // Create new workers
    this.#workers = whenElementsDefined({ input, queue }).then(() => {
      return {
        input: input.createWorker(),
        queue: queue.worker(),
      };
    });

    // When defined
    await customElements.whenDefined(this.input.localName);
    await customElements.whenDefined(this.output.localName);

    // Watch tracks collection
    this.effect(() => {
      const tracks = output.tracks.collection().filter((t) =>
        t.kind !== "placeholder"
      );

      untracked(() => this.poolAvailable(tracks));
    });
  }

  /**
   * @override
   */
  async disconnectedCallback() {
    super.disconnectedCallback();
    terminateProvisions(await this.#workers);
  }

  // 🌊

  /**
   * @param {Track[]} cachedTracks
   */
  async poolAvailable(cachedTracks) {
    return await callWorkerWithProvisions(
      this.#workers,
      this.#proxy.poolAvailable,
      { tracks: cachedTracks },
    );
  }
}

export default QueueTracksOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = QueueTracksOrchestrator;
export const NAME = "do-queue-tracks";

customElements.define(NAME, QueueTracksOrchestrator);
