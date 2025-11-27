import {
  callWorkerWithProvisions,
  DiffuseElement,
  provisionWorkers,
  query,
  terminateWorkers,
} from "@common/element.js";
import { signal, untracked } from "@common/signal.js";
import {
  transfer,
  workerLink,
  workerProxy,
  workerTunnel,
} from "@common/worker.js";

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
 * Processes inputs into tracks whenever
 * the already existing tracks are loaded
 * from the assigned output element.
 */
class ProcessTracksOrchestrator extends DiffuseElement {
  static NAME = "diffuse/orchestrator/process-tracks";
  static WORKER_URL = "components/orchestrator/process-tracks/worker.js";

  /** @type {ProxiedActions<Actions>} */
  #proxy;

  /** @type {Promise<ProvisionedWorkers<"input" | "metadataProcessor">> | undefined} */
  #workers = undefined;

  constructor() {
    super();
    this.#proxy = workerProxy(this.workerLink);
  }

  // SIGNALS

  #isProcessing = signal(false);

  // STATE

  isProcessing = this.#isProcessing.get;

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();

    /** @type {InputElement} */
    const input = query(this, "input-selector");

    /** @type {OutputElement<Track[]>} */
    const output = query(this, "output-selector");

    /** @type {import("@components/processor/metadata/element.js").CLASS} */
    const metadataProcessor = query(this, "metadata-processor-selector");

    // Assign to self
    this.input = input;
    this.output = output;
    this.metadataProcessor = metadataProcessor;

    // Create new workers
    this.#workers = provisionWorkers({ input, metadataProcessor });

    // Wait until defined
    await customElements.whenDefined(output.localName);

    // Process whenever tracks are initially loaded
    this.effect(() => {
      const state = output.tracks.state();
      if (state !== "loaded") return;

      untracked(() => this.process());
    });
  }

  /**
   * @override
   */
  async disconnectedCallback() {
    super.disconnectedCallback();
    terminateWorkers(await this.#workers);
  }

  // ACTIONS

  async process() {
    if (!this.output) return;

    // Start
    this.#isProcessing.value = true;
    console.log("🪵 Processing initiated");

    const cachedTracks = this.output.tracks.collection();
    const result = await callWorkerWithProvisions(
      this.#workers,
      this.#proxy.process,
      { tracks: cachedTracks },
    );

    // Save if collection changed
    if (result) await this.output.tracks.save(result);

    // Fin
    console.log("🪵 Processing completed");
    this.#isProcessing.value = false;
  }
}

export default ProcessTracksOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ProcessTracksOrchestrator;
export const NAME = "do-process-tracks";

customElements.define(NAME, ProcessTracksOrchestrator);
