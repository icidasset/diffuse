import { DiffuseElement, query } from "@common/element.js";
import { signal, untracked } from "@common/signal.js";
import {
  transfer,
  workerLink,
  workerProxy,
  workerTunnel,
} from "@common/worker.js";

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
 * Processes inputs into tracks whenever
 * the already existing tracks are loaded
 * from the assigned output element.
 */
class ProcessTracksOrchestrator extends DiffuseElement {
  #process;

  /** @type {Promise<{ input: Worker | SharedWorker; metadataProcessor: Worker | SharedWorker }> | undefined} */
  #workers = undefined;

  static NAME = "diffuse/orchestrator/process-tracks";
  static WORKER_URL = "components/orchestrator/process-tracks/worker.js";

  constructor() {
    super();

    /** @type {ProxiedActions<Actions>} */
    const p = workerProxy(this.workerLink);

    this.#process = p.process;
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

    // Create new workers specially for track processing
    this.#workers = Promise.all([
      customElements.whenDefined(input.localName),
      customElements.whenDefined(metadataProcessor.localName),
    ]).then(() => {
      return {
        input: input.worker(),
        metadataProcessor: metadataProcessor.worker(),
      };
    });

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

    const workers = await this.#workers;

    if (workers?.input instanceof Worker) workers.input.terminate();
    if (workers?.metadataProcessor instanceof Worker) {
      workers.metadataProcessor.terminate();
    }
  }

  // ACTIONS

  async process() {
    const workers = await this.#workers;

    if (!workers) return;
    if (!this.output) return;

    // Start
    this.#isProcessing.value = true;
    console.log("🪵 Processing initiated");

    const cachedTracks = this.output.tracks.collection();

    // Establish channel between external workers and our processing worker
    const ports = {
      input: workerTunnel(workerLink(workers.input)),
      metadataProcessor: workerTunnel(workerLink(workers.metadataProcessor)),
    };

    // Send everything to worker
    const result = await this.#process(transfer({
      ports: {
        input: ports.input.port,
        metadataProcessor: ports.metadataProcessor.port,
      },
      tracks: cachedTracks,
    }, [
      ports.input.port,
      ports.metadataProcessor.port,
    ]));

    // Save if collection changed
    if (result) await this.output.tracks.save(result);

    // Close external channels
    ports.input.disconnect();
    ports.metadataProcessor.disconnect();

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
