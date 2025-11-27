import { DiffuseElement, query } from "@common/element.js";
import { signal, untracked } from "@common/signal.js";
import {
  portProvider,
  transfer,
  workerLink,
  workerProxy,
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
  /** @type {Promise<{ input: Worker | SharedWorker; metadataProcessor: Worker | SharedWorker } | undefined>} */
  #external = Promise.resolve(undefined);
  #process;

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
    this.input = query(this, "input-selector");

    /** @type {OutputElement<Track[]>} */
    this.output = query(this, "output-selector");

    /** @type {import("@components/processor/metadata/element.js").CLASS} */
    this.metadataProcessor = query(this, "metadata-processor-selector");

    // Create new workers specially for track processing
    this.#external = Promise.all([
      customElements.whenDefined(this.input.localName),
      customElements.whenDefined(this.metadataProcessor.localName),
    ]).then(() => {
      if (!this.input) return undefined;
      if (!this.metadataProcessor) return undefined;

      return {
        input: this.input.worker(),
        metadataProcessor: this.metadataProcessor.worker(),
      };
    });

    // Wait until defined
    await customElements.whenDefined(this.output.localName);

    // Process whenever tracks are initially loaded
    this.effect(() => {
      if (!this.output) return;

      const state = this.output.tracks.state();
      if (state !== "loaded") return;

      untracked(() => this.process());
    });
  }

  /**
   * @override
   */
  async disconnectedCallback() {
    super.disconnectedCallback();

    const ext = await this.#external;
    if (!ext) return;

    if (ext.input instanceof Worker) ext.input.terminate();
    if (ext.metadataProcessor instanceof Worker) {
      ext.metadataProcessor.terminate();
    }
  }

  // ACTIONS

  async process() {
    const ext = await this.#external;

    if (!ext) return;
    if (!this.output) return;

    // Start
    this.#isProcessing.value = true;
    console.log("🪵 Processing initiated");

    const cachedTracks = this.output.tracks.collection();

    // Establish channel between external workers and our processing worker
    const ports = {
      input: portProvider(() => workerLink(ext.input))(),
      metadataProcessor: portProvider(() =>
        workerLink(ext.metadataProcessor)
      )(),
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
