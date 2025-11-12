import { DiffuseElement, query } from "@common/element.js";
import { signal, untracked } from "@common/signal.js";
import { getTransferables, use } from "@common/worker.js";

/**
 * @import {InputElement, Track} from "@common/types.d.ts"
 * @import {OutputElement} from "@components/output/types.d.ts"
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

  constructor() {
    super();

    // Setup worker
    const name = `diffuse/orchestrator/process-tracks/${this.group}`;
    const url = "/components/orchestrator/process-tracks/worker.js";
    const worker = new Worker(url, { name, type: "module" });

    /** @type {InputElement} */
    this.input = query(this, "input-selector");

    /** @type {OutputElement<Track[]>} */
    this.output = query(this, "output-selector");

    /** @type {import("@components/processor/metadata/element.js").CLASS} */
    this.metadataProcessor = query(this, "metadata-processor-selector");

    // Worker proxy
    this.#process = use("process", worker, {
      transfer: getTransferables,
    });
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

    // Wait until defined
    await customElements.whenDefined(this.output.localName);

    // Process whenever tracks are initially loaded
    this.effect(() => {
      const state = this.output.tracks.state();
      if (state !== "loaded") return;

      untracked(() => this.process());
    });
  }

  // ACTIONS

  async process() {
    await customElements.whenDefined(this.input.localName);
    await customElements.whenDefined(this.metadataProcessor.localName);

    // Start
    this.#isProcessing.value = true;
    console.log("🪵 Processing initiated");

    const cachedTracks = this.output.tracks.collection();

    // External ports
    const ports = {
      input: this.input.port(),
      metadataProcessor: this.metadataProcessor.port(),
    };

    console.log(ports);

    // Send everything to worker
    const result = await this.#process({
      ports: {
        input: ports.input.port,
        metadataProcessor: ports.metadataProcessor.port,
      },
      tracks: cachedTracks,
    });

    console.log(result);

    // Save if changed
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
