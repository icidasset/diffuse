import { BroadcastableDiffuseElement, query } from "@common/element.js";
import { signal, untracked } from "@common/signal.js";

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
class ProcessTracksOrchestrator extends BroadcastableDiffuseElement {
  static NAME = "diffuse/orchestrator/process-tracks";
  static WORKER_URL = "components/orchestrator/process-tracks/worker.js";

  /** @type {ProxiedActions<Actions>} */
  #proxy;

  constructor() {
    super();
    this.#proxy = this.workerProxy({
      forceNew: {
        dependencies: { input: true },
      },
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

    /** @type {import("@components/processor/metadata/element.js").CLASS} */
    const metadataProcessor = query(this, "metadata-processor-selector");

    // Assign to self
    this.input = input;
    this.output = output;
    this.metadataProcessor = metadataProcessor;

    // Wait until defined
    await customElements.whenDefined(output.localName);

    // Process whenever tracks are initially loaded
    if (this.hasAttribute("process-when-ready")) {
      this.effect(() => {
        const state = output.tracks.state();
        if (state !== "loaded") return;

        const skip = /** @type {any} */ (import.meta).env
          ?.DISABLE_AUTOMATIC_TRACKS_PROCESSING ?? false;
        if (skip) {
          // Should still trigger contextualize which `process` normally does for us.
          untracked(() => {
            input.contextualize(
              output.tracks.collection(),
            );
          });
          return;
        }

        untracked(() => this.process());
      });
    }
  }

  // WORKERS

  /**
   * @override
   */
  dependencies() {
    if (!this.input) throw new Error("Input element not defined yet");
    if (!this.metadataProcessor) {
      throw new Error("Metadata processor element not defined yet");
    }

    return {
      input: this.input,
      metadataProcessor: this.metadataProcessor,
    };
  }

  // ACTIONS

  async process() {
    if (!this.output) return;
    if (!(await this.isLeader())) return;

    // Start
    this.#isProcessing.value = true;
    console.log("🪵 Processing initiated");

    const cachedTracks = this.output.tracks.collection();
    const result = await this.#proxy.process(cachedTracks);

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
