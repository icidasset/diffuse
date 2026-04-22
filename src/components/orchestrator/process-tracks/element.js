import {
  BroadcastableDiffuseElement,
  defineElement,
  query,
} from "~/common/element.js";
import { data, mergeTracks } from "~/common/output.js";
import { signal, untracked } from "~/common/signal.js";
import { listen } from "~/common/worker.js";

/**
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 * @import {InputElement} from "~/components/input/types.d.ts"
 * @import {OutputElement} from "~/components/output/types.d.ts"
 * @import {Track} from "~/definitions/types.d.ts"
 * @import MetadataConfigurator from "~/components/configurator/metadata/element.js"
 *
 * @import {Actions, Progress} from "./types.d.ts"
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
  #performedInitialProcess = signal(false);
  #progress = signal(/** @type {Progress} */ ({ processed: 0, total: 0 }));

  // STATE

  isProcessing = this.#isProcessing.get;
  progress = this.#progress.get;

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(this.identifier, {
        getPerfInit: {
          strategy: "leaderOnly",
          fn: this.#performedInitialProcess.get,
        },
        setPerfInit: {
          strategy: "replicate",
          fn: this.#performedInitialProcess.set,
        },
        getIsProcessing: {
          strategy: "leaderOnly",
          fn: this.#isProcessing.get,
        },
        setIsProcessing: {
          strategy: "replicate",
          fn: this.#isProcessing.set,
        },
        process: { strategy: "leaderOnly", fn: this.process },
      });

      if (!actions) return;

      this.process = actions.process;
      this.#isProcessing.set = actions.setIsProcessing;

      // Sync #performedInitialProcess and #isProcessing with leader
      actions.getPerfInit().then((val) => {
        this.#performedInitialProcess.value = val;
      });

      actions.getIsProcessing().then((val) => {
        this.#isProcessing.value = val;
      });
    }

    // Super
    super.connectedCallback();

    /** @type {InputElement} */
    const input = query(this, "input-selector");

    /** @type {OutputElement} */
    const output = query(this, "output-selector");

    /** @type {MetadataConfigurator} */
    const metadataConfigurator = query(this, "metadata-selector");

    // Assign to self
    this.input = input;
    this.output = output;
    this.metadataConfigurator = metadataConfigurator;

    // Worker link
    const link = this.workerLink();

    // Wait until defined
    await customElements.whenDefined(input.localName);
    await customElements.whenDefined(output.localName);
    await customElements.whenDefined(metadataConfigurator.localName);

    // Sync progress with worker
    listen("progress", this.#progress.set, link);
    this.#proxy.progress().then(this.#progress.set);

    //
    listen("list", /** @param {Track[]} tracks */ async (tracks) => {
      if (!this.output) return;
      this.output.tracks.save(tracks);
    }, link);

    // Save intermediate batches as they arrive so progress isn't lost.
    // Merge with tracks not present in the batch to avoid overwriting them.
    listen("batch", /** @param {Track[]} tracks */ async (tracks) => {
      if (!this.output) return;
      const existing = await data(this.output.tracks);
      this.output.tracks.save(mergeTracks(existing, tracks));
    }, link);

    // Process whenever tracks are initially loaded;
    // unless already done so (possibly through another instance of this element)
    if (this.hasAttribute("process-when-ready")) {
      let unregistered = false;

      const unregister = this.effect(() => {
        if (unregistered) {
          unregister();
          return;
        }

        const col = output.tracks.collection();
        if (col.state !== "loaded") return;

        if (this.#performedInitialProcess.value) {
          unregistered = true;
          return;
        }

        this.#performedInitialProcess.set(true);

        const skip = /** @type {any} */ (import.meta).env
          ?.DISABLE_AUTOMATIC_TRACKS_PROCESSING ?? false;
        if (skip) return;

        unregistered = true;
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
    if (!this.metadataConfigurator) {
      throw new Error("Metadata configurator element not defined yet");
    }

    return {
      input: this.input,
      metadata: this.metadataConfigurator,
    };
  }

  // ACTIONS

  async process() {
    if (!this.output) return;
    if (this.#isProcessing.value) return;

    // Start
    this.#isProcessing.set(true);
    console.log("🪵 Processing initiated");

    const cachedTracks = await data(this.output.tracks);
    const result = await this.#proxy.process(cachedTracks);

    if (result) {
      await this.output.tracks.save(mergeTracks(cachedTracks, result));
    }

    // Fin
    console.log("🪵 Processing completed");
    this.#isProcessing.set(false);
  }
}

export default ProcessTracksOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ProcessTracksOrchestrator;
export const NAME = "do-process-tracks";

defineElement(NAME, ProcessTracksOrchestrator);
