import { DiffuseElement } from "@common/element.js";
import { signal } from "@common/signal.js";

/**
 * @import {InputElement, OutputElement, Track} from "@component/core/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class ProcessTracksOrchestrator extends DiffuseElement {
  constructor() {
    super();

    this.inputSelector = this.getAttribute("input-selector");
    this.outputSelector = this.getAttribute("output-selector");
    this.metadataProcessorSelector = this.getAttribute(
      "metadata-processor-selector",
    );

    if (!this.inputSelector) {
      throw new Error("Missing required `input-selector` attribute");
    }

    if (!this.outputSelector) {
      throw new Error("Missing required `output-selector` attribute");
    }

    if (!this.metadataProcessorSelector) {
      throw new Error(
        "Missing required `metadata-processor-selector` attribute",
      );
    }
  }

  // SIGNALS

  #isProcessing = signal(false);

  // STATE

  isProcessing = this.#isProcessing.get;

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    /** @type {OutputElement | null} */
    const output = document.querySelector(this.outputSelector);

    // Check output element presence
    if (!output) throw new Error("Missing required `output` element");

    // Process whenever tracks are loaded
    this.effect(async () => {
      // TODO: Make configurable
      await customElements.whenDefined("do-indexed-db");

      const state = output.tracks.state();
      console.log(state);
      if (state !== "loaded") return;

      this.process(output.tracks.collection());
    });
  }

  /**
   * @param {Track[]} cachedTracks
   */
  async process(cachedTracks) {
    this.#isProcessing.value = true;
    console.log("🪵 Processing initiated");

    /** @type {InputElement | null} */
    const input = document.querySelector(this.inputSelector);

    // TODO
    /** @type {any} */
    const metadataProcessor = document.querySelector(
      this.metadataProcessorSelector,
    );

    // Check element presence
    if (!input) throw new Error("Missing required `input` element");
    if (!metadataProcessor) {
      throw new Error("Missing required `metadata-processor` element");
    }

    // Contextualize
    await input.contextualize(cachedTracks);

    // List
    const tracks = await input.list(cachedTracks);

    console.log(tracks);

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
