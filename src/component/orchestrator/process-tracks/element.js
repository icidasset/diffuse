import deepDiff from "@fry69/deep-diff";

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
  async connectedCallback() {
    super.connectedCallback();

    /** @type {OutputElement | null} */
    const output = document.querySelector(this.outputSelector);

    // Check output element presence
    if (!output) throw new Error("Missing required `output` element");

    // Wait until defined
    await customElements.whenDefined(output.localName);

    // Process whenever tracks are loaded
    this.effect(() => {
      const state = output.tracks.state();
      if (state !== "loaded") return;

      this.process(output);
    });
  }

  /**
   * @param {OutputElement} output
   */
  async process(output) {
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

    // Wait until defined
    await customElements.whenDefined(input.localName);
    await customElements.whenDefined(metadataProcessor.localName);

    // Start
    this.#isProcessing.value = true;
    console.log("🪵 Processing initiated");

    const cachedTracks = output.tracks.collection();

    // Contextualize
    await input.contextualize(cachedTracks);

    // List
    const tracks = await input.list(cachedTracks);

    // Fetch metadata if needed
    // TODO: Parallelisation
    const tracksWithMetadata = await tracks.reduce(
      /**
       * @param {Promise<Track[]>} promise
       * @param {Track} track
       */
      async (promise, track) => {
        const acc = await promise;

        if (track.tags && track.stats) return [...acc, track];

        const resGet = await input.resolve({ method: "GET", uri: track.uri });
        const resHead = await input.resolve({ method: "HEAD", uri: track.uri });

        if (!resGet) return [...acc, track];

        const { stats, tags } = await metadataProcessor.supply({
          urls: { get: resGet.url, head: resHead?.url || resGet.url },
        });

        return [...acc, { ...track, stats, tags }];
      },
      Promise.resolve([]),
    );

    // Changed?
    const diff = deepDiff.diff(tracksWithMetadata, cachedTracks);
    const changed = !!diff;

    // Save if changed
    if (changed) await output.tracks.save(tracksWithMetadata);

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
