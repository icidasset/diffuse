import deepDiff from "@fry69/deep-diff";

import { DiffuseElement, query } from "@common/element.js";
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

    /** @type {InputElement} */
    this.input = query(this, "input-selector");

    /** @type {OutputElement} */
    this.output = query(this, "output-selector");

    /** @type {import("@component/processor/metadata/element.js").CLASS} */
    this.metadataProcessor = query(this, "metadata-processor-selector");
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

      this.process();
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

    // Contextualize
    await this.input.contextualize(cachedTracks);

    // List
    const tracks = await this.input.list(cachedTracks);

    // Fetch metadata if needed
    const tracksWithMetadata = await tracks.reduce(
      /**
       * @param {Promise<Track[]>} promise
       * @param {Track} track
       */
      async (promise, track) => {
        const acc = await promise;

        if (track.tags && track.stats) return [...acc, track];

        const resGet = await this.input.resolve({
          method: "GET",
          uri: track.uri,
        });
        const resHead = await this.input.resolve({
          method: "HEAD",
          uri: track.uri,
        });

        if (!resGet) return [...acc, track];

        const { stats, tags } = await this.metadataProcessor.supply({
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
    if (changed) await this.output.tracks.save(tracksWithMetadata);

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
