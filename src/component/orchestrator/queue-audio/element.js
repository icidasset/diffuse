import { DiffuseElement, query } from "@common/element.js";
import { untracked } from "@common/signal.js";

/**
 * @import {InputElement, OutputElement, Track} from "@component/core/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * When the active queue item changes,
 * coordinate the audio engine accordingly.
 *
 * Vice versa, when the audio ends,
 * shift the queue if needed.
 */
class QueueAudioOrchestrator extends DiffuseElement {
  constructor() {
    super();

    /** @type {InputElement} */
    this.input = query(this, "input-selector");

    /** @type {import("@component/engine/audio/element.js").CLASS} */
    this.audio = query(this, "audio-engine-selector");

    /** @type {import("@component/engine/queue/element.js").CLASS} */
    this.queue = query(this, "queue-engine-selector");
  }

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();

    // Wait until defined
    await customElements.whenDefined(this.audio.localName);
    await customElements.whenDefined(this.input.localName);
    await customElements.whenDefined(this.queue.localName);

    // Effects
    this.effect(() => this.monitorActiveQueueItem());
    this.effect(() => this.monitorAudioEnd());
  }

  // 🛠️

  async monitorActiveQueueItem() {
    const activeTrack = this.queue.now();
    const isPlaying = untracked(this.audio.isPlaying);

    // Resolve URIs
    const url = activeTrack
      ? await this.input.resolve({ method: "GET", uri: activeTrack.uri }).then(
        (a) => a?.url,
      )
      : undefined;

    // Check if we still need to render
    if (this.queue.now?.()?.id !== activeTrack?.id) return;

    // Play new active queue item
    // TODO: Take URL expiration timestamp into account
    // TODO: Preload next queue item
    this.audio.supply({
      audio: activeTrack && url
        ? [{
          id: activeTrack.id,
          isPreload: false,
          url,
        }]
        // TODO: Keep preloads
        : [],
      play: activeTrack && isPlaying ? { audioId: activeTrack.id } : undefined,
    });
  }

  async monitorAudioEnd() {
    if (this.audio.hasEnded()) await this.queue.shift();
  }
}

export default QueueAudioOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = QueueAudioOrchestrator;
export const NAME = "do-queue-audio";

customElements.define(NAME, QueueAudioOrchestrator);
