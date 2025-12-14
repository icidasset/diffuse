import { DiffuseElement, query } from "@common/element.js";
import { untracked } from "@common/signal.js";

/**
 * @import {InputElement} from "@components/input/types.d.ts"
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
  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();

    /** @type {InputElement} */
    this.input = query(this, "input-selector");

    /** @type {import("@components/engine/audio/element.js").CLASS} */
    this.audio = query(this, "audio-engine-selector");

    /** @type {import("@components/engine/queue/element.js").CLASS} */
    this.queue = query(this, "queue-engine-selector");

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
    if (!this.audio) return;
    if (!this.input) return;
    if (!this.queue) return;

    const activeTrack = this.queue.now();
    const isPlaying = untracked(this.audio.isPlaying);

    // Resolve URIs
    const resolvedUri = activeTrack
      ? await this.input.resolve({ method: "GET", uri: activeTrack.uri })
      : undefined;

    if (resolvedUri && "stream" in resolvedUri) {
      throw new Error("Streams are not supported yet.");
    }

    const url = resolvedUri?.url;

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
    if (!this.audio) return;
    if (!this.queue) return;

    const now = this.queue.now();
    const aud = now ? this.audio.state(now.id) : undefined;

    if (aud?.hasEnded()) await this.queue.shift();
  }
}

export default QueueAudioOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = QueueAudioOrchestrator;
export const NAME = "do-queue-audio";

customElements.define(NAME, QueueAudioOrchestrator);
