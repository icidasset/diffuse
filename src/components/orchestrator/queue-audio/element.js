import { BroadcastableDiffuseElement, query } from "~/common/element.js";
import { untracked } from "~/common/signal.js";

/**
 * @import {InputElement} from "~/components/input/types.d.ts"
 * @import {OutputElement} from "~/components/output/types.d.ts"
 * @import RepeatShuffleEngine from "~/components/engine/repeat-shuffle/element.js"
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
class QueueAudioOrchestrator extends BroadcastableDiffuseElement {
  static NAME = "diffuse/orchestrator/queue-audio";

  // LIFE CYCLE

  /**
   * @override
   */
  async connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      this.broadcast(this.identifier, {});
    }

    // Super
    super.connectedCallback();

    /** @type {import("~/components/engine/audio/element.js").CLASS} */
    this.audio = query(this, "audio-engine-selector");

    /** @type {InputElement} */
    this.input = query(this, "input-selector");

    /** @type {OutputElement} */
    this.output = query(this, "output-selector");

    /** @type {import("~/components/engine/queue/element.js").CLASS} */
    this.queue = query(this, "queue-engine-selector");

    /** @type {RepeatShuffleEngine} */
    this.repeatShuffle = query(this, "repeat-shuffle-engine-selector");

    // Wait until defined
    await customElements.whenDefined(this.audio.localName);
    await customElements.whenDefined(this.input.localName);
    await customElements.whenDefined(this.queue.localName);
    await customElements.whenDefined(this.repeatShuffle.localName);

    // Effects
    this.effect(() => this.monitorActiveQueueItem());
    this.effect(() => this.monitorAudioEnd());
  }

  // 🛠️

  async monitorActiveQueueItem() {
    const audio = this.audio;
    const input = this.input;
    const queue = this.queue;

    if (!audio) return;
    if (!input) return;
    if (!queue) return;

    const activeItem = queue.now();
    const tracksCol = this.output?.tracks.collection();
    const tracks = tracksCol?.state === "loaded" ? tracksCol.data : undefined;

    const activeTrack = activeItem
      ? tracks?.find((t) => t.id === activeItem.id)
      : undefined;

    if ((await this.isLeader()) === false) return;

    const isPlaying = untracked(audio.isPlaying);

    // Resolve active URI
    const resolvedUri = activeTrack
      ? await input.resolve({ method: "GET", uri: activeTrack.uri })
      : undefined;

    // Check if we still need to render
    if (queue.now?.()?.id !== activeItem?.id) return;

    // Supply active track immediately
    // TODO: Take URL expiration timestamp into account
    // TODO: Add support for seeking streams
    //       (requires a lot of code, decoding audio frames, etc.)
    const activeAudio = activeTrack && resolvedUri
      ? [{
        id: activeTrack.id,
        isPreload: false,
        track: activeTrack,
        ...resolvedUri,
      }]
      : [];

    audio.supply({
      audio: activeAudio,
      play: activeItem && isPlaying ? { audioId: activeItem.id } : undefined,
    });
  }

  async monitorAudioEnd() {
    if (!this.audio) return;
    if (!this.queue) return;

    const now = this.queue.now();
    const aud = now ? this.audio.state(now.id) : undefined;

    if (aud?.hasEnded() && (await this.isLeader())) {
      if (this.repeatShuffle?.repeat() && now) {
        this.audio.seek({ audioId: now.id, currentTime: 0 });
        this.audio.play({ audioId: now.id });
        return;
      }

      await this.queue.shift();
    }
  }
}

export default QueueAudioOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = QueueAudioOrchestrator;
export const NAME = "do-queue-audio";

customElements.define(NAME, QueueAudioOrchestrator);
