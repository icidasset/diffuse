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
    const nextItem = queue.future()[0] ?? null;

    const tracks = this.output?.tracks.collection();
    const activeTrack = activeItem
      ? tracks?.find((t) => t.id === activeItem.id)
      : undefined;
    const nextTrack = nextItem
      ? tracks?.find((t) => t.id === nextItem.id)
      : undefined;

    if ((await this.isLeader()) === false) return;

    const isPlaying = untracked(audio.isPlaying);

    // Resolve active URI
    const resolvedUri = activeTrack
      ? await input.resolve({ method: "GET", uri: activeTrack.uri })
      : undefined;

    if (resolvedUri && "stream" in resolvedUri) {
      throw new Error("Streams are not supported yet.");
    }

    const url = resolvedUri?.url;

    // Check if we still need to render
    if (queue.now?.()?.id !== activeItem?.id) return;

    // Supply active track immediately
    // TODO: Take URL expiration timestamp into account
    const activeAudio = activeItem && url
      ? [{ id: activeItem.id, isPreload: false, url }]
      : [];
    audio.supply({
      audio: activeAudio,
      play: activeItem && isPlaying ? { audioId: activeItem.id } : undefined,
    });

    // Preload next track after a delay
    clearTimeout(this._preloadTimeout);
    if (!nextTrack) return;

    this._preloadTimeout = setTimeout(async () => {
      const resolvedNextUri = await input.resolve({
        method: "GET",
        uri: nextTrack.uri,
      });
      const nextUrl = resolvedNextUri && !("stream" in resolvedNextUri)
        ? resolvedNextUri.url
        : undefined;
      if (!nextUrl) return;

      audio.supply({
        audio: [...activeAudio, {
          id: nextItem.id,
          isPreload: true,
          url: nextUrl,
        }],
      });
    }, 30_000);
  }

  async monitorAudioEnd() {
    if (!this.audio) return;
    if (!this.queue) return;

    const now = this.queue.now();
    const aud = now ? this.audio.state(now.id) : undefined;

    if (aud?.hasEnded() && (await this.isLeader())) {
      // TODO: Not sure yet if this is the best way to approach this.
      //       The idea is that scrobblers would more easily pick this up,
      //       as opposed to just resetting the audio.
      if (this.repeatShuffle?.repeat()) {
        const now = this.queue.now();
        if (now) {
          await this.queue.add({
            inFront: true,
            trackIds: [now.id],
          });
        }
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
