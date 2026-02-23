import { BroadcastableDiffuseElement, query } from "@common/element.js";

/**
 * @import {DiffuseElement} from "@common/element.js";
 * @import {SignalReader} from "@common/signal.d.ts";
 * @import {Track} from "@definitions/types.d.ts"
 * @import RepeatShuffleEngine from "@components/engine/repeat-shuffle/element.js"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Update the queue pool whenever tracks have been loaded,
 * or the tracks collection changes.
 */
class AutoTracksOrchestrator extends BroadcastableDiffuseElement {
  static NAME = "diffuse/orchestrator/auto-queue";

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

    /** @type {import("@components/engine/queue/element.js").CLASS} */
    const queue = query(this, "queue-engine-selector");

    /** @type {RepeatShuffleEngine} */
    const repeatShuffle = query(this, "repeat-shuffle-engine-selector");

    /** @type {DiffuseElement & { tracks: SignalReader<Track[]> }} */
    const tracksElement = query(this, "tracks-selector");

    // When defined
    await customElements.whenDefined(queue.localName);
    await customElements.whenDefined(repeatShuffle.localName);
    await customElements.whenDefined(tracksElement.localName);

    // Watch tracks
    this.effect(() => {
      const tracks = tracksElement.tracks();

      this.isLeader().then(async (isLeader) => {
        if (!isLeader) return;
        queue.supply({ trackIds: tracks.map((t) => t.id) });
      });
    });

    // Automatically fill queue
    let lastShuffle = repeatShuffle.shuffle();
    let lastFingerprint = queue.supplyFingerprint();

    this.effect(() => {
      const trigger = queue.now();
      const fingerprint = queue.supplyFingerprint();
      const shuffled = repeatShuffle.shuffle();

      this.isLeader().then((isLeader) => {
        if (!isLeader) return;

        // Clear non-manual items from the queue
        // when 'shuffle' gets turned off or on;
        // or when queue supply changes.
        if (shuffled !== lastShuffle || fingerprint !== lastFingerprint) {
          lastShuffle = shuffled;
          lastFingerprint = fingerprint;
          queue.clear({ manualOnly: true });
        }

        queue.fill({ amount: 10, shuffled: repeatShuffle.shuffle() });

        // Insert now-playing track if there's none
        if (!trigger) queue.shift();
      });
    });
  }
}

export default AutoTracksOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = AutoTracksOrchestrator;
export const NAME = "do-auto-queue";

customElements.define(NAME, CLASS);
