import {
  BroadcastableDiffuseElement,
  query,
  queryOptional,
} from "@common/element.js";

/**
 * @import {OutputElement} from "@components/output/types.d.ts"
 * @import {Artwork} from "@components/processor/artwork/types.d.ts"
 * @import ArtworkProcessor from "@components/processor/artwork/element.js"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Keeps the browser/OS Media Session in sync with queue and audio engine state.
 *
 * Forwards play, pause, seek and track-skip actions from the OS back to the engines.
 */
class MediaSessionOrchestrator extends BroadcastableDiffuseElement {
  static NAME = "diffuse/orchestrator/media-session";

  // LIFECYCLE

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

    if (!("mediaSession" in navigator)) return;

    /** @type {import("@components/engine/audio/element.js").CLASS} */
    this.audio = query(this, "audio-engine-selector");

    /** @type {import("@components/engine/queue/element.js").CLASS} */
    this.queue = query(this, "queue-engine-selector");

    /** @type {OutputElement | null} */
    this.output = queryOptional(this, "output-selector");

    /** @type {ArtworkProcessor | null} */
    this.artwork = queryOptional(this, "artwork-processor-selector");

    // Wait until defined
    await customElements.whenDefined(this.audio.localName);
    await customElements.whenDefined(this.queue.localName);
    if (this.output) await customElements.whenDefined(this.output.localName);
    if (this.artwork) await customElements.whenDefined(this.artwork.localName);

    // Register Media Session action handlers
    this.#registerActionHandlers();

    // Effects
    this.effect(() => this.#syncMetadata());
    this.effect(() => this.#syncPlaybackState());
    this.effect(() => this.#syncPositionState());
  }

  // 🛠️

  async #syncMetadata() {
    if (!this.queue) return;

    const now = this.queue.now();
    const track = now && this.output
      ? this.output.tracks.collection().find((t) => t.id === now.id)
      : undefined;

    if (!track) {
      navigator.mediaSession.metadata = null;
      return;
    }

    const tags = track.tags ?? {};

    navigator.mediaSession.metadata = new MediaMetadata({
      title: tags.title ?? "",
      artist: tags.artist ?? tags.albumartist ?? "",
      album: tags.album ?? "",
      artwork: [],
    });

    // Optionally fetch and attach artwork
    if (this.artwork) {
      const artworkProcessor = this.artwork;

      /** @type {Artwork[]} */
      let artworkItems;

      try {
        artworkItems = await artworkProcessor.artwork({
          cacheId: track.id,
          tags,
        });
      } catch {
        artworkItems = [];
      }

      if (artworkItems?.length && navigator.mediaSession.metadata) {
        const { bytes, mime } = artworkItems[0];
        const blob = new Blob([/** @type {ArrayBuffer} */ (bytes.buffer)], {
          type: mime,
        });

        const url = URL.createObjectURL(blob);
        const nowLater = this.queue.now();

        // If in the meantime the now-playing track has changed,
        // don't set the artwork.
        if (nowLater?.id !== now?.id) return;

        navigator.mediaSession.metadata.artwork = [
          { src: url, type: mime },
        ];
      }
    }
  }

  #syncPlaybackState() {
    if (!this.audio) return;
    navigator.mediaSession.playbackState = this.audio.isPlaying()
      ? "playing"
      : "paused";
  }

  #syncPositionState() {
    if (!this.audio || !this.queue) return;

    const now = this.queue.now();
    if (!now) return;

    const state = this.audio.state(now.id);
    if (!state) return;

    const duration = state.duration();
    const progress = state.progress();

    if (!duration || isNaN(duration) || duration === 0) return;

    try {
      navigator.mediaSession.setPositionState({
        duration,
        position: duration * progress,
        playbackRate: 1,
      });
    } catch {
      // setPositionState may throw if duration is not finite
    }
  }

  #registerActionHandlers() {
    navigator.mediaSession.setActionHandler("play", async () => {
      if (!this.audio || !this.queue) return;
      if (!(await this.isLeader())) return;
      const now = this.queue.now();
      if (now) this.audio.play({ audioId: now.id });
    });

    navigator.mediaSession.setActionHandler("pause", async () => {
      if (!this.audio || !this.queue) return;
      if (!(await this.isLeader())) return;
      const now = this.queue.now();
      if (now) this.audio.pause({ audioId: now.id });
    });

    navigator.mediaSession.setActionHandler("previoustrack", async () => {
      if (!this.queue) return;
      if (!(await this.isLeader())) return;
      await this.queue.unshift();
    });

    navigator.mediaSession.setActionHandler("nexttrack", async () => {
      if (!this.queue) return;
      if (!(await this.isLeader())) return;
      await this.queue.shift();
    });

    navigator.mediaSession.setActionHandler("seekto", async (details) => {
      if (!this.audio || !this.queue) return;
      if (!(await this.isLeader())) return;
      const now = this.queue.now();
      if (!now || details.seekTime == null) return;
      const state = this.audio.state(now.id);
      const duration = state?.duration();
      if (!duration || duration === 0) return;
      this.audio.seek({
        audioId: now.id,
        percentage: details.seekTime / duration,
      });
    });
  }
}

export default MediaSessionOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = MediaSessionOrchestrator;
export const NAME = "do-media-session";

customElements.define(NAME, MediaSessionOrchestrator);
