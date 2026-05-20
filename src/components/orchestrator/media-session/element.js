import {
  BroadcastableDiffuseElement,
  defineElement,
  query,
  queryOptional,
} from "~/common/element.js";

/**
 * @import {OutputElement} from "~/components/output/types.d.ts"
 * @import ArtworkOrchestrator from "~/components/orchestrator/artwork/element.js"
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
    if (this.hasAttribute("group")) {
      this.broadcast(this.identifier, {});
    }

    super.connectedCallback();

    if (!("mediaSession" in navigator)) return;

    this.audio = query(this, "audio-engine-selector");
    this.queue = query(this, "queue-engine-selector");
    this.output = queryOptional(this, "output-selector");
    this.artwork = queryOptional(this, "artwork-selector");

    await Promise.all([
      customElements.whenDefined(this.audio.localName),
      customElements.whenDefined(this.queue.localName),
      this.output && customElements.whenDefined(this.output.localName),
      this.artwork && customElements.whenDefined(this.artwork.localName),
    ].filter(Boolean));

    this.#registerActionHandlers();
    this.effect(() => this.#syncMetadata());
    this.effect(() => this.#syncPlaybackState());
    this.effect(() => this.#syncPositionState());
  }

  // 🛠️

  async #syncMetadata() {
    if (!this.queue) return;

    const now = this.queue.now();
    const tracksCol = this.output?.tracks.collection();
    const track = now && tracksCol?.state === "loaded"
      ? tracksCol.data.find((t) => t.id === now.id)
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
      const artworkOrchestrator = this.artwork;

      /** @type {Uint8Array | null} */
      let bytes = null;

      try {
        bytes = await artworkOrchestrator.get(track);
      } catch {
        bytes = null;
      }

      if (bytes && navigator.mediaSession.metadata) {
        const mime = detectMime(bytes);
        const blob = new Blob([/** @type {ArrayBuffer} */ (bytes.buffer)], {
          type: mime,
        });

        const url = URL.createObjectURL(blob);
        const nowLater = this.queue.now();

        // If in the meantime the now-playing track has changed,
        // don't set the artwork.
        if (nowLater?.id !== now?.id) return;

        navigator.mediaSession.metadata = new MediaMetadata({
          title: tags.title ?? "",
          artist: tags.artist ?? tags.albumartist ?? "",
          album: tags.album ?? "",
          artwork: [{ src: url, type: mime }],
        });
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
// 🛠️
////////////////////////////////////////////

/**
 * @param {Uint8Array} bytes
 * @returns {string}
 */
function detectMime(bytes) {
  if (bytes[0] === 0xFF && bytes[1] === 0xD8) return "image/jpeg";
  if (bytes[0] === 0x89 && bytes[1] === 0x50) return "image/png";
  if (bytes[0] === 0x47 && bytes[1] === 0x49) return "image/gif";
  if (bytes[0] === 0x52 && bytes[1] === 0x49) return "image/webp";
  return "image/jpeg";
}

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = MediaSessionOrchestrator;
export const NAME = "do-media-session";

defineElement(NAME, MediaSessionOrchestrator);
