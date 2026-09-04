import {
  BroadcastableDiffuseElement,
  defineElement,
  query,
  queryOptional,
} from "~/common/element.js";
import { signal } from "~/common/signal.js";

/**
 * @import {OutputElement} from "@specs/components/output/types.d.ts"
 * @import ArtworkOrchestrator from "~/components/orchestrator/artwork/element.js"
 * @import {default as AudioEngine} from "~/components/engine/audio/element.js"
 * @import {default as QueueEngine} from "~/components/engine/queue/element.js"
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

  // SIGNALS

  /** @type {string | null} */
  #artworkUrl = null;

  #isLeader = signal(true);

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

    /** @type {AudioEngine} */
    this.audio = query(this, "audio-engine-selector");
    /** @type {QueueEngine} */
    this.queue = query(this, "queue-engine-selector");
    /** @type {OutputElement | null} */
    this.output = queryOptional(this, "output-selector");
    /** @type {ArtworkOrchestrator | null} */
    this.artwork = queryOptional(this, "artwork-selector");

    await Promise.all([
      customElements.whenDefined(this.audio.localName),
      customElements.whenDefined(this.queue.localName),
      this.output && customElements.whenDefined(this.output.localName),
      this.artwork && customElements.whenDefined(this.artwork.localName),
    ].filter(Boolean));

    this.effect(() => {
      const promise = this.isLeader();
      promise?.then((b) => this.#isLeader.set(b));
    });

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
      if (this.#artworkUrl) {
        URL.revokeObjectURL(this.#artworkUrl);
        this.#artworkUrl = null;
      }
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
        if (nowLater?.id !== now?.id) {
          URL.revokeObjectURL(url);
          return;
        }

        if (this.#artworkUrl) URL.revokeObjectURL(this.#artworkUrl);
        this.#artworkUrl = url;

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
    if (!this.audio || !this.queue) return;

    // Reflect the real state: "none" when nothing is loaded, so the OS media
    // controls don't linger with stale "paused" UI. Firefox keeps the media
    // session surface alive as long as playbackState is "playing"/"paused",
    // and its `setPositionState` throws while the state is "none" — so
    // keeping this in sync matters on Firefox in particular.
    const playing = this.audio.isPlaying();

    navigator.mediaSession.playbackState = this.queue.now()
      ? playing ? "playing" : "paused"
      : "none";
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
    if (navigator.mediaSession.playbackState === "none") return;
    if (typeof navigator.mediaSession.setPositionState !== "function") return;

    try {
      navigator.mediaSession.setPositionState({
        duration,
        position: Math.min(duration * progress, duration),
        playbackRate: 1,
      });
    } catch {
      // setPositionState may throw (e.g. on Firefox) when the position is
      // not finite or the browser doesn't consider the session playable.
    }
  }

  #registerActionHandlers() {
    // Register each handler defensively: `setActionHandler` throws for
    // actions a browser doesn't support (Firefox is stricter than Chrome), so
    // an unsupported action must never abort registration of the rest. An
    // exception here would also escape `connectedCallback` and prevent the
    // metadata / playback-state / position-state effects below from running,
    // leaving the media session completely inert.
    /** @type {Array<[MediaSessionAction, MediaSessionActionHandler]>} */
    const handlers = [
      ["play", () => {
        if (!this.audio || !this.queue) return;
        if (!this.#isLeader.value) return;
        const now = this.queue.now();
        if (now) this.audio.play({ audioId: now.id });
      }],
      ["pause", () => {
        if (!this.audio || !this.queue) return;
        if (!this.#isLeader.value) return;
        const now = this.queue.now();
        if (now) this.audio.pause({ audioId: now.id });
      }],
      ["previoustrack", () => {
        if (!this.queue) return;
        if (!this.#isLeader.value) return;
        this.queue.unshift();
      }],
      ["nexttrack", () => {
        if (!this.queue) return;
        if (!this.#isLeader.value) return;
        this.queue.shift();
      }],
      ["seekto", (details) => {
        if (!this.audio || !this.queue) return;
        if (!this.#isLeader.value) return;
        const now = this.queue.now();
        if (!now || details.seekTime == null) return;
        const state = this.audio.state(now.id);
        const duration = state?.duration();
        if (!duration || duration === 0) return;
        this.audio.seek({
          audioId: now.id,
          percentage: details.seekTime / duration,
        });
      }],
    ];

    for (const [action, handler] of handlers) {
      try {
        navigator.mediaSession.setActionHandler(action, handler);
      } catch {
        // Action not supported on this browser/platform — skip it.
      }
    }
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
