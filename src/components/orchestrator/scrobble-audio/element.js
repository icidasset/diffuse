import { BroadcastableDiffuseElement, query } from "~/common/element.js";

/**
 * @import {ScrobbleElement} from "~/components/supplement/types.d.ts"
 * @import {Track} from "~/definitions/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Connects the audio engine with the scrobble configurator.
 *
 * Calls `nowPlaying` when a track starts and `scrobble` once the user
 * has listened long enough per the last.fm rules:
 *   - Track must be at least 30 seconds long.
 *   - User must have listened to at least min(duration / 2, 4 minutes).
 */
class ScrobbleAudioOrchestrator extends BroadcastableDiffuseElement {
  static NAME = "diffuse/orchestrator/scrobble-audio";

  // LIFECYCLE

  /** @override */
  async connectedCallback() {
    if (this.hasAttribute("group")) {
      this.broadcast(this.identifier, {});
    }

    super.connectedCallback();

    /** @type {import("~/components/engine/audio/element.js").CLASS} */
    this.audio = query(this, "audio-engine-selector");

    /** @type {ScrobbleElement} */
    this.scrobble = query(this, "scrobble-selector");

    await customElements.whenDefined(this.audio.localName);
    await customElements.whenDefined(this.scrobble.localName);

    this.effect(() => this.#monitorAudio());
  }

  /** @override */
  disconnectedCallback() {
    super.disconnectedCallback();
    this.#stopTimer();
  }

  // TRACK STATE
  // Resets whenever the active (non-preload) audio item changes.

  /** @type {string | null} */
  #trackId = null;

  /** @type {Track | null} */
  #activeTrack = null;

  /** @type {number | null} Date.now() when track first started, used as the scrobble timestamp. */
  #startedAt = null;

  /** Whether `nowPlaying` has been called for the current track. */
  #nowPlayingSent = false;

  /** Whether `scrobble` has been called for the current track. */
  #scrobbled = false;

  // TIMER STATE
  // Accumulates actual listening time (pauses don't count).

  /** Accumulated listening time in ms before the last pause. */
  #listenedMs = 0;

  /** Date.now() when the timer was last resumed; null when paused. */
  #timerResumedAt = /** @type {number | null} */ (null);

  /** @type {number | null} */
  #intervalId = null;

  // EFFECT

  /**
   * Reacts to audio item changes and playback state.
   * Detects track changes, resets state, and starts/stops the listening timer.
   */
  #monitorAudio() {
    if (!this.audio) return;

    const active = this.audio.items().find((item) => !item.isPreload);
    const id = active?.id ?? null;

    // Detect track change
    if (id !== this.#trackId) {
      this.#stopTimer();

      this.#trackId = id;
      this.#activeTrack = active?.track ?? null;
      this.#startedAt = id ? Date.now() : null;
      this.#nowPlayingSent = false;
      this.#scrobbled = false;
      this.#listenedMs = 0;
    }

    if (!id) return;

    const isPlaying = this.audio.state(id)?.isPlaying() ?? false;

    if (isPlaying) {
      this.#startTimer();

      if (!this.#nowPlayingSent) {
        this.#nowPlayingSent = true;
        this.#sendNowPlaying(id);
      }
    } else {
      this.#stopTimer();
    }
  }

  // TIMER

  #startTimer() {
    if (this.#timerResumedAt !== null) return;

    this.#timerResumedAt = Date.now();
    this.#intervalId = setInterval(() => this.#checkScrobble(), 1_000);
  }

  #stopTimer() {
    if (this.#timerResumedAt !== null) {
      this.#listenedMs += Date.now() - this.#timerResumedAt;
      this.#timerResumedAt = null;
    }

    if (this.#intervalId !== null) {
      clearInterval(this.#intervalId);
      this.#intervalId = null;
    }
  }

  #totalListenedMs() {
    return this.#listenedMs +
      (this.#timerResumedAt !== null ? Date.now() - this.#timerResumedAt : 0);
  }

  // SCROBBLING

  /**
   * @param {string} id
   */
  async #sendNowPlaying(id) {
    if (!(await this.isLeader())) return;
    if (this.#trackId !== id || !this.#activeTrack) return;

    try {
      await this.scrobble?.nowPlaying(this.#activeTrack);
    } catch (err) {
      console.warn("scrobble: nowPlaying failed", err);
    }
  }

  async #checkScrobble() {
    if (this.#scrobbled) return;

    const id = this.#trackId;
    if (!id || !this.#startedAt || !this.#activeTrack) return;

    const durationSec = this.audio?.state(id)?.duration() ?? 0;

    // last.fm: track must be at least 30 seconds
    if (durationSec < 30) return;

    // last.fm: must have listened to min(half the track, 4 minutes)
    const listenedSec = this.#totalListenedMs() / 1000;
    if (listenedSec < Math.min(durationSec / 2, 240)) return;

    this.#scrobbled = true;

    if (!(await this.isLeader())) return;
    if (this.#trackId !== id) return;

    const track = this.#activeTrack;
    const startedAt = this.#startedAt;

    try {
      await this.scrobble?.scrobble(track, startedAt);
    } catch (err) {
      console.warn("scrobble: scrobble failed", err);
      this.#scrobbled = false;
    }
  }
}

export default ScrobbleAudioOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ScrobbleAudioOrchestrator;
export const NAME = "do-scrobble-audio";

customElements.define(NAME, ScrobbleAudioOrchestrator);
