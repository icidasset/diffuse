import {
  BroadcastableDiffuseElement,
  defineElement,
  query,
} from "~/common/element.js";
import { data, mergeById } from "~/common/output.js";

/**
 * @import {OutputElement} from "@specs/components/output/types.d.ts"
 * @import AudioEngine from "~/components/engine/audio/element.js"
 * @import {MetadataElement} from "@specs/components/metadata/types.d.ts"
 * @import {Track} from "~/definitions/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Fetches spectrogram data for a track when it starts playing, if it doesn't
 * already have spectral stats.
 *
 * Monitors the audio engine for the active (non-preload) track. When playback
 * begins, it checks whether the track is missing spectrogram-derived stats and,
 * if so, asks the spectrogram metadata element to analyse it, then persists the
 * patched track back to the output.
 *
 * Only the leader tab performs the analysis (broadcasting), so multi-tab setups
 * don't duplicate the (expensive) decoding work.
 */
class SpectrogramAudioOrchestrator extends BroadcastableDiffuseElement {
  static NAME = "diffuse/orchestrator/spectrogram-audio";

  // LIFECYCLE

  /** @override */
  async connectedCallback() {
    if (this.hasAttribute("group")) {
      this.broadcast(this.identifier, {});
    }

    super.connectedCallback();

    /** @type {AudioEngine} */
    this.audio = query(this, "audio-engine-selector");

    /** @type {MetadataElement} */
    this.spectrogram = query(this, "spectrogram-selector");

    /** @type {OutputElement} */
    this.output = query(this, "output-selector");

    await customElements.whenDefined(this.audio.localName);
    await customElements.whenDefined(this.spectrogram.localName);
    await customElements.whenDefined(this.output.localName);

    this.effect(() => this.#monitorAudio());
  }

  // TRACK STATE
  // Resets whenever the active (non-preload) audio item changes.

  /** @type {string | null} */
  #trackId = null;

  /** Whether analysis has already been kicked off for the current track. */
  #analysed = false;

  /**
   * Track ids with an in-flight analysis, so a rapidly toggling play/pause or
   * a track change doesn't trigger duplicate concurrent analyses.
   *
   * @type {Set<string>}
   */
  #inFlight = new Set();

  // EFFECT

  /**
   * Reacts to audio item changes and playback state. When a track starts
   * playing and is missing spectral stats, triggers the spectrogram analysis.
   */
  #monitorAudio() {
    if (!this.audio) return;

    const active = this.audio.items().find((item) => !item.isPreload);
    const id = active?.id ?? null;

    // Detect track change
    if (id !== this.#trackId) {
      this.#trackId = id;
      this.#analysed = false;
    }

    if (!id || !active) return;

    const state = this.audio.state(id);
    const loadingState = state?.loadingState() ?? "loading";
    const hasError = typeof loadingState === "object" && "error" in loadingState;
    // Wait until `canplay` (loadingState "loaded") before fetching, so the
    // audio element gets first priority for bandwidth during its initial load
    // and the spectrogram's full-file fetch doesn't compete with it.
    const canPlay = !hasError && loadingState === "loaded";

    if (!canPlay) return;
    if (this.#analysed) return;

    this.#analysed = true;
    this.#analyse(id, active.track);
  }

  // ANALYSIS

  /**
   * @param {string} id
   * @param {Track} track
   */
  async #analyse(id, track) {
    // Already has spectral stats — nothing to do.
    if (hasSpectralStats(track.stats)) return;
    // Skip streams and very long tracks, matching the spectrogram worker's
    // own guardrails. Checking here avoids queueing work the worker would drop.
    if (track.kind === "stream") return;
    if (track.stats?.duration && track.stats.duration > MAX_DURATION_MS) return;
    // A previous play is still analysing this same track.
    if (this.#inFlight.has(id)) return;

    if (!(await this.isLeader())) return;
    if (this.#trackId !== id) return; // track changed while we awaited

    this.#inFlight.add(id);

    try {
      const patched = await this.spectrogram?.patch(track);
      if (!patched || patched === track) return;
      if (this.#trackId !== id) return; // track changed during analysis

      await this.#save(patched);
    } catch (err) {
      console.warn("spectrogram-audio: analysis failed", err);
    } finally {
      this.#inFlight.delete(id);
    }
  }

  /**
   * Merges the patched track back into the output's track list and persists it.
   *
   * @param {Track} track
   */
  async #save(track) {
    if (!this.output) return;

    const existing = await data(this.output.tracks);
    const merged = mergeById(existing, [track]);
    await this.output.tracks.save(merged);
  }
}

////////////////////////////////////////////
// STATS HELPERS
////////////////////////////////////////////

const SPECTRAL_KEYS = [
  "spectralCentroid",
  "spectralRolloff",
  "spectralSpread",
  "spectralFlatness",
  "spectralFlux",
];

/**
 * @param {import("~/definitions/types.d.ts").TrackStats | undefined} stats
 * @returns {boolean}
 */
function hasSpectralStats(stats) {
  if (!stats) return false;
  return SPECTRAL_KEYS.every((k) =>
    stats[/** @type {keyof import("~/definitions/types.d.ts").TrackStats} */ (k)] != null
  );
}

// Keep this in sync with the spectrogram worker's `MAX_DURATION_MS`.
const MAX_DURATION_MS = 4 * 60 * 60 * 1000; // 4 hours

export default SpectrogramAudioOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = SpectrogramAudioOrchestrator;
export const NAME = "do-spectrogram-audio";

defineElement(NAME, SpectrogramAudioOrchestrator);
