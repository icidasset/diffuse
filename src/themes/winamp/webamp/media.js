/**
 * @import AudioEngine from "~/components/engine/audio/element.js"
 */

////////////////////////////////////////////
// REFS
////////////////////////////////////////////

/** @type {AudioEngine | null} */
let audioEngineRef = null;

/**
 * Resolves the Diffuse track ID for whatever track webamp is currently
 * buffering. Provided by the facet so it can close over `amp.store` and
 * `webampIndexToTrackId` without those references living inside this module.
 *
 * @type {(() => string | undefined) | null}
 */
let currentTrackIdResolverRef = null;

/**
 * @param {AudioEngine} audio
 */
export function setAudioEngine(audio) {
  audioEngineRef = audio;
}

/**
 * @param {() => string | undefined} resolver
 */
export function setCurrentTrackIdResolver(resolver) {
  currentTrackIdResolverRef = resolver;
}

////////////////////////////////////////////
// EMITTER
////////////////////////////////////////////

class Emitter {
  constructor() {
    /** @type {Record<string, Array<(...args: any[]) => void>>} */
    this._listeners = {};
  }

  /**
   * @param {string} event
   * @param {(...args: any[]) => void} callback
   */
  on(event, callback) {
    if (!this._listeners[event]) this._listeners[event] = [];
    this._listeners[event].push(callback);

    return () => {
      this._listeners[event] = this._listeners[event].filter((cb) =>
        cb !== callback
      );
    };
  }

  /**
   * @param {string} event
   * @param {...any} args
   */
  trigger(event, ...args) {
    (this._listeners[event] ?? []).forEach((cb) => cb(...args));
  }

  dispose() {
    this._listeners = {};
  }
}

////////////////////////////////////////////
// MEDIA
////////////////////////////////////////////

/**
 * IMedia implementation backed by Diffuse's AudioEngine.
 *
 * URI resolution is handled by the Diffuse input component.
 * Playback state is polled from the AudioEngine's de-audio-item element so
 * that webamp's UI stays in sync without a separate audio pipeline.
 */
class DiffuseMedia {
  constructor() {
    this._emitter = new Emitter();

    /** @type {string | null} */
    this._currentId = null;

    this._disposed = false;

    // Previous-state cache for edge detection in the poll loop
    this._prevTime = -1;
    this._prevPlaying = false;
    this._prevEnded = false;
    this._prevLoadingState = /** @type {any} */ (null);

    // Seek target held until the signal catches up, so timeElapsed() and the
    // poll loop both return the correct position during the stale window.
    /** @type {number | null} */
    this._seekTarget = null;

    // Set when loadFromUrl is called with autoPlay=true; cleared once
    // the audio item reports "loaded" and we issue the play command.
    this._autoPlayPending = false;

    // AudioContext + EQ chain + analyser for webamp's spectrum visualizer.
    // Chain: source → preamp → eq[0..9] → analyser → destination
    this._context = new AudioContext();

    // Preamp gain node (linear gain, default 1.0 = 0 dB)
    this._preampNode = this._context.createGain();

    // 10-band peaking EQ at standard Winamp frequencies
    // webamp passes the frequency itself as the `band` argument to setEqBand
    this._eqFreqs = [60, 170, 310, 600, 1000, 3000, 6000, 12000, 14000, 16000];
    this._eqNodes = this._eqFreqs.map((freq) => {
      const f = this._context.createBiquadFilter();
      f.type = "peaking";
      f.frequency.value = freq;
      f.Q.value = 1.0;
      f.gain.value = 0;
      return f;
    });
    this._eqEnabled = true;
    /** @type {number[]} stored band values in webamp's 0-100 scale (50 = flat) */
    this._eqValues = new Array(10).fill(50);
    this._preampValue = 50;

    this._analyser = this._context.createAnalyser();

    // Wire the chain
    this._preampNode.connect(this._eqNodes[0]);
    for (let i = 0; i < this._eqNodes.length - 1; i++) {
      this._eqNodes[i].connect(this._eqNodes[i + 1]);
    }
    this._eqNodes[this._eqNodes.length - 1].connect(this._analyser);
    this._analyser.connect(this._context.destination);

    /** @type {WeakSet<HTMLAudioElement>} */
    this._connectedAudio = new WeakSet();

    this._poll();
  }

  // HELPERS

  /** @returns {any | null} */
  _item() {
    const id = this._currentId;
    if (!id || !audioEngineRef) return null;
    return audioEngineRef.querySelector(
      `de-audio-item[id="${id}"]:not([preload])`,
    );
  }

  // POLL LOOP

  _poll() {
    if (this._disposed) return;

    const id = this._currentId;

    if (id && audioEngineRef) {
      const item = /** @type {any} */ (
        audioEngineRef.querySelector(`de-audio-item[id="${id}"]:not([preload])`)
      );

      if (item) {
        const state = item.state;

        const rawTime = state.currentTime();

        // Clear seek target once the signal has caught up
        if (this._seekTarget !== null && Math.abs(rawTime - this._seekTarget) < 0.5) {
          this._seekTarget = null;
        }

        const currentTime = this._seekTarget ?? rawTime;
        const isPlaying = state.isPlaying();
        const hasEnded = state.hasEnded();
        const loadingState = state.loadingState();

        if (currentTime !== this._prevTime) {
          this._prevTime = currentTime;
          this._emitter.trigger("timeupdate");
        }

        if (isPlaying && !this._prevPlaying) {
          this._emitter.trigger("playing");
        }
        this._prevPlaying = isPlaying;

        if (hasEnded && !this._prevEnded) {
          this._emitter.trigger("ended");
        }
        this._prevEnded = hasEnded;

        if (loadingState !== this._prevLoadingState) {
          if (loadingState === "loaded") {
            this._emitter.trigger("stopWaiting");
            this._emitter.trigger("fileLoaded");
            if (this._autoPlayPending) {
              this._autoPlayPending = false;
              if (id && audioEngineRef) audioEngineRef.play({ audioId: id });
            }
          } else if (loadingState === "loading") {
            this._emitter.trigger("waiting");
          }
          this._prevLoadingState = loadingState;
        }

        // Tap the <audio> element into our AudioContext once per track.
        // Direct element access is required here for the Web Audio API.
        const audioEl = /** @type {HTMLAudioElement | null} */ (
          item.querySelector("audio")
        );

        if (audioEl && !this._connectedAudio.has(audioEl)) {
          try {
            this._context.createMediaElementSource(audioEl).connect(
              this._preampNode,
            );
            this._connectedAudio.add(audioEl);
          } catch {
            // Already owned by another context, or CORS restriction — ignore
          }
        }
      }
    }

    requestAnimationFrame(() => this._poll());
  }

  // IMedia

  /**
   * @param {number} volume 0–100
   */
  setVolume(volume) {
    audioEngineRef?.adjustVolume({ volume: volume / 100 });
  }

  /** @param {number} _balance */
  setBalance(_balance) {}

  /**
   * @param {string} event
   * @param {(...args: any[]) => void} callback
   */
  on(event, callback) {
    return this._emitter.on(event, callback);
  }

  timeElapsed() {
    if (this._seekTarget !== null) return this._seekTarget;
    return this._item()?.state.currentTime() ?? 0;
  }

  duration() {
    const d = this._item()?.state.duration();
    return !d || isNaN(d) ? 0 : d;
  }

  async play() {
    const id = this._currentId;
    if (!id || !audioEngineRef) return;
    if (this._context.state === "suspended") await this._context.resume();
    audioEngineRef.play({ audioId: id });
  }

  pause() {
    const id = this._currentId;
    if (id && audioEngineRef) audioEngineRef.pause({ audioId: id });
  }

  stop() {
    const id = this._currentId;
    if (!id || !audioEngineRef) return;
    audioEngineRef.pause({ audioId: id });
    audioEngineRef.seek({ audioId: id, currentTime: 0 });
  }

  /**
   * @param {number} percent 0–100
   */
  seekToPercentComplete(percent) {
    const id = this._currentId;
    if (id && audioEngineRef) {
      audioEngineRef.seek({ audioId: id, percentage: percent / 100 });
      const dur = this._item()?.state.duration() ?? 0;
      this._seekTarget = dur * (percent / 100);
    }
  }

  /**
   * @param {string} _uri
   * @param {boolean} autoPlay
   */
  async loadFromUrl(_uri, autoPlay) {
    // Audio node creation and playback are handled by the queueAudio
    // orchestrator. This method only needs to point the poll loop at the
    // correct Diffuse track and record whether autoPlay was requested so
    // we can trigger play once the audio item becomes ready.
    const id = currentTrackIdResolverRef?.() ?? null;
    if (!id) return;

    this._currentId = id;
    this._autoPlayPending = autoPlay;

    // Reset poll state for the new track
    this._prevTime = -1;
    this._prevPlaying = false;
    this._prevEnded = false;
    this._prevLoadingState = null;
    this._seekTarget = null;

    this._emitter.trigger("waiting");
    if (this._context.state === "suspended") await this._context.resume();
  }

  /** @param {number} _value */
  setPreamp(_value) {
    this._preampValue = _value;
    if (this._eqEnabled) {
      const dB = (_value - 50) / 50 * 12;
      this._preampNode.gain.value = Math.pow(10, dB / 20);
    }
  }

  /**
   * @param {number} band frequency in Hz (60 | 170 | 310 | 600 | 1000 | 3000 | 6000 | 12000 | 14000 | 16000)
   * @param {number} value 0–100 (50 = flat, 0 = −12 dB, 100 = +12 dB)
   */
  setEqBand(band, value) {
    const idx = this._eqFreqs.indexOf(band);
    if (idx === -1) return;
    this._eqValues[idx] = value;
    if (this._eqEnabled) {
      this._eqNodes[idx].gain.value = (value - 50) / 50 * 12;
    }
  }

  enableEq() {
    this._eqEnabled = true;
    this._eqValues.forEach((value, i) => {
      this._eqNodes[i].gain.value = (value - 50) / 50 * 12;
    });
    const dB = (this._preampValue - 50) / 50 * 12;
    this._preampNode.gain.value = Math.pow(10, dB / 20);
  }

  disableEq() {
    this._eqEnabled = false;
    this._eqNodes.forEach((node) => { node.gain.value = 0; });
    this._preampNode.gain.value = 1;
  }

  getAnalyser() {
    return this._analyser;
  }

  dispose() {
    this._disposed = true;
    this._emitter.dispose();
    this._preampNode.disconnect();
    this._eqNodes.forEach((n) => n.disconnect());
    this._context.close();
  }
}

export default DiffuseMedia;
