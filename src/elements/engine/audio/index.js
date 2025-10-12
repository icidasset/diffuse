import DiffuseElement from "@common/element.js";
import { effect, signal } from "@common/signal.js";

/**
 * @import {Audio, AudioState, State} from "./types.d.ts"
 * @import {RenderArg} from "@common/element.d.ts"
 * @import {Signal} from "@common/signal.d.ts"
 */

////////////////////////////////////////////
// CONSTANTS
////////////////////////////////////////////
const SILENT_MP3 =
  "data:audio/mp3;base64,SUQzBAAAAAAAI1RTU0UAAAAPAAADTGF2ZjU2LjM2LjEwMAAAAAAAAAAAAAAA//OEAAAAAAAAAAAAAAAAAAAAAAAASW5mbwAAAA8AAAAEAAABIADAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDV1dXV1dXV1dXV1dXV1dXV1dXV1dXV1dXV6urq6urq6urq6urq6urq6urq6urq6urq6v////////////////////////////////8AAAAATGF2YzU2LjQxAAAAAAAAAAAAAAAAJAAAAAAAAAAAASDs90hvAAAAAAAAAAAAAAAAAAAA//MUZAAAAAGkAAAAAAAAA0gAAAAATEFN//MUZAMAAAGkAAAAAAAAA0gAAAAARTMu//MUZAYAAAGkAAAAAAAAA0gAAAAAOTku//MUZAkAAAGkAAAAAAAAA0gAAAAANVVV";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class AudioEngine extends DiffuseElement {
  static observedAttributes = ["is-playing", "volume"];

  constructor() {
    super();

    // TODO: Get volume from previous session if possible
    // const VOLUME_KEY = `@elements/engine/audio/${this.groupId}/volume`;
    // const vol = localStorage.getItem(VOLUME_KEY);
  }

  // SIGNALS

  defaultVolume = signal(0.5);
  isPlaying = signal(false);
  items = signal(/** @type {Audio[]} */ ([]));

  // STATE

  get state() {
    return {
      isPlaying: this.isPlaying,
      items: this.items,
      volume: { default: this.defaultVolume() },
    };
  }

  // ACTIONS

  /**
   * @param {{ audioId: string }} _
   */
  pause({ audioId }) {
    this.withAudioNode(audioId, (audio) => audio.pause());
  }

  /**
   * @param {{ audioId: string; volume?: number }} _
   */
  play({ audioId, volume }) {
    this.withAudioNode(audioId, (audio, item) => {
      audio.volume = volume ?? this.state.volume.default;
      audio.muted = false;

      if (audio.readyState === 0) audio.load();
      if (!audio.isConnected) return;

      const promise = audio.play() || Promise.resolve();
      item.state = { isPlaying: true };

      promise.catch((e) => {
        if (!audio.isConnected) {
          return; /* The node was removed from the DOM, we can ignore this error */
        }
        const err =
          "Couldn't play audio automatically. Please resume playback manually.";
        console.error(err, e);
        item.state = { isPlaying: false };
      });
    });
  }

  /**
   * @param {{ audioId: string; play: boolean; progress?: number }} args
   */
  reload(args) {
    this.withAudioNode(args.audioId, (audio, item) => {
      if (audio.readyState === 0 || audio.error?.code === 2) {
        audio.load();

        if (args.progress !== undefined) {
          item.setAttribute(
            "initial-progress",
            JSON.stringify(args.progress),
          );
        }

        if (args.play) {
          this.play({ audioId: args.audioId, volume: audio.volume });
        }
      }
    });
  }

  /**
   * @param {{ audioId: string; percentage: number }} _
   */
  seek({ audioId, percentage }) {
    this.withAudioNode(audioId, (audio) => {
      if (!isNaN(audio.duration)) {
        audio.currentTime = audio.duration * percentage;
      }
    });
  }

  /**
   * @param {{ audioId?: string; volume: number }} args
   */
  volume(args) {
    // TODO:
    // if (!args.audioId) update({ volume: { default: args.volume } });

    Array.from(this.querySelectorAll("de-audio-item audio")).forEach((node) => {
      const audio = /** @type {HTMLAudioElement} */ (node);
      if (audio.hasAttribute("preload")) return;
      if (args.audioId === undefined || args.audioId === audio.id) {
        audio.volume = args.volume;
      }
    });
  }

  /**
   * @param {{ audio: Audio[]; play?: { audioId: string; volume?: number } }} args
   */
  yield(args) {
    this.items(args.audio);
    if (args.play) this.play(args.play);
  }

  // RENDER

  /**
   * @param {RenderArg<State>} _
   */
  render({ html, state }) {
    console.log("Render");

    const nodes = state.items().map((audio) => {
      const ip = audio.progress === undefined
        ? "0"
        : JSON.stringify(audio.progress);

      return html`
        <de-audio-item
          id="${audio.id}"
          initial-progress="${ip}"
          url="${audio.url}"
          ${audio.isPreload ? "preload" : ""}
          ${audio.mimeType ? 'mime-type="' + audio.mimeType + '"' : ""}
        >
          <audio
            crossorigin="anonymous"
            muted="true"
            preload="auto"
          >
            <source
              src="${audio.url}"
              ${audio.mimeType ? 'type="' + audio.mimeType + '"' : ""}
            />
          </audio>
        </de-audio-item>
      `;
    });

    return html`
      <section id="audio-nodes">
        ${nodes.join("")}
      </section>
    `;
  }

  // 🛠️

  /**
   * @param {string} audioId
   * @param {(audio: HTMLAudioElement, item: AudioEngineItem) => void} fn
   */
  withAudioNode(audioId, fn) {
    const node = this.querySelector(
      `de-audio-item[id="${audioId}"]:not([preload])`,
    );

    if (node) {
      const item = /** @type {AudioEngineItem} */ (node);
      fn(item.audio, item);
    }
  }
}

export default AudioEngine;

////////////////////////////////////////////
// ITEM ELEMENT
////////////////////////////////////////////

export class AudioEngineItem extends HTMLElement {
  /**
   * @type {AudioState}
   */
  #state;

  constructor() {
    super();

    const ip = this.getAttribute("initial-progress");

    this.#state = {
      duration: 0,
      hasEnded: false,
      id: this.id,
      isPlaying: true,
      isPreload: this.hasAttribute("preload"),
      loadingState: "loading",
      mimeType: this.getAttribute("mime-type") ?? undefined,
      progress: ip ? parseFloat(ip) : 0,
      url: this.getAttribute("url") ?? "",
    };

    const audio = this.audio;

    audio.addEventListener("canplay", this.canplayEvent);
    audio.addEventListener("durationchange", this.durationchangeEvent);
    audio.addEventListener("ended", this.endedEvent);
    audio.addEventListener("error", this.errorEvent);
    audio.addEventListener("pause", this.pauseEvent);
    audio.addEventListener("play", this.playEvent);
    audio.addEventListener("suspend", this.suspendEvent);
    audio.addEventListener("timeupdate", this.timeupdateEvent);
    audio.addEventListener("waiting", this.waitingEvent);
  }

  // RELATED ELEMENTS

  get audio() {
    const el = this.querySelector("audio");
    if (el) return /** @type {HTMLAudioElement} */ (el);
    else throw new Error("Cannot find child audio element");
  }

  get engine() {
    const el = this.closest("de-audio");
    if (el) return /** @type {AudioEngine} */ (el);
    else throw new Error("Cannot find parent de-audio element");
  }

  // STATE

  get state() {
    return { ...this.#state };
  }

  /**
   * @param {Partial<AudioState>} s
   */
  set state(s) {
    this.#state = { ...this.#state, ...s };
  }

  // EVENTS

  /**
   * @param {Event} event
   */
  canplayEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);
    const item = engineItem(audio);

    if (
      item.hasAttribute("initial-progress") &&
      audio.duration &&
      !isNaN(audio.duration)
    ) {
      const progress = JSON.parse(
        item.getAttribute("initial-progress") ?? "0",
      );
      audio.currentTime = audio.duration * progress;
      item.removeAttribute("initial-progress");
    }

    finishedLoading(event);
  }

  /**
   * @param {Event} event
   */
  durationchangeEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);

    if (!isNaN(audio.duration)) {
      engineItem(audio).state = { duration: audio.duration };
    }
  }

  /**
   * @param {Event} event
   */
  endedEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);
    audio.currentTime = 0;

    engineItem(audio).state = { hasEnded: true };
  }

  /**
   * @param {Event} event
   */
  errorEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);
    const code = audio.error?.code || 0;

    engineItem(audio).state = { loadingState: { error: { code } } };
  }

  /**
   * @param {Event} event
   */
  pauseEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);

    const item = engineItem(audio).state;
    const ended = item ? item.hasEnded || item.progress === 1 : false;

    engineItem(audio).state = { isPlaying: false };
    engineItem(audio).engine.isPlaying(ended);
  }

  /**
   * @param {Event} event
   */
  playEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);

    engineItem(audio).state = { isPlaying: true };
    engineItem(audio).engine.isPlaying(true);

    // In case audio was preloaded:
    if (audio.readyState === 4) finishedLoading(event);
  }

  /**
   * @param {Event} event
   */
  suspendEvent(event) {
    finishedLoading(event);
  }

  /**
   * @param {Event} event
   */
  timeupdateEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);

    engineItem(audio).state = {
      progress: isNaN(audio.duration) || audio.duration === 0
        ? 0
        : audio.currentTime / audio.duration,
    };
  }

  /**
   * @param {Event} event
   */
  waitingEvent(event) {
    initiateLoading(event);
  }
}

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

/**
 * @param {HTMLAudioElement} audio
 */
function engineItem(audio) {
  const c = audio.closest("de-audio-item");
  if (c) return /** @type {AudioEngineItem} */ (c);
  else throw new Error("Cannot find parent de-audio-item element");
}

/**
 * @param {Event} event
 */
function finishedLoading(event) {
  const audio = /** @type {HTMLAudioElement} */ (event.target);
  engineItem(audio).state = { loadingState: "loaded" };
}

/**
 * @param {Event} event
 */
function initiateLoading(event) {
  const audio = /** @type {HTMLAudioElement} */ (event.target);
  if (audio.readyState < 4) {
    engineItem(audio).state = { loadingState: "loading" };
  }
}

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

customElements.define("de-audio", AudioEngine);
customElements.define("de-audio-item", AudioEngineItem);
