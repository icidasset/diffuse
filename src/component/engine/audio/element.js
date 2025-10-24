import { BroadcastableDiffuseElement } from "@common/element.js";
import { signal } from "@common/signal.js";

/**
 * @import {Actions, Audio, AudioState, State} from "./types.d.ts"
 * @import {RenderArg} from "@common/element.d.ts"
 */

////////////////////////////////////////////
// CONSTANTS
////////////////////////////////////////////
const _SILENT_MP3 =
  "data:audio/mp3;base64,SUQzBAAAAAAAI1RTU0UAAAAPAAADTGF2ZjU2LjM2LjEwMAAAAAAAAAAAAAAA//OEAAAAAAAAAAAAAAAAAAAAAAAASW5mbwAAAA8AAAAEAAABIADAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDV1dXV1dXV1dXV1dXV1dXV1dXV1dXV1dXV6urq6urq6urq6urq6urq6urq6urq6urq6v////////////////////////////////8AAAAATGF2YzU2LjQxAAAAAAAAAAAAAAAAJAAAAAAAAAAAASDs90hvAAAAAAAAAAAAAAAAAAAA//MUZAAAAAGkAAAAAAAAA0gAAAAATEFN//MUZAMAAAGkAAAAAAAAA0gAAAAARTMu//MUZAYAAAGkAAAAAAAAA0gAAAAAOTku//MUZAkAAAGkAAAAAAAAA0gAAAAANVVV";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {Actions}
 */
class AudioEngine extends BroadcastableDiffuseElement {
  constructor() {
    super();

    // Group
    const group = this.getAttribute("group") || crypto.randomUUID();
    const isShared = this.hasAttribute("group");

    // Setup leader election if shared
    if (isShared) {
      const fn = this.broadcast(`diffuse/engine/audio/${group}`);

      this.pause = fn("pause", this.pause).leaderOnly;
      this.play = fn("play", this.play).leaderOnly;
      this.reload = fn("reload", this.reload).leaderOnly;
      this.seek = fn("seek", this.seek).leaderOnly;
      this.supply = fn("supply", this.supply).replicate;

      this.$isPlaying.set = fn("isPlaying", this.$isPlaying.set).replicate;
    }

    // TODO: Get volume from previous session if possible
    // const VOLUME_KEY = `@elements/engine/audio/${this.groupId}/volume`;
    // const vol = localStorage.getItem(VOLUME_KEY);
  }

  // SIGNALS

  #items = signal(/** @type {Audio[]} */ ([]));
  #volume = signal(0.5);

  $isPlaying = signal(false);

  // STATE

  isPlaying = this.$isPlaying.get;
  items = this.#items.get;
  volume = this.#volume.get;

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    // Manage playback across tabs if needed
    if (this.broadcasted) {
      this.effect(async () => {
        const status = await this.broadcastingStatus();
        if (status.leader && status.initialLeader === false) {
          // TODO:
          // console.log("🧙 Leadership acquired");
        }
      });
    }

    // Monitor volume
    // NOTE: Support different volume levels for audio elements?
    this.effect(() => {
      Array.from(this.querySelectorAll("de-audio-item audio")).forEach(
        (node) => {
          const audio = /** @type {HTMLAudioElement} */ (node);
          if (audio.hasAttribute("preload")) return;
          audio.volume = this.#volume.value;
        },
      );
    });
  }

  // ACTIONS (PRIVATE)

  /**
   * @type {Actions["pause"]}
   */
  pause({ audioId }) {
    this.withAudioNode(audioId, (audio) => audio.pause());
  }

  /**
   * @type {Actions["play"]}
   */
  play({ audioId, volume }) {
    this.withAudioNode(audioId, (audio, item) => {
      audio.volume = volume ?? this.volume();
      audio.muted = false;

      if (audio.readyState === 0) audio.load();
      if (!audio.isConnected) return;

      const promise = audio.play() || Promise.resolve();
      item.state({ isPlaying: true });

      promise.catch((e) => {
        if (!audio.isConnected) {
          return; /* The node was removed from the DOM, we can ignore this error */
        }
        const err =
          "Couldn't play audio automatically. Please resume playback manually.";
        console.error(err, e);
        item.state({ isPlaying: false });
      });
    });
  }

  /**
   * @type {Actions["reload"]}
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
   * @type {Actions["seek"]}
   */
  seek({ audioId, percentage }) {
    this.withAudioNode(audioId, (audio) => {
      if (!isNaN(audio.duration)) {
        audio.currentTime = audio.duration * percentage;
      }
    });
  }

  /**
   * @type {Actions["supply"]}
   */
  supply(args) {
    this.#items.value = args.audio;
    if (args.play) this.play(args.play);
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    const nodes = this.items().map((audio) => {
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
      if (item) fn(item.audio, item);
    }
  }
}

export default AudioEngine;

////////////////////////////////////////////
// ITEM ELEMENT
////////////////////////////////////////////

class AudioEngineItem extends HTMLElement {
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
    else return null;
  }

  // STATE

  /**
   * @param {Partial<AudioState> | undefined} [s]
   */
  state(s) {
    if (s) this.#state = { ...this.#state, ...s };
    else return { ...this.#state };
  }

  // EVENTS

  /**
   * @param {Event} event
   */
  canplayEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);
    const item = engineItem(audio);

    if (
      item?.hasAttribute("initial-progress") &&
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
      engineItem(audio)?.state({ duration: audio.duration });
    }
  }

  /**
   * @param {Event} event
   */
  endedEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);
    audio.currentTime = 0;

    engineItem(audio)?.state({ hasEnded: true });
  }

  /**
   * @param {Event} event
   */
  errorEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);
    const code = audio.error?.code || 0;

    engineItem(audio)?.state({ loadingState: { error: { code } } });
  }

  /**
   * @param {Event} event
   */
  pauseEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);

    const item = engineItem(audio);
    const itemState = item?.state();
    const ended = itemState
      ? itemState.hasEnded || itemState.progress === 1
      : false;

    item?.state({ isPlaying: false });
    item?.engine?.$isPlaying.set(ended);
  }

  /**
   * @param {Event} event
   */
  playEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);

    engineItem(audio)?.state({ isPlaying: true });
    engineItem(audio)?.engine?.$isPlaying.set(true);

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

    engineItem(audio)?.state({
      progress: isNaN(audio.duration) || audio.duration === 0
        ? 0
        : audio.currentTime / audio.duration,
    });
  }

  /**
   * @param {Event} event
   */
  waitingEvent(event) {
    initiateLoading(event);
  }
}

export { AudioEngineItem };

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

/**
 * @param {HTMLAudioElement} audio
 */
function engineItem(audio) {
  const c = audio.closest("de-audio-item");
  if (c) return /** @type {AudioEngineItem} */ (c);
  else return null;
}

/**
 * @param {Event} event
 */
function finishedLoading(event) {
  const audio = /** @type {HTMLAudioElement} */ (event.target);
  engineItem(audio)?.state({ loadingState: "loaded" });
}

/**
 * @param {Event} event
 */
function initiateLoading(event) {
  const audio = /** @type {HTMLAudioElement} */ (event.target);
  if (audio.readyState < 4) {
    engineItem(audio)?.state({ loadingState: "loading" });
  }
}

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = AudioEngine;
export const NAME = "de-audio";
export const NAME_ITEM = "de-audio-item";

customElements.define(NAME, AudioEngine);
customElements.define(NAME_ITEM, AudioEngineItem);
