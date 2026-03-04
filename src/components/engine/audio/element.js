import { keyed } from "lit-html/directives/keyed.js";

import { BroadcastableDiffuseElement, nothing } from "~/common/element.js";
import { computed, signal, untracked } from "~/common/signal.js";

/**
 * @import {Actions, Audio, AudioState, AudioStateReadOnly, LoadingState} from "./types.d.ts"
 * @import {RenderArg} from "~/common/element.d.ts"
 * @import {SignalReader} from "~/common/signal.d.ts"
 */

////////////////////////////////////////////
// CONSTANTS
////////////////////////////////////////////
const SILENT_MP3 =
  "data:audio/mp3;base64,SUQzBAAAAAAAI1RTU0UAAAAPAAADTGF2ZjU2LjM2LjEwMAAAAAAAAAAAAAAA//OEAAAAAAAAAAAAAAAAAAAAAAAASW5mbwAAAA8AAAAEAAABIADAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDV1dXV1dXV1dXV1dXV1dXV1dXV1dXV1dXV6urq6urq6urq6urq6urq6urq6urq6urq6v////////////////////////////////8AAAAATGF2YzU2LjQxAAAAAAAAAAAAAAAAJAAAAAAAAAAAASDs90hvAAAAAAAAAAAAAAAAAAAA//MUZAAAAAGkAAAAAAAAA0gAAAAATEFN//MUZAMAAAGkAAAAAAAAA0gAAAAARTMu//MUZAYAAAGkAAAAAAAAA0gAAAAAOTku//MUZAkAAAGkAAAAAAAAA0gAAAAANVVV";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {Actions}
 */
class AudioEngine extends BroadcastableDiffuseElement {
  static NAME = "diffuse/engine/audio";

  constructor() {
    super();

    this.isPlaying = this.isPlaying.bind(this);
    this.state = this.state.bind(this);
  }

  // SIGNALS

  #items = signal(/** @type {Audio[]} */ ([]));
  #volume = signal(0.5);

  // STATE

  items = this.#items.get;
  volume = this.#volume.get;

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    // Setup broadcasting if part of group
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(
        this.identifier,
        {
          adjustVolume: { strategy: "replicate", fn: this.adjustVolume },
          pause: { strategy: "leaderOnly", fn: this.pause },
          play: { strategy: "leaderOnly", fn: this.play },
          seek: { strategy: "leaderOnly", fn: this.seek },
          supply: { strategy: "replicate", fn: this.supply },

          // State
          items: { strategy: "leaderOnly", fn: this.items },
        },
      );

      if (!actions) return;

      this.adjustVolume = actions.adjustVolume;
      this.pause = actions.pause;
      this.play = actions.play;
      this.seek = actions.seek;
      this.supply = actions.supply;

      // Sync items with leader if needed
      this.broadcastingStatus().then(async (status) => {
        if (status.leader) return;
        this.#items.value = await actions.items();
      });
    }

    // Super
    super.connectedCallback();

    // Get volume from previous session if possible
    const VOLUME_KEY =
      `${this.constructor.prototype.constructor.NAME}/${this.group}/volume`;
    const volume = localStorage.getItem(VOLUME_KEY);

    if (volume != undefined) {
      this.#volume.set(parseFloat(volume));
    }

    // Monitor volume signal
    this.effect(() => {
      Array.from(this.querySelectorAll("de-audio-item")).forEach(
        (node) => {
          const item = /** @type {AudioEngineItem} */ (node);
          if (item.hasAttribute("preload")) return;
          const audio = item.querySelector("audio");
          if (audio) audio.volume = this.#volume.value;
        },
      );

      localStorage.setItem(VOLUME_KEY, this.#volume.value.toString());
    });

    // Only broadcasting stuff from here on out
    if (!this.broadcasted) return;

    // Manage playback across tabs if needed
    this.effect(async () => {
      const status = await this.broadcastingStatus();
      untracked(() => {
        if (!(status.leader && status.initialLeader === false)) return;

        console.log("🧙 Leadership acquired");
        this.items().forEach((item) => {
          const el = this.#itemElement(item.id);
          if (!el) return;

          el.removeAttribute("initial-progress");

          if (!el.audio) return;

          const progress = el.$state.progress.value;
          const canPlay = () => {
            this.seek({
              audioId: item.id,
              percentage: progress,
            });

            if (el.$state.isPlaying.value) this.play({ audioId: item.id });
          };

          el.audio.addEventListener("canplay", canPlay, { once: true });

          if (el.audio.readyState === 0) el.audio.load();
          else canPlay();
        });
      });
    });
  }

  // ACTIONS

  /**
   * @type {Actions["adjustVolume"]}
   */
  adjustVolume(args) {
    if (args.audioId) {
      this.#withAudioNode(args.audioId, (audio) => {
        audio.volume = args.volume;
      });
    } else {
      this.#volume.value = args.volume;
    }
  }

  /**
   * @type {Actions["pause"]}
   */
  pause({ audioId }) {
    this.#withAudioNode(audioId, (audio) => audio.pause());
  }

  /**
   * @type {Actions["play"]}
   */
  play({ audioId, volume }) {
    this.#withAudioNode(audioId, (audio, item) => {
      audio.volume = volume ?? this.volume();
      audio.muted = false;

      // TODO: Might need this for `data-initial-progress`
      //       Does seem to cause trouble when broadcasting
      //       (open multiple sessions and play the next audio)
      // if (audio.readyState === 0) audio.load();
      if (!audio.isConnected) return;

      const promise = audio.play() || Promise.resolve();
      item.$state.isPlaying.set(true);

      promise.catch((e) => {
        if (!audio.isConnected) {
          return; /* The node was removed from the DOM, we can ignore this error */
        }
        const err =
          "Couldn't play audio automatically. Please resume playback manually.";
        console.error(err, e);
        item.$state.isPlaying.set(false);
      });
    });
  }

  /**
   * @type {Actions["reload"]}
   */
  reload(args) {
    this.#withAudioNode(args.audioId, (audio, item) => {
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
    this.#withAudioNode(audioId, (audio) => {
      if (!isNaN(audio.duration)) {
        audio.currentTime = audio.duration * percentage;
      }
    });
  }

  /**
   * @type {Actions["supply"]}
   */
  supply(args) {
    const existingMap = new Map(this.#items.value.map((a) => [a.id, a]));

    const hasNewIds = args.audio.some((a) => !existingMap.has(a.id));
    const hasPreloadChanges = args.audio.some(
      (a) => existingMap.get(a.id)?.isPreload !== a.isPreload,
    );

    if (hasNewIds || hasPreloadChanges) {
      this.#items.value = args.audio;
    }

    if (args.play) this.play(args.play);
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    const ids = this.items().map((i) => i.id);

    this.querySelectorAll("de-audio-item").forEach((element) => {
      if (ids.includes(element.id)) return;

      const source = element.querySelector("source");
      if (source) source.src = SILENT_MP3;
    });

    const group = this.group;
    const nodes = this.items().map((audio) => {
      const ip = audio.progress === undefined
        ? "0"
        : JSON.stringify(audio.progress);

      return keyed(
        audio.id,
        html`
          <de-audio-item
            group="${this.broadcasted ? `${group}/${audio.id}` : nothing}"
            id="${audio.id}"
            initial-progress="${ip}"
            mime-type="${audio.mimeType ? audio.mimeType : nothing}"
            preload="${audio.isPreload ? `preload` : nothing}"
            url="${audio.url}"
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
        `,
      );
    });

    return html`
      <section id="audio-nodes">
        ${nodes}
      </section>
    `;
  }

  // 🛠️

  /**
   * Convenience signal to track if something is, or was, playing.
   */
  _isPlaying() {
    return computed(() => {
      const item = this.items()?.[0];
      if (!item) return false;

      const state = this.state(item.id);
      if (!state) return false;

      return state.isPlaying() || state.hasEnded() || state.progress() === 1;
    });
  }

  /**
   * Get the state of a single audio item.
   *
   * @param {string} audioId
   * @returns {SignalReader<AudioStateReadOnly | undefined>}
   */
  _state(audioId) {
    return computed(() => {
      const _trigger = this.#items.value;

      const s = this.#itemElement(audioId)?.state;
      return s ? { ...s } : undefined;
    });
  }

  /**
   * Convenience signal to track if something is, or was, playing.
   */
  isPlaying() {
    return this._isPlaying()();
  }

  /**
   * Get the state of a single audio item.
   *
   * @param {string} audioId
   * @returns {AudioStateReadOnly | undefined}
   */
  state(audioId) {
    return this._state(audioId)();
  }

  /**
   * @param {string} audioId
   */
  #itemElement(audioId) {
    const node = this.querySelector(
      `de-audio-item[id="${audioId}"]:not([preload])`,
    );

    if (node) {
      const item = /** @type {AudioEngineItem} */ (node);
      return item;
    }
  }

  /**
   * @param {string} audioId
   * @param {(audio: HTMLAudioElement, item: AudioEngineItem) => void} fn
   */
  #withAudioNode(audioId, fn) {
    const item = this.#itemElement(audioId);
    if (item) fn(item.audio, item);
  }
}

export default AudioEngine;

////////////////////////////////////////////
// ITEM ELEMENT
////////////////////////////////////////////

class AudioEngineItem extends BroadcastableDiffuseElement {
  static NAME = "diffuse/engine/audio/item";

  constructor() {
    super();

    const ip = this.getAttribute("initial-progress");

    /**
     * @type {AudioState}
     */
    this.$state = {
      duration: signal(0),
      hasEnded: signal(false),
      isPlaying: signal(false),
      isPreload: signal(this.hasAttribute("preload")),
      loadingState: signal(/** @type {LoadingState} */ ("loading")),
      progress: signal(ip ? parseFloat(ip) : 0),
    };
  }

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
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

    // Setup broadcasting if part of group
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(
        this.identifier,
        {
          getDuration: { strategy: "leaderOnly", fn: this.$state.duration.get },
          getHasEnded: { strategy: "leaderOnly", fn: this.$state.hasEnded.get },
          getIsPlaying: {
            strategy: "leaderOnly",
            fn: this.$state.isPlaying.get,
          },
          getIsPreload: {
            strategy: "leaderOnly",
            fn: this.$state.isPreload.get,
          },
          getLoadingState: {
            strategy: "leaderOnly",
            fn: this.$state.loadingState.get,
          },
          getProgress: { strategy: "leaderOnly", fn: this.$state.progress.get },

          // SET
          setDuration: { strategy: "replicate", fn: this.$state.duration.set },
          setHasEnded: { strategy: "replicate", fn: this.$state.hasEnded.set },
          setIsPlaying: {
            strategy: "replicate",
            fn: this.$state.isPlaying.set,
          },
          setIsPreload: {
            strategy: "replicate",
            fn: this.$state.isPreload.set,
          },
          setLoadingState: {
            strategy: "replicate",
            fn: this.$state.loadingState.set,
          },
          setProgress: { strategy: "replicate", fn: this.$state.progress.set },
        },
        {
          // Sync leadership with engine's broadcasting channel
          assumeLeadership: (await this.engine?.broadcastingStatus())?.leader,
        },
      );

      if (actions) {
        this.$state.duration.set = actions.setDuration;
        this.$state.hasEnded.set = actions.setHasEnded;
        this.$state.isPlaying.set = actions.setIsPlaying;
        this.$state.isPreload.set = actions.setIsPreload;
        this.$state.loadingState.set = actions.setLoadingState;
        this.$state.progress.set = actions.setProgress;

        untracked(async () => {
          this.$state.duration.value = await actions.getDuration();
          this.$state.hasEnded.value = await actions.getHasEnded();
          this.$state.isPlaying.value = await actions.getIsPlaying();
          this.$state.isPreload.value = await actions.getIsPreload();
          this.$state.loadingState.value = await actions.getLoadingState();
          this.$state.progress.value = await actions.getProgress();
        });
      }
    }

    // Super
    super.connectedCallback();
  }

  // STATE

  /**
   * @type {AudioStateReadOnly}
   */
  get state() {
    return {
      id: this.id,
      mimeType: this.getAttribute("mime-type") ?? undefined,
      url: this.getAttribute("url") ?? "",

      duration: this.$state.duration.get,
      hasEnded: this.$state.hasEnded.get,
      isPlaying: this.$state.isPlaying.get,
      isPreload: this.$state.isPreload.get,
      loadingState: this.$state.loadingState.get,
      progress: this.$state.progress.get,
    };
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
      engineItem(audio)?.$state.duration.set(audio.duration);
    }
  }

  /**
   * @param {Event} event
   */
  endedEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);
    audio.currentTime = 0;

    engineItem(audio)?.$state.hasEnded.set(true);
  }

  /**
   * @param {Event} event
   */
  errorEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);
    const code = audio.error?.code || 0;

    engineItem(audio)?.$state.loadingState.set({ error: { code } });
  }

  /**
   * @param {Event} event
   */
  pauseEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);
    const item = engineItem(audio);

    item?.$state.isPlaying.set(false);
  }

  /**
   * @param {Event} event
   */
  playEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);

    const item = engineItem(audio);
    item?.$state.hasEnded.set(false);
    item?.$state.isPlaying.set(true);

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
    if (isNaN(audio.duration) || audio.duration === 0) return;

    const progress = audio.currentTime / audio.duration;
    if (progress === 0) return;

    engineItem(audio)?.$state.progress.set(progress);
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
  engineItem(audio)?.$state.loadingState.set("loaded");
}

/**
 * @param {Event} event
 */
function initiateLoading(event) {
  const audio = /** @type {HTMLAudioElement} */ (event.target);
  if (audio.readyState < 4) {
    engineItem(audio)?.$state.loadingState.set("loading");
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
