import {
  defineElement,
  DiffuseElement,
  query,
  whenElementsDefined,
} from "~/common/element.js";

import { signal } from "~/common/signal.js";

/**
 * @import {RenderArg} from "~/common/element.d.ts"
 *
 * @import ControllerOrchestrator from "~/components/orchestrator/controller/element.js"
 * @import RepeatShuffleEngine from "~/components/engine/repeat-shuffle/element.js"
 */

/**
 * Classic audio controller — a faithful recreation of the classic Diffuse
 * console (commit 01ea0c8472b67187d03669327b353d9625c04ef0).
 *
 * Renders a now-playing label, a thin progress bar, and a row of transport
 * buttons with light indicators above each one. No artwork, no volume slider,
 * no favourites — just the essentials, exactly like the original.
 */
class ClassicController extends DiffuseElement {
  static observedAttributes = ["group-label"];

  constructor() {
    super();
    this.attachShadow({ mode: "open" });
  }

  // VARIABLES

  /** @type {ReturnType<typeof setTimeout> | undefined} */
  #isLoadingTimeout = undefined;

  // SIGNALS

  #audioError = signal(false);
  #isLoading = signal(true);

  // SIGNALS - DEPENDENCIES

  $controller = signal(
    /** @type {ControllerOrchestrator | undefined} */ (undefined),
  );
  $repeatShuffle = signal(
    /** @type {RepeatShuffleEngine | undefined} */ (undefined),
  );

  // SIGNALS - COMPUTED

  audio = () => this.$controller.value?.audio();
  currentTrack = () => this.$controller.value?.currentTrack();
  isPlaying = () => this.$controller.value?.isPlaying();

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    /** @type {ControllerOrchestrator} */
    const controller = query(this, "controller-orchestrator-selector");

    /** @type {RepeatShuffleEngine} */
    const repeatShuffle = query(this, "repeat-shuffle-engine-selector");

    whenElementsDefined({
      controller,
      repeatShuffle,
    })
      .then(
        () => {
          this.$controller.value = controller;
          this.$repeatShuffle.value = repeatShuffle;

          this.effect(() => {
            const now = !!this.$controller.value?.$queue.value?.now();
            const aud = this.audio()?.loadingState();
            const isError = now && typeof aud === "object" && aud !== null &&
              "error" in aud;
            const isLoading = now && !isError && aud !== "loaded";

            this.#audioError.value = isError;

            if (this.#isLoadingTimeout) {
              clearTimeout(this.#isLoadingTimeout);
            }

            if (isLoading) {
              this.#isLoadingTimeout = setTimeout(
                () => this.#isLoading.value = true,
                2000,
              );
            } else {
              this.#isLoading.value = false;
            }
          });
        },
      );
  }

  // EVENTS

  reload = () => {
    const audioId = this.$controller.value?.$queue.value?.now()?.id;
    if (audioId) {
      const progress = this.audio()?.progress();
      this.$controller.value?.$audio.value?.reload({
        audioId,
        play: true,
        progress,
      });
    }
  };

  next = () => {
    this.$controller.value?.$queue.value?.shift();
  };

  playPause = () => {
    const audioId = this.$controller.value?.$queue.value?.now()?.id;

    if (this.isPlaying() && audioId) {
      this.$controller.value?.$audio.value?.pause({ audioId });
    } else if (audioId) {
      this.$controller.value?.$audio.value?.play({ audioId });
    }
  };

  previous = () => {
    this.$controller.value?.$queue.value?.unshift();
  };

  /**
   * @param {MouseEvent} event
   */
  seek = (event) => {
    const target = event.currentTarget
      ? /** @type {HTMLElement} */ (event.currentTarget)
      : null;
    const percentage = target ? event.offsetX / target.clientWidth : 0;
    const audioId = this.$controller.value?.$queue.value?.now()?.id;

    if (audioId) {
      this.$controller.value?.$audio.value?.seek({ audioId, percentage });
    }
  };

  toggleRepeat = () => {
    const rs = this.$repeatShuffle.value;
    if (rs) rs.setRepeat(!rs.repeat());
  };

  toggleShuffle = () => {
    const rs = this.$repeatShuffle.value;
    if (rs) rs.setShuffle(!rs.shuffle());
  };

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    const track = this.currentTrack();
    const isRepeat = this.$repeatShuffle.value?.repeat() ?? false;
    const isShuffle = this.$repeatShuffle.value?.shuffle() ?? false;
    const playing = this.isPlaying();

    // Now playing text
    let nowPlayingText = "Diffuse";
    if (this.#audioError.value) {
      nowPlayingText = "(!) An error occurred while decoding the audio";
    } else if (this.#isLoading.value) {
      nowPlayingText = "Loading track ...";
    } else if (track) {
      const artist = track.tags?.artist;
      const title = track.tags?.title ?? "";
      nowPlayingText = artist ? `${artist} - ${title}` : title;
    }

    const progress = (this.audio()?.progress() ?? 0) * 100;

    return html`
      <link rel="stylesheet" href="vendor/@phosphor-icons/web/bold/style.css" />
      <link rel="stylesheet" href="vendor/@phosphor-icons/web/fill/style.css" />
      <link rel="stylesheet"
        href="facets/themes/blur-classic/controller/element.css" />

      <main>
        <!-- NOW PLAYING -->
        <p class="now-playing" title="${nowPlayingText}">${nowPlayingText}</p>

        <!-- PROGRESS BAR -->
        <div class="progress" @click="${this.seek}">
          <div class="progress__track">
            <div class="progress__fill" style="width: ${progress}%;"></div>
          </div>
        </div>

        <!-- CONTROLS -->
        <div class="controls">
          <!-- repeat -->
          <button class="control-btn" title="Toggle repeat" @click="${this
            .toggleRepeat}">
            <span class="light light--small ${isRepeat
              ? "light--on-blue"
              : ""}"></span>
            <span class="control-btn__icon">
              <i class="ph-bold ph-repeat"></i>
            </span>
          </button>

          <!-- previous -->
          <button class="control-btn" title="Play previous track"
            @click="${this.previous}">
            <span class="light light--placeholder"></span>
            <span class="control-btn__icon">
              <i class="ph-fill ph-rewind"></i>
            </span>
          </button>

          <!-- error -->
          <button
            class="control-btn"
            style="display: ${this.#audioError.value ? "flex" : "none"};"
            title="Reload"
            @click="${this.reload}"
          >
            <span class="light light--placeholder"></span>
            <span class="control-btn__icon">
              <i class="ph-fill ph-warning-circle"></i>
            </span>
          </button>

          <!-- play / pause -->
          <button
            class="control-btn"
            title="${playing ? "Pause" : "Play"}"
            @click="${this.playPause}"
            style="display: ${this.#audioError.value ? "none" : "flex"};"
          >
            <span class="light light--large ${playing
              ? "light--on-green"
              : ""}"></span>
            <span class="control-btn__icon control-btn__icon--play">
              <span class="play-text">PLAY</span>
            </span>
          </button>

          <!-- next -->
          <button class="control-btn" title="Play next track" @click="${this
            .next}">
            <span class="light light--placeholder"></span>
            <span class="control-btn__icon">
              <i class="ph-fill ph-fast-forward"></i>
            </span>
          </button>

          <!-- shuffle -->
          <button class="control-btn" title="Toggle shuffle" @click="${this
            .toggleShuffle}">
            <span class="light light--small ${isShuffle
              ? "light--on-blue"
              : ""}"></span>
            <span class="control-btn__icon">
              <i class="ph-bold ph-shuffle"></i>
            </span>
          </button>
        </div>
      </main>
    `;
  }
}

export default ClassicController;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ClassicController;
export const NAME = "db-blur-classic-controller";

defineElement(NAME, CLASS);
