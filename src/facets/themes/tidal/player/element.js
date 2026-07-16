import {
  defineElement,
  DiffuseElement,
  query,
  queryOptional,
  whenElementsDefined,
} from "~/common/element.js";
import { batch, signal } from "~/common/signal.js";

/**
 * @import {RenderArg} from "~/common/element.d.ts"
 * @import ArtworkOrchestrator from "~/components/orchestrator/artwork/element.js"
 * @import ControllerOrchestrator from "~/components/orchestrator/controller/element.js"
 * @import FavouritesOrchestrator from "~/components/orchestrator/favourites/element.js"
 * @import QueueEngine from "~/components/engine/queue/element.js"
 * @import RepeatShuffleEngine from "~/components/engine/repeat-shuffle/element.js"
 * @import {InputElement} from "@specs/components/input/types.d.ts"
 */

class Player extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
  }

  // SIGNALS

  $artwork = signal(
    /** @type {ArtworkOrchestrator | undefined} */ (undefined),
  );

  $controller = signal(
    /** @type {ControllerOrchestrator | undefined} */ (undefined),
  );

  $favourites = signal(
    /** @type {FavouritesOrchestrator | undefined} */ (undefined),
  );

  $queue = signal(
    /** @type {QueueEngine | undefined} */ (undefined),
  );

  $repeatShuffle = signal(
    /** @type {RepeatShuffleEngine | undefined} */ (undefined),
  );

  $input = signal(
    /** @type {InputElement | undefined} */ (undefined),
  );

  #artUrl = signal(/** @type {string | null | undefined} */ (undefined));
  #audioError = signal(false);
  #isLoading = signal(false);
  #lastArtKey = /** @type {string | undefined} */ (undefined);
  #volumeOpen = signal(false);
  #lastNonZeroVolume = signal(0.75);
  /** @type {ReturnType<typeof setTimeout> | undefined} */
  #isLoadingTimeout = undefined;
  /** @type {ReturnType<typeof setTimeout> | undefined} */
  #volumeCloseTimeout = undefined;

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    /** @type {ArtworkOrchestrator} */
    const artwork = query(this, "artwork-selector");

    /** @type {ControllerOrchestrator} */
    const controller = query(this, "controller-orchestrator-selector");

    /** @type {FavouritesOrchestrator} */
    const favourites = query(this, "favourites-orchestrator-selector");

    /** @type {QueueEngine} */
    const queue = query(this, "queue-engine-selector");

    /** @type {RepeatShuffleEngine} */
    const repeatShuffle = query(this, "repeat-shuffle-engine-selector");

    /** @type {InputElement | null} */
    const input = queryOptional(this, "input-selector");

    whenElementsDefined({
      artwork,
      controller,
      favourites,
      queue,
      repeatShuffle,
    }).then(() => {
      batch(() => {
        this.$artwork.value = artwork;
        this.$controller.value = controller;
        this.$favourites.value = favourites;
        this.$queue.value = queue;
        this.$repeatShuffle.value = repeatShuffle;
      });
    });

    if (input) {
      whenElementsDefined({ input }).then(() => {
        this.$input.value = input;
      });
    }

    // Fetch artwork for the current track
    this.effect(() => {
      const track = this.$controller.value?.currentTrack();
      const artKey = track
        ? String(track.tags?.album ?? "").toLowerCase()
        : "";

      if (!track || !artKey) {
        this.#artUrl.value = undefined;
        this.#lastArtKey = undefined;
        return;
      }

      if (this.#lastArtKey === artKey) return;
      this.#lastArtKey = artKey;
      this.#artUrl.value = undefined;

      this.$artwork.value?.get(track).then((bytes) => {
        if (this.#lastArtKey !== artKey) return;
        if (!bytes) {
          this.#artUrl.value = null;
          return;
        }
        const mime = detectMime(bytes);
        const url = URL.createObjectURL(
          new Blob([bytes], { type: mime }),
        );
        this.#artUrl.value = url;
      });
    });

    // Loading state
    this.effect(() => {
      const ctrl = this.$controller.value;
      const now = !!ctrl?.$queue.value?.now();
      const aud = ctrl?.audio();
      const state = aud?.loadingState();
      const isError = now && typeof state === "object" && state !== null &&
        "error" in state;
      // When audio() is undefined (engine not yet created or destroyed),
      // there is no audio to wait on — don't show the loading spinner.
      const isLoading = now && !isError && state !== undefined &&
        state !== "loaded";

      this.#audioError.value = !!isError;

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
  }

  /**
   * @override
   */
  disconnectedCallback() {
    super.disconnectedCallback();
    if (this.#isLoadingTimeout) {
      clearTimeout(this.#isLoadingTimeout);
      this.#isLoadingTimeout = undefined;
    }
    if (this.#volumeCloseTimeout) {
      clearTimeout(this.#volumeCloseTimeout);
      this.#volumeCloseTimeout = undefined;
    }
  }

  // ACTIONS

  playPause = () => {
    const ctrl = this.$controller.value;
    if (!ctrl) return;
    const audioId = ctrl.$queue.value?.now()?.id;
    if (!audioId) return;
    if (ctrl.isPlaying()) {
      ctrl.$audio.value?.pause({ audioId });
    } else {
      ctrl.$audio.value?.play({ audioId });
    }
  };

  next = () => this.$queue.value?.shift();
  previous = () => this.$queue.value?.unshift();

  toggleShuffle = () => {
    const rs = this.$repeatShuffle.value;
    if (rs) rs.setShuffle(!rs.shuffle());
  };

  toggleRepeat = () => {
    const rs = this.$repeatShuffle.value;
    if (rs) rs.setRepeat(!rs.repeat());
  };

  toggleFavourite = () => {
    const track = this.$controller.value?.currentTrack();
    if (track) this.$favourites.value?.toggle(track);
  };

  reload = () => {
    const ctrl = this.$controller.value;
    if (!ctrl) return;
    const audioId = ctrl.$queue.value?.now()?.id;
    if (!audioId) return;
    const progress = ctrl.audio()?.progress();
    ctrl.$audio.value?.reload({ audioId, play: true, progress });
  };

  mute = () => {
    const current = this.$controller.value?.$audio.value?.volume() ?? 1;
    if (current > 0) {
      this.#lastNonZeroVolume.value = current;
    }
    this.$controller.value?.$audio.value?.adjustVolume({ volume: 0 });
  };

  fullVolume = () => {
    this.$controller.value?.$audio.value?.adjustVolume({ volume: 1 });
  };

  toggleMute = () => {
    const vol = this.$controller.value?.$audio.value?.volume() ?? 1;
    if (vol === 0) {
      const restore = this.#lastNonZeroVolume.value > 0
        ? this.#lastNonZeroVolume.value
        : 0.75;
      this.$controller.value?.$audio.value?.adjustVolume({
        volume: restore,
      });
    } else {
      this.mute();
    }
  };

  openVolume = () => {
    if (this.#volumeCloseTimeout) {
      clearTimeout(this.#volumeCloseTimeout);
      this.#volumeCloseTimeout = undefined;
    }
    this.#volumeOpen.value = true;
  };

  scheduleCloseVolume = () => {
    if (this.#volumeCloseTimeout) {
      clearTimeout(this.#volumeCloseTimeout);
    }
    this.#volumeCloseTimeout = setTimeout(() => {
      this.#volumeOpen.value = false;
      this.#volumeCloseTimeout = undefined;
    }, 150);
  };

  cancelCloseVolume = () => {
    if (this.#volumeCloseTimeout) {
      clearTimeout(this.#volumeCloseTimeout);
      this.#volumeCloseTimeout = undefined;
    }
  };

  /**
   * @param {MouseEvent} event
   */
  setVolume = (event) => {
    const target = /** @type {HTMLElement | null} */ (event.target);
    if (!target) return;
    const rect = target.getBoundingClientRect();
    const clickY = event.clientY - rect.top;
    const height = rect.height;
    const percentage = Math.max(
      0,
      Math.min(1, 1 - clickY / height),
    );
    if (percentage > 0) {
      this.#lastNonZeroVolume.value = percentage;
    }
    this.$controller.value?.$audio.value?.adjustVolume({ volume: percentage });
  };

  /**
   * @param {MouseEvent} event
   */
  seek = (event) => {
    const target = event.target
      ? /** @type {HTMLProgressElement} */ (event.target)
      : null;
    const percentage = target ? event.offsetX / target.clientWidth : 0;
    const ctrl = this.$controller.value;
    const audioId = ctrl?.$queue.value?.now()?.id;
    if (audioId) {
      ctrl?.$audio.value?.seek({ audioId, percentage });
    }
  };

  // RENDER

  /**
   * @param {RenderArg} { html }
   */
  render({ html }) {
    const artUrl = this.#artUrl.value;

    return html`
      <link rel="stylesheet" href="vendor/@phosphor-icons/web/bold/style.css" />
      <link rel="stylesheet" href="vendor/@phosphor-icons/web/fill/style.css" />
      <link rel="stylesheet" href="facets/themes/tidal/variables.css" />
      <link rel="stylesheet" href="facets/themes/tidal/player/element.css" />

      <div class="td-player">
        <!-- LEFT: track info -->
        <div class="td-player__left">
          <div class="td-player__art">
            ${artUrl
              ? html`<img src="${artUrl}" alt="" />`
              : html`
                <div class="td-player__art-placeholder">
                  <i class="ph-fill ph-music-notes"></i>
                </div>
              `}
          </div>
          <div class="td-player__meta">
            <span class="td-player__title">
              ${this.$controller.value?.currentTrack()?.tags?.title ?? ""}
            </span>
            <span class="td-player__artist">
              ${this.$controller.value?.currentTrack()?.tags?.artist ?? ""}
            </span>
          </div>
          <button
            class="td-player__fav ${this.$controller.value?.currentTrack() &&
              this.$favourites.value?.isFavourite(
                this.$controller.value.currentTrack(),
              )
              ? `td-player__fav--active`
              : ""}"
            @click="${this.toggleFavourite}"
            title="${this.$controller.value?.currentTrack() &&
              this.$favourites.value?.isFavourite(
                this.$controller.value.currentTrack(),
              )
              ? `Remove from favourites`
              : `Add to favourites`}"
            ?disabled="${!this.$controller.value?.currentTrack()}"
          >
            <i class="${this.$controller.value?.currentTrack() &&
              this.$favourites.value?.isFavourite(
                this.$controller.value.currentTrack(),
              )
              ? `ph-fill ph-heart`
              : `ph-bold ph-heart`}"></i>
          </button>
        </div>

        <!-- CENTER: controls + progress -->
        <div class="td-player__center">
          <div class="td-player__controls">
            <button
              class="td-player__btn ${this.$repeatShuffle.value?.shuffle()
                ? `td-player__btn--active`
                : ""}"
              @click="${this.toggleShuffle}"
              title="Toggle shuffle"
              ?disabled="${!this.$controller.value?.currentTrack()}"
            >
              <i class="ph-bold ph-shuffle"></i>
            </button>
            <button
              class="td-player__btn"
              @click="${this.previous}"
              title="Previous track"
              ?disabled="${!this.$controller.value?.currentTrack()}"
            >
              <i class="ph-fill ph-skip-back"></i>
            </button>
            <button
              class="td-player__play"
              @click="${this.playPause}"
              title="${this.$controller.value?.isPlaying() ? `Pause` : `Play`}"
              ?disabled="${!this.$controller.value?.currentTrack()}"
            >
              ${this.#isLoading.value
                ? html`<i class="ph-fill ph-circle-notch td-player__spin"></i>`
                : this.#audioError.value
                ? html`<i class="ph-fill ph-warning-circle"></i>`
                : this.$controller.value?.isPlaying()
                ? html`<i class="ph-fill ph-pause"></i>`
                : html`<i class="ph-fill ph-play"></i>`}
            </button>
            <button
              class="td-player__btn"
              @click="${this.next}"
              title="Next track"
              ?disabled="${!this.$controller.value?.currentTrack()}"
            >
              <i class="ph-fill ph-skip-forward"></i>
            </button>
            <button
              class="td-player__btn ${this.$repeatShuffle.value?.repeat()
                ? `td-player__btn--active`
                : ""}"
              @click="${this.toggleRepeat}"
              title="Toggle repeat"
              ?disabled="${!this.$controller.value?.currentTrack()}"
            >
              <i class="ph-bold ph-repeat"></i>
            </button>
          </div>
          <div class="td-player__progress">
            <span class="td-player__time">
              ${formatTime(
                this.$controller.value?.audio()?.currentTime() ?? 0,
              )}
            </span>
            <div class="td-player__seek" @click="${this.seek}">
              <div
                class="td-player__seek-fill"
                style="width: ${(() => {
                  const audio = this.$controller.value?.audio();
                  const track = this.$controller.value?.currentTrack();
                  const ct = audio?.currentTime() ?? 0;
                  const dur = track?.stats?.duration
                    ? track.stats.duration / 1000
                    : (audio?.duration() ?? 0);
                  return dur > 0 ? (ct / dur) * 100 : 0;
                })()}%"
              ></div>
            </div>
            <span class="td-player__time">
              ${(() => {
                const audio = this.$controller.value?.audio();
                const track = this.$controller.value?.currentTrack();
                const dur = track?.stats?.duration
                  ? track.stats.duration / 1000
                  : (audio?.duration() ?? 0);
                return dur > 0 ? formatTime(dur) : "0:00";
              })()}
            </span>
          </div>
        </div>

        <!-- RIGHT: volume + actions -->
        <div class="td-player__right">
          <button
            class="td-player__icon-btn"
            @click="${this.toggleMute}"
            @mouseenter="${this.openVolume}"
            @mouseleave="${this.scheduleCloseVolume}"
            title="${(this.$controller.value?.$audio.value?.volume() ?? 1) === 0
              ? `Unmute`
              : `Mute`}"
          >
            ${(this.$controller.value?.$audio.value?.volume() ?? 1) === 0
              ? html`<i class="ph-fill ph-speaker-x"></i>`
              : (this.$controller.value?.$audio.value?.volume() ?? 1) < 0.5
              ? html`<i class="ph-fill ph-speaker-low"></i>`
              : html`<i class="ph-fill ph-speaker-high"></i>`}
          </button>
          ${this.#volumeOpen.value
            ? html`
              <div
                class="td-player__volume-popover"
                @mouseenter="${this.openVolume}"
                @mouseleave="${this.scheduleCloseVolume}"
              >
                <div
                  class="td-player__volume-bar"
                  @click="${this.setVolume}"
                >
                  <div
                    class="td-player__volume-fill"
                    style="height: ${(this.$controller.value?.$audio.value
                      ?.volume() ?? 1) * 100}%"
                  ></div>
                </div>
              </div>
            `
            : ``}
        </div>
      </div>
    `;
  }
}

/**
 * @param {number} seconds
 */
function formatTime(seconds) {
  if (!Number.isFinite(seconds) || seconds < 0) seconds = 0;
  const m = Math.floor(seconds / 60);
  const s = Math.floor(seconds % 60);
  return `${m}:${String(s).padStart(2, "0")}`;
}

/**
 * @param {Uint8Array} bytes
 */
function detectMime(bytes) {
  if (bytes[0] === 0xFF && bytes[1] === 0xD8) return "image/jpeg";
  if (bytes[0] === 0x89 && bytes[1] === 0x50) return "image/png";
  if (bytes[0] === 0x47 && bytes[1] === 0x49) return "image/gif";
  if (bytes[0] === 0x52 && bytes[1] === 0x49) return "image/webp";
  return "image/jpeg";
}

export default Player;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = Player;
export const NAME = "db-tidal-player";

defineElement(NAME, CLASS);
