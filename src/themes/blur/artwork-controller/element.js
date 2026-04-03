import { FastAverageColor } from "fast-average-color";
import { Temporal } from "~/common/temporal.js";
import { cache } from "lit-html/directives/cache.js";
import { debounce } from "throttle-debounce";
import { xxh32r } from "xxh32/dist/raw.js";

import {
  DEFAULT_GROUP,
  defineElement,
  DiffuseElement,
  query,
  whenElementsDefined,
} from "~/common/element.js";

import { signal, untracked } from "~/common/signal.js";

/**
 * @import {RenderArg} from "~/common/element.d.ts"
 *
 * @import {InputElement} from "~/components/input/types.d.ts"
 * @import ArtworkOrchestrator from "~/components/orchestrator/artwork/element.js"
 * @import ControllerOrchestrator from "~/components/orchestrator/controller/element.js"
 * @import FavouritesOrchestrator from "~/components/orchestrator/favourites/element.js"
 * @import RepeatShuffleEngine from "~/components/engine/repeat-shuffle/element.js"
 */

class ArtworkController extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
  }

  // VARIABLES

  /** @type {number | undefined} */
  #isLoadingTimeout = undefined;

  // SIGNALS

  #artwork = signal(
    /** @type {{ current: ({ bytes: Uint8Array; mime: string; hash: string; index: number; loaded: boolean; url: string }) | null; previous: ({ bytes: Uint8Array; mime: string; hash: string; index: number; loaded: boolean; url: string }) | null }} */ ({
      current: null,
      previous: null,
    }),
  );

  #artworkColor = signal(/** @type {string | undefined} */ (undefined));
  #artworkLightMode = signal(false);
  #duration = signal("0:00");
  #isLoading = signal(true);
  #time = signal("0:00");

  // SIGNALS - DEPENDENCIES

  $artwork = signal(/** @type {ArtworkOrchestrator | undefined} */ (undefined));
  $controller = signal(
    /** @type {ControllerOrchestrator | undefined} */ (undefined),
  );
  $favourites = signal(
    /** @type {FavouritesOrchestrator | undefined} */ (undefined),
  );
  $input = signal(/** @type {InputElement | undefined} */ (undefined));
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

    /** @type {ArtworkOrchestrator} */
    const artwork = query(this, "artwork-selector");

    /** @type {ControllerOrchestrator} */
    const controller = query(this, "controller-orchestrator-selector");

    /** @type {InputElement} */
    const input = query(this, "input-selector");

    /** @type {FavouritesOrchestrator} */
    const favourites = query(this, "favourites-orchestrator-selector");

    /** @type {RepeatShuffleEngine} */
    const repeatShuffle = query(this, "repeat-shuffle-engine-selector");

    whenElementsDefined({ artwork, controller, favourites, input, repeatShuffle })
      .then(
        () => {
          this.$artwork.value = artwork;
          this.$controller.value = controller;
          this.$input.value = input;
          this.$favourites.value = favourites;
          this.$repeatShuffle.value = repeatShuffle;

          // Changed artwork based on active queue item.
          const debouncedChangeArtwork = debounce(
            1000,
            this.#setArtwork.bind(this),
          );

          this.effect(() => {
            const _trigger = this.currentTrack();
            debouncedChangeArtwork();
          });

          this.effect(() => this.#formatTimestamps());
          this.effect(() => this.#lightOrDark());

          this.effect(() => {
            const now = !!this.$controller.value?.$queue.value?.now();
            const aud = this.audio()?.loadingState();
            const bool = now && aud !== "loaded";

            if (this.#isLoadingTimeout) {
              clearTimeout(this.#isLoadingTimeout);
            }

            if (bool) {
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

  ////////////////////////////////////////////
  // ✨ EFFECTS
  // 🖼️ Artwork
  ////////////////////////////////////////////

  #lightOrDark() {
    const controller = this.root().querySelector(".controller__inner");
    if (!controller) return;

    if (this.#artworkLightMode.value) {
      controller.classList.add("controller__inner--light-mode");
    } else controller.classList.remove("controller__inner--light-mode");
  }

  /** */
  async #setArtwork() {
    const track = this.currentTrack();
    const currArtwork = untracked(this.#artwork.get);

    if (!track) {
      if (currArtwork.current) {
        this.#artwork.value = { current: null, previous: currArtwork.current };
      }

      return;
    }

    if (this.$controller.value?.$queue.value?.now()?.id !== track?.id) {
      return;
    }

    const bytes = await this.$artwork.value?.get(track) ?? null;

    // Check if queue item has changed while fetching the artwork
    const currTrack = this.currentTrack();

    if (track.id === currTrack?.id) {
      this.#artwork.set({
        previous: currArtwork.current
          ? { ...currArtwork.current, loaded: false }
          : null,
        current: bytes
          ? (() => {
            const mime = detectMime(bytes);
            return {
              bytes,
              mime,
              hash: xxh32r(bytes).toString(),
              index: (currArtwork.current?.index ?? 0) + 1,
              loaded: false,
              url: URL.createObjectURL(
                new Blob([/** @type {ArrayBuffer} */ (bytes.buffer)], { type: mime }),
              ),
            };
          })()
          : null,
      });

      if (!bytes) {
        this.#artworkColor.value = undefined;
        this.#artworkLightMode.value = false;
      }
    }
  }

  ////////////////////////////////////////////
  // ✨ EFFECTS
  // ⌚️ Time
  ////////////////////////////////////////////
  #formatTimestamps() {
    const currTrack = this.currentTrack();
    const audio = this.audio();
    const curMs = (audio?.currentTime() ?? 0) * 1000;
    const durMs = currTrack?.stats?.duration ??
      (audio?.duration() != null ? audio.duration() * 1000 : undefined);

    if (audio && durMs && !isNaN(durMs)) {
      const p = Temporal.Duration.from({
        milliseconds: Math.round(curMs),
      }).round({
        largestUnit: "hours",
        smallestUnit: "seconds",
      });

      if (durMs === Infinity) {
        this.#time.value = this.#formatTime(p);
        this.#duration.value = "∞";
        return;
      }

      const d = Temporal.Duration.from({ milliseconds: Math.round(durMs) })
        .round({
          largestUnit: "hours",
          smallestUnit: "seconds",
        });

      this.#time.value = this.#formatTime(p);
      this.#duration.value = this.#formatTime(d);
    } else {
      this.#time.value = "0:00";
      this.#duration.value = "0:00";
    }
  }

  /**
   * @param {import("temporal-polyfill").Temporal.Duration} duration
   */
  #formatTime(duration) {
    return `${duration.hours > 0 ? duration.hours.toFixed(0) + ":" : ""}${
      duration.hours > 0
        ? (duration.minutes > 9
          ? duration.minutes.toFixed(0)
          : "0" + duration.minutes.toFixed(0))
        : duration.minutes.toFixed(0)
    }:${
      duration.seconds > 9
        ? duration.seconds.toFixed(0)
        : "0" + duration.seconds.toFixed(0)
    }`;
  }

  // EVENTS

  /**
   * @param {Event} event
   */
  artworkLoaded = (event) => {
    if (!(event.target instanceof HTMLImageElement)) return;

    const hash = event.target.getAttribute("data-hash");
    if (!hash) return;

    if (hash !== this.#artwork.value.current?.hash) return;
    if (this.#artwork.value.current?.loaded) return;

    const fac = new FastAverageColor();
    const color = fac.getColor(event.target);
    const rgb = color.value;
    const o = Math.round(
      (rgb[0] * 299 + rgb[1] * 587 + rgb[2] * 114) / 1000,
    );

    this.#artworkColor.value = color.rgba;
    this.#artworkLightMode.value = o > 165;
    this.#artwork.value = {
      previous: this.#artwork.value.previous,
      current: { ...this.#artwork.value.current, loaded: true },
    };
  };

  fullVolume = () => {
    this.$controller.value?.$audio.value?.adjustVolume({ volume: 1 });
  };

  mute = () => {
    this.$controller.value?.$audio.value?.adjustVolume({ volume: 0 });
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
    const target = event.target
      ? /** @type {HTMLProgressElement} */ (event.target)
      : null;
    const percentage = target ? event.offsetX / target.clientWidth : 0;
    const audioId = this.$controller.value?.$queue.value?.now()?.id;

    if (audioId) this.$controller.value?.$audio.value?.seek({ audioId, percentage });
  };

  /**
   * @param {MouseEvent} event
   */
  setVolume = (event) => {
    const target = event.target
      ? /** @type {HTMLProgressElement} */ (event.target)
      : null;

    const percentage = target ? event.offsetX / target.clientWidth : 0;
    this.$controller.value?.$audio.value?.adjustVolume({ volume: percentage });
  };

  toggleFavourite = () => {
    const track = this.currentTrack();
    if (!track) return;

    this.$favourites.value?.toggle(track);
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
    const activeQueueItem = this.currentTrack();
    const isFav = activeQueueItem
      ? this.$favourites.value?.isFavourite(activeQueueItem) ?? false
      : false;
    const isRepeat = this.$repeatShuffle.value?.repeat() ?? false;
    const isShuffle = this.$repeatShuffle.value?.shuffle() ?? false;

    // Artwork
    const artworkArr = [
      this.#artwork.value.previous,
      this.#artwork.value.current,
    ].sort((a, b) => {
      if (!a || !b) return 0;
      return a.index % 2 ? 1 : -1;
    });

    const artwork = artworkArr.map((art) => {
      if (art === null) {
        return null;
      }

      return cache(html`
        <img
          @load="${this.artworkLoaded}"
          data-hash="${art.hash}"
          src="${art.url}"
          style="opacity: ${art.loaded ? `1` : `0`}"
        />
      `);
    });

    return html`
      <link rel="stylesheet" href="vendor/@phosphor-icons/web/bold/style.css" />
      <link rel="stylesheet" href="vendor/@phosphor-icons/web/fill/style.css" />
      <link rel="stylesheet" href="styles/animations.css" />
      <link rel="stylesheet" href="themes/blur/artwork-controller/element.css" />

      <main style="background-color: ${this.#artworkColor.value ??
        `var(--color-3)`}; opacity: 0;">
        <section class="artwork">
          <label style="display: ${this.group === DEFAULT_GROUP
            ? `none`
            : `block`};">
            ${this.group}
          </label>

          ${artwork}
        </section>

        <section class="controller">
          <div class="gradient-blur">
            <div></div>
            <div></div>
            <div></div>
            <div></div>
            <div></div>
            <div></div>
            <div></div>
            <div></div>
          </div>

          <div
            class="controller__background"
            style="background-color: ${this.#artworkColor.value ??
              `transparent`};"
          >
          </div>

          <section class="controller__inner">
            <!-- NOW PLAYING -->

            <cite>
              <strong>${activeQueueItem?.tags?.title ||
                "Diffuse"}</strong>
              <span style="font-style: ${activeQueueItem
                ? `normal`
                : `italic`}">
                ${activeQueueItem?.tags?.artist ??
                  (activeQueueItem ? `` : `Waiting on queue ...`)}
              </span>
            </cite>

            <!-- PROGRESS -->

            <div class="progress" @click="${this.seek}">
              <progress max="100" value="${(this.audio()?.loadingState() ===
                  "loaded"
                ? (this.audio()?.progress() ?? 0)
                : 0) * 100}"></progress>
              <div class="timestamps">
                <time datetime="${this.#time.value}">${this.#time.value}</time>
                <time datetime="${this.#time.value}">${this.#duration
                  .value}</time>
              </div>
            </div>

            <!-- CONTROLS -->

            <menu>
              <!-- previous -->
              <li @click="${this.previous}">
                <i class="ph-fill ph-rewind" title="Previous track"></i>
              </li>

              <!-- loading ... -->
              <div
                class="animate-bounce menu__loader"
                style="display: ${this.#isLoading.value ? `inherit` : `none`};"
              >
                <i class="ph-fill ph-vinyl-record" title="Loading ..."></i>
              </div>

              <!-- play -->
              <li
                @click="${this.playPause}"
                style="display: ${!this.#isLoading.value &&
                    !this.isPlaying()
                  ? `inline`
                  : `none`};"
              >
                <i class="ph-fill ph-play" title="Play"></i>
              </li>

              <!-- pause -->
              <li
                @click="${this.playPause}"
                style="display: ${!this.#isLoading.value && this.isPlaying()
                  ? `inline`
                  : `none`};"
              >
                <i class="ph-fill ph-pause" title="Pause"></i>
              </li>

              <!-- next -->
              <li @click="${this.next}">
                <i class="ph-fill ph-fast-forward" title="Next track"></i>
              </li>
            </menu>

            <!-- VOLUME -->

            <div class="volume">
              <i @click="${this.mute}" class="ph-fill ph-speaker-none"></i>
              <div @click="${this.setVolume}" class="progress-bar">
                <progress max="100" value="${(this.$controller.value?.$audio.value?.volume() ??
                  0) * 100}"></progress>
              </div>
              <i @click="${this
                .fullVolume}" class="ph-fill ph-speaker-high"></i>
            </div>

            <footer>
              <div class="button-row">
                <button
                  title="Toggle repeat"
                  data-enabled="${isRepeat ? `t` : `f`}"
                  @click="${this.toggleRepeat}"
                >
                  <i class="ph-${isRepeat ? `fill` : `bold`} ph-repeat"></i>
                </button>
                <button
                  title="Toggle favourite"
                  data-enabled="${isFav ? `t` : `f`}"
                  @click="${this.toggleFavourite}"
                >
                  <i class="ph-${isFav ? `fill` : `bold`} ph-star"></i>
                </button>
                <button
                  title="Toggle shuffle"
                  data-enabled="${isShuffle ? `t` : `f`}"
                  @click="${this.toggleShuffle}"
                >
                  <i class="ph-${isShuffle ? `fill` : `bold`} ph-shuffle"></i>
                </button>
              </div>
            </footer>
          </section>
        </section>
      </main>
    `;
  }
}

export default ArtworkController;

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

export const CLASS = ArtworkController;
export const NAME = "db-artwork-controller";

defineElement(NAME, CLASS);
