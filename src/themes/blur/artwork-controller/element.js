import { FastAverageColor } from "fast-average-color";
import { Temporal } from "@js-temporal/polyfill";
import { xxh32r } from "xxh32/dist/raw.js";
import { debounce } from "throttle-debounce";

import {
  DiffuseElement,
  keyed,
  query,
  whenElementsDefined,
} from "@common/element.js";
import { trackArtworkCacheId } from "@common/index.js";
import { computed, signal } from "@common/signal.js";

/**
 * @import {RenderArg} from "@common/element.d.ts"
 * @import {Track} from "@definitions/types.d.ts"
 *
 * @import {InputElement} from "@components/input/types.d.ts"
 * @import {Artwork} from "@components/processor/artwork/types.d.ts"
 * @import AudioEngine from "@components/engine/audio/element.js"
 * @import QueueEngine from "@components/engine/queue/element.js"
 * @import ArtworkProcessor from "@components/processor/artwork/element.js"
 */

class ArtworkController extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });

    // Bind event handlers to self
    this.artworkLoaded = this.artworkLoaded.bind(this);
    this.fullVolume = this.fullVolume.bind(this);
    this.mute = this.mute.bind(this);
    this.next = this.next.bind(this);
    this.playPause = this.playPause.bind(this);
    this.previous = this.previous.bind(this);
    this.seek = this.seek.bind(this);
    this.setVolume = this.setVolume.bind(this);
  }

  // VARIABLES

  /** @type {number | undefined} */
  #isLoadingTimeout = undefined;

  // SIGNALS

  #artwork = signal(/** @type {Artwork[]} */ ([]), { eager: true });
  #artworkColor = signal(/** @type {string | undefined} */ (undefined));
  #artworkLightMode = signal(false);
  #duration = signal("0:00");
  #isLoading = signal(false);
  #time = signal("0:00");

  // SIGNALS - DEPENDENCIES

  $artwork = signal(/** @type {ArtworkProcessor | undefined} */ (undefined));
  $audio = signal(/** @type {AudioEngine | undefined} */ (undefined));
  $input = signal(/** @type {InputElement | undefined} */ (undefined));
  $queue = signal(/** @type {QueueEngine | undefined} */ (undefined));

  // SIGNALS - COMPUTED

  #audio = computed(() => {
    const curr = this.$queue.value?.now();
    return curr ? this.$audio.value?.state(curr.id) : undefined;
  });

  #isPlaying = computed(() => {
    return !!this.$queue.value?.now() &&
      this.$audio.value?.isPlaying() === true;
  });

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    /** @type {ArtworkProcessor} */
    const artwork = query(this, "artwork-processor-selector");

    /** @type {AudioEngine} */
    const audio = query(this, "audio-engine-selector");

    /** @type {InputElement} */
    const input = query(this, "input-selector");

    /** @type {QueueEngine} */
    const queue = query(this, "queue-engine-selector");

    this.$artwork.value = artwork;
    this.$audio.value = audio;
    this.$input.value = input;
    this.$queue.value = queue;

    whenElementsDefined({ audio, artwork, input, queue }).then(() => {
      // Changed artwork based on active queue item.
      const debouncedChangeArtwork = debounce(
        1000,
        this.#setArtwork.bind(this),
      );

      this.effect(() => {
        debouncedChangeArtwork(queue.now());
      });

      this.effect(() => this.#formatTimestamps());
      this.effect(() => this.#lightOrDark());

      this.effect(() => {
        const now = !!queue.now();
        const bool = !now ||
          (now && this.#audio()?.loadingState() !== "loaded");

        if (this.#isLoadingTimeout) {
          clearTimeout(this.#isLoadingTimeout);
        }

        if (bool) {
          this.#isLoadingTimeout = setTimeout(
            () => this.#isLoading.value = true,
            2000,
          );
        } else {
          this.#isLoading.set(false);
        }
      });
    });
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

  /**
   * @param {Track | null} track
   */
  async #setArtwork(track) {
    console.log("SET", track);

    if (!track) {
      this.#artwork.value = [];
      return;
    }

    const cacheId = await trackArtworkCacheId(track);

    const resGet = await this.$input.value?.resolve({
      method: "GET",
      uri: track.uri,
    });

    const resHead = await this.$input.value?.resolve({
      method: "HEAD",
      uri: track.uri,
    });

    if (!resGet) return;

    const request = "stream" in resGet
      ? {
        cacheId,
        stream: resGet.stream,
        tags: track.tags,
      }
      : {
        cacheId,
        tags: track.tags,
        urls: {
          get: resGet.url,
          head: resHead && "url" in resHead ? resHead.url : resGet.url,
        },
      };

    const art = await this.$artwork.value?.artwork(request) ?? [];
    const currCacheId = track ? await trackArtworkCacheId(track) : undefined;
    if (cacheId === currCacheId) this.#artwork.set(art);
  }

  ////////////////////////////////////////////
  // ✨ EFFECTS
  // ⌚️ Time
  ////////////////////////////////////////////
  #formatTimestamps() {
    const curr = this.$queue.value?.now?.() ?? undefined;
    const audio = this.#audio();
    const prog = audio?.progress() ?? 0;
    const dur = curr?.stats?.duration ?? audio?.duration();

    if (audio && dur != undefined && !isNaN(dur)) {
      const p = Temporal.Duration.from({
        milliseconds: Math.round(dur * prog * 1000),
      }).round({
        largestUnit: "hours",
      });

      const d = Temporal.Duration.from({ milliseconds: Math.round(dur * 1000) })
        .round({
          largestUnit: "hours",
        });

      this.#time.value = this.#formatTime(p);
      this.#duration.value = this.#formatTime(d);
    } else {
      this.#time.value = "0:00";
      this.#duration.value = "0:00";
    }
  }

  /**
   * @param {Temporal.Duration} duration
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
  artworkLoaded(event) {
    if (!(event.target instanceof HTMLImageElement)) return;

    const fac = new FastAverageColor();
    const color = fac.getColor(event.target);
    const rgb = color.value;
    const o = Math.round(
      (rgb[0] * 299 + rgb[1] * 587 + rgb[2] * 114) / 1000,
    );

    this.#artworkColor.value = color.rgba;
    this.#artworkLightMode.value = o > 165;

    event.target.style.opacity = "1";
  }

  fullVolume() {
    this.$audio.value?.adjustVolume({ volume: 1 });
  }

  mute() {
    this.$audio.value?.adjustVolume({ volume: 0 });
  }

  next() {
    this.$queue.value?.shift();
  }

  playPause() {
    const audioId = this.$queue.value?.now()?.id;

    if (this.#isPlaying() && audioId) {
      this.$audio.value?.pause({ audioId });
    } else if (audioId) {
      this.$audio.value?.play({ audioId });
    }
  }

  previous() {
    this.$queue.value?.unshift();
  }

  /**
   * @param {MouseEvent} event
   */
  seek(event) {
    const target = event.target
      ? /** @type {HTMLProgressElement} */ (event.target)
      : null;
    const percentage = target ? event.offsetX / target.clientWidth : 0;
    const audioId = this.$queue.value?.now()?.id;

    if (audioId) this.$audio.value?.seek({ audioId, percentage });
  }

  /**
   * @param {MouseEvent} event
   */
  setVolume(event) {
    const target = event.target
      ? /** @type {HTMLProgressElement} */ (event.target)
      : null;

    const percentage = target ? event.offsetX / target.clientWidth : 0;
    this.$audio.value?.adjustVolume({ volume: percentage });
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    const activeQueueItem = this.$queue.value?.now();

    // Artwork
    const artwork = this.#artwork.value.map((art) => {
      const hash = xxh32r(art.bytes).toString();
      const blob = new Blob(
        [/** @type {ArrayBuffer} */ (art.bytes.buffer)],
        { type: art.mime },
      );

      const url = URL.createObjectURL(blob);

      return keyed(
        hash,
        html`
          <img @load="${this.artworkLoaded}" data-hash="${hash}" src="${url}" />
        `,
      );
    });

    return html`
      <style>
      @import "../../../styles/vendor/phosphor/fill/style.css";
      @import "./element.css";
      </style>

      <main style="background-color: ${this.#artworkColor.value ??
        `revert-layer`};">
        <section class="artwork">
          <label style="background: ${this.#artworkColor.value ??
            `revert-layer`}; display: ${`block`};">
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
              `revert-layer`};"
          >
          </div>

          <section class="controller__inner">
            <!-- NOW PLAYING -->

            <cite>
              <strong>${activeQueueItem?.tags?.title ||
                "Diffuse"}</strong>
              <br />
              <span style="font-style: ${activeQueueItem
                ? `normal`
                : `italic`}">
                ${activeQueueItem?.tags?.artist ??
                  (activeQueueItem ? `Waiting on queue ...` : ``)}
              </span>
            </cite>

            <!-- PROGRESS -->

            <div class="progress" @click="${this.seek}">
              <progress max="100" value="${(this.#audio()?.progress() ??
                0) * 100}"></progress>
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
                    !this.#isPlaying()
                  ? `inline`
                  : `none`};"
              >
                <i class="ph-fill ph-play" title="Play"></i>
              </li>

              <!-- pause -->
              <li
                @click="${this.playPause}"
                style="display: ${!this.#isLoading.value && this.#isPlaying()
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

            <footer>
              <i @click="${this.mute}" class="ph-fill ph-speaker-none"></i>
              <div @click="${this.setVolume}" class="progress-bar">
                <progress max="100" value="${(this.$audio.value?.volume() ??
                  0) * 100}"></progress>
              </div>
              <i @click="${this
                .fullVolume}" class="ph-fill ph-speaker-high"></i>
            </footer>
          </section>
        </section>
      </main>
    `;
  }
}

export default ArtworkController;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ArtworkController;
export const NAME = "db-artwork-controller";

customElements.define(NAME, CLASS);
