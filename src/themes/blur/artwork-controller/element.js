import { FastAverageColor } from "fast-average-color";
import { Temporal } from "@js-temporal/polyfill";
import { xxh32r } from "xxh32/dist/raw.js";
import { debounce } from "throttle-debounce";

import { DiffuseElement, query, whenElementsDefined } from "@common/element.js";
import { trackArtworkCacheId } from "@common/index.js";
import { signal } from "@common/signal.js";

/**
 * @import {RenderArg} from "@common/element.d.ts"
 * @import {Track} from "@definitions/types.d.ts"
 * @import {AudioStateReadOnly} from "@components/engine/audio/types.d.ts"
 * @import {InputElement} from "@components/input/types.d.ts"
 * @import {Artwork} from "@components/processor/artwork/types.d.ts"
 */

class ArtworkController extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
  }

  // SIGNALS

  // #audio = signal(/** @type {AudioStateReadOnly | undefined} */ (undefined));
  #artwork = signal(/** @type {Artwork[]} */ ([]));
  #artworkColor = signal(/** @type {string | undefined} */ (undefined));
  #artworkLightMode = signal(false);
  #duration = signal("0:00");
  // isLoading = signal(true);
  // isPlaying = signal(false);
  // progress = signal(0);
  #time = signal("0:00");
  // volume = signal(0);

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    /** @type {import("@components/processor/artwork/element.js").CLASS} */
    const artwork = query(this, "artwork-processor-selector");

    /** @type {import("@components/engine/audio/element.js").CLASS} */
    const audio = query(this, "audio-engine-selector");

    /** @type {InputElement} */
    const input = query(this, "input-selector");

    /** @type {import("@components/engine/queue/element.js").CLASS} */
    const queue = query(this, "queue-engine-selector");

    this.artwork = artwork;
    this.audio = audio;
    this.input = input;
    this.queue = queue;

    whenElementsDefined({ audio, artwork, input, queue }).then(() => {
      // Changed artwork based on active queue item.
      const debouncedChangeArtwork = debounce(
        1000,
        this.#setArtwork.bind(this),
      );

      this.effect(() => {
        debouncedChangeArtwork(queue.now());
      });

      this.effect(() => {
        const curr = queue.now();
        const aud = curr ? audio.state(curr.id) : undefined;

        console.log("NOW", curr, aud);
      });

      this.effect(() => this.#changeArtworkInDOM());
      this.effect(() => this.#formatTimestamps());
      this.effect(() => this.#lightOrDark());
    });
  }

  ////////////////////////////////////////////
  // ✨ EFFECTS
  // 🖼️ Artwork
  ////////////////////////////////////////////

  /** @type {Record<string, ReturnType<typeof setTimeout>>} */
  #timeouts = {};

  #changeArtworkInDOM() {
    const art = this.#artwork.value;

    // No artwork, fade out existing.
    if (art.length === 0) {
      this.root().querySelectorAll(".artwork img").forEach((el) => {
        const element = /** @type {HTMLElement} */ (el);
        element.style.opacity = "0";
        const hash = element.getAttribute("data-hash");
        if (hash) {
          this.#timeouts[hash] = setTimeout(() => element.remove(), 1000);
        }
      });
      return;
    }

    // Determine if the current artwork needs to be replaced.
    const hash = xxh32r(art[0].bytes).toString();

    /** @type {HTMLImageElement | null} */
    const existingArtwork = this.root().querySelector(
      `.artwork img[data-hash="${hash}"]`,
    );

    // If the artwork is the same, stop here.
    if (existingArtwork) {
      const timeoutId = this.#timeouts[hash];
      if (timeoutId) clearTimeout(timeoutId);
      existingArtwork.style.opacity = "1";
      return;
    }

    // Add new artwork
    const blob = new Blob(
      [/** @type {ArrayBuffer} */ (art[0].bytes.buffer)],
      { type: art[0].mime },
    );
    const url = URL.createObjectURL(blob);

    /** @type {HTMLImageElement} */
    const img = document.createElement("img");
    img.setAttribute("data-hash", hash);
    img.src = url;

    // Extract average color
    img.onload = () => {
      const fac = new FastAverageColor();
      const color = fac.getColor(img);
      const rgb = color.value;
      const o = Math.round(
        (rgb[0] * 299 + rgb[1] * 587 + rgb[2] * 114) / 1000,
      );

      this.#artworkColor.value = color.rgba;
      this.#artworkLightMode.value = o > 165;

      /** @type {HTMLElement | null} */
      const bg = this.root().querySelector(".controller__background");
      if (bg) bg.style.backgroundColor = color.rgba;

      /** @type {HTMLElement | null} */
      const main = this.root().querySelector("main");
      if (main) main.style.backgroundColor = color.rgba;

      img.style.opacity = "1";

      this.root().querySelectorAll(".artwork img").forEach((el) => {
        if (el === img) return;

        const element = /** @type {HTMLElement} */ (el);
        element.style.opacity = "0";
        this.#timeouts[hash] = setTimeout(() => element.remove(), 1000);
      });
    };

    // Insert new artwork
    this.root().querySelector(".artwork")?.appendChild(img);
  }

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
    if (!track) {
      this.#artwork.value = [];
      return;
    }

    const cacheId = await trackArtworkCacheId(track);

    const resGet = await this.input?.resolve({ method: "GET", uri: track.uri });
    const resHead = await this.input?.resolve({
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

    const art = await this.artwork?.artwork(request) ?? [];

    console.log("ART", art);

    const currCacheId = track ? await trackArtworkCacheId(track) : undefined;
    if (cacheId === currCacheId) this.#artwork.set(art);
  }

  ////////////////////////////////////////////
  // ✨ EFFECTS
  // ⌚️ Time
  ////////////////////////////////////////////
  #formatTimestamps() {
    const curr = this.queue?.now?.() ?? undefined;
    const audio = curr ? this.audio?.state(curr.id) : undefined;
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
   * @param {MouseEvent} event
   */
  seek(event) {
    const target = event.target
      ? /** @type {HTMLProgressElement} */ (event.target)
      : null;
    const percentage = target ? event.offsetX / target.clientWidth : 0;
    const audioId = this.queue?.now()?.id;

    if (audioId) this.audio?.seek({ audioId, percentage });
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    return html`
      <style>
      @import "../../../styles/vendor/phosphor/fill/style.css";
      @import "./element.css";
      </style>

      <main>
        <section class="artwork"></section>

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

          <div class="controller__background"></div>

          <section class="controller__inner">
            <!-- Now playing -->
            <cite>
              <strong>Diffuse</strong>
              <br />
              <span></span>
            </cite>

            <!-- Progress -->
            <div class="progress">
              <progress max="100" value="${0}"></progress>
              <div class="timestamps">
                <time datetime="${this.#time.value}">${this.#time.value}</time>
                <time datetime="${this.#time.value}">${this.#duration
                  .value}</time>
              </div>
            </div>

            <!-- Controls -->
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
