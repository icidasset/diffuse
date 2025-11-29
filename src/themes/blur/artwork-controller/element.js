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
 * @import {InputElement} from "@components/input/types.d.ts"
 * @import {OutputElement} from "@components/output/types.d.ts"
 * @import {Artwork} from "@components/processor/artwork/types.d.ts"
 */

class ArtworkController extends DiffuseElement {
  // constructor() {
  //   super();
  //   this.attachShadow({ mode: "open" });
  // }

  // SIGNALS

  // activeTrack = signal(/** @type {Track | undefined} */ (undefined));
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
        this.#changeArtwork.bind(this),
      );

      this.effect(() => {
        debouncedChangeArtwork(queue.now());
      });

      this.effect(() => {
        const trigger = queue.now();
        const _other_trigger = queue.poolHash();

        queue.fill({ amount: 10, shuffled: true });
        if (!trigger) queue.shift();
      });

      // Force render when elements are defined

      // this.effect(() => {
      //   this.forceRender();
      // });
    });

    this.#artworkEffects();
  }

  // EFFECTS

  /**
   * @param {Track | null} track
   */
  async #changeArtwork(track) {
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
    const currCacheId = track ? await trackArtworkCacheId(track) : undefined;
    if (cacheId === currCacheId) this.#artwork.set(art);
  }

  #artworkEffects() {
    /** @type {Record<string, ReturnType<typeof setTimeout>>} */
    const timeouts = {};

    this.effect(() => {
      const art = this.#artwork.value;

      // No artwork, fade out existing.
      if (art.length === 0) {
        this.querySelectorAll(":scope .artwork img").forEach((el) => {
          const element = /** @type {HTMLElement} */ (el);
          element.style.opacity = "0";
          const hash = element.getAttribute("data-hash");
          if (hash) timeouts[hash] = setTimeout(() => element.remove(), 1000);
        });
        return;
      }

      // Determine if the current artwork needs to be replaced.
      const hash = xxh32r(art[0].bytes).toString();

      /** @type {HTMLImageElement | null} */
      const existingArtwork = this.querySelector(
        `:scope .artwork img[data-hash="${hash}"]`,
      );

      // If the artwork is the same, stop here.
      if (existingArtwork) {
        const timeoutId = timeouts[hash];
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
        const bg = this.querySelector(":scope .controller__background");
        if (bg) bg.style.backgroundColor = color.rgba;

        /** @type {HTMLElement | null} */
        const main = this.querySelector(":scope main");
        if (main) main.style.backgroundColor = color.rgba;

        img.style.opacity = "1";

        this.querySelectorAll(":scope .artwork img").forEach((el) => {
          if (el === img) return;

          const element = /** @type {HTMLElement} */ (el);
          element.style.opacity = "0";
          timeouts[hash] = setTimeout(() => element.remove(), 1000);
        });
      };

      // Insert new artwork
      this.querySelector(":scope .artwork")?.appendChild(img);
    });

    this.effect(() => {
      // if (artworkLightMode()) {
      //   controller.classList.add("controller__inner--light-mode");
      // } else controller.classList.remove("controller__inner--light-mode");
    });
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    return html`
      <style>
      /*@import "../../../styles/icons/phosphor.css";*/
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

          <!-- Content -->
          <section class="controller__inner"></section>
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
