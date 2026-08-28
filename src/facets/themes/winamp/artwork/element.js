import {
  defineElement,
  DiffuseElement,
  query,
  whenElementsDefined,
} from "~/common/element.js";
import { signal, untracked } from "~/common/signal.js";

/**
 * @import {RenderArg} from "~/common/element.d.ts"
 *
 * @import ArtworkOrchestrator from "~/components/orchestrator/artwork/element.js"
 * @import ControllerOrchestrator from "~/components/orchestrator/controller/element.js"
 */

class ArtworkWindowElement extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
  }

  // SIGNALS

  #artworkUrl = signal(/** @type {string | null} */ (null));

  /** @type {string | null} */
  #prevUrl = null;

  // SIGNALS - DEPENDENCIES

  $artwork = signal(/** @type {ArtworkOrchestrator | undefined} */ (undefined));
  $controller = signal(
    /** @type {ControllerOrchestrator | undefined} */ (undefined),
  );

  // COMPUTED

  currentTrack = () => this.$controller.value?.currentTrack();

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

    whenElementsDefined({ artwork, controller }).then(() => {
      this.$artwork.value = artwork;
      this.$controller.value = controller;

      this.effect(() => {
        const track = this.currentTrack();
        untracked(() => this.#fetchArtwork(track));
      });
    });
  }

  /**
   * @override
   */
  disconnectedCallback() {
    if (this.#prevUrl) {
      URL.revokeObjectURL(this.#prevUrl);
      this.#prevUrl = null;
    }
  }

  /**
   * @param {ReturnType<ControllerOrchestrator["currentTrack"]>} track
   */
  async #fetchArtwork(track) {
    const bytes = track
      ? ((await this.$artwork.value?.get(track)) ?? null)
      : null;

    if (this.$controller.value?.$queue.value?.now()?.id !== track?.id) return;

    if (this.#prevUrl) {
      URL.revokeObjectURL(this.#prevUrl);
      this.#prevUrl = null;
    }

    if (bytes) {
      const mime = detectMime(bytes);
      const url = URL.createObjectURL(
        new Blob([/** @type {ArrayBuffer} */ (bytes.buffer)], { type: mime }),
      );
      this.#prevUrl = url;
      this.#artworkUrl.value = url;
    } else {
      this.#artworkUrl.value = null;
    }
  }

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    const url = this.#artworkUrl.value;

    return html`
      <style>
        :host {
          background: #000;
          display: block;
          height: 100%;
          width: 100%;
        }

        .art {
          align-items: center;
          background: #000;
          display: flex;
          height: 100%;
          justify-content: center;
          min-height: 240px;
          width: 100%;
        }

        img {
          display: block;
          max-height: 100%;
          max-width: 100%;
          object-fit: contain;
        }

        .no-artwork {
          color: #808080;
          font-family: "Pixelated MS Sans Serif", sans-serif;
          font-size: 11px;
        }
      </style>

      <div class="art">
        ${url
          ? html`<img src="${url}" decoding="async" alt="" />`
          : html`<span class="no-artwork">No artwork</span>`}
      </div>
    `;
  }
}

export default ArtworkWindowElement;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ArtworkWindowElement;
export const NAME = "dtw-winamp-artwork";

defineElement(NAME, ArtworkWindowElement);

/**
 * @param {Uint8Array} bytes
 * @returns {string}
 */
function detectMime(bytes) {
  if (bytes[0] === 0xff && bytes[1] === 0xd8) return "image/jpeg";
  if (bytes[0] === 0x89 && bytes[1] === 0x50) return "image/png";
  if (bytes[0] === 0x47 && bytes[1] === 0x49) return "image/gif";
  if (bytes[0] === 0x52 && bytes[1] === 0x49) return "image/webp";
  return "image/jpeg";
}
