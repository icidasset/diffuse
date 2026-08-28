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
 * @import FavouritesOrchestrator from "~/components/orchestrator/favourites/element.js"
 */

class TrackDetailsElement extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
  }

  // SIGNALS - DEPENDENCIES

  $controller = signal(/** @type {ControllerOrchestrator | undefined} */ (undefined));
  $favourites = signal(/** @type {FavouritesOrchestrator | undefined} */ (undefined));

  // COMPUTED

  currentTrack = () => this.$controller.value?.currentTrack();

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    /** @type {ControllerOrchestrator} */
    const controller = query(this, "controller-orchestrator-selector");

    /** @type {FavouritesOrchestrator} */
    const favourites = query(this, "favourites-orchestrator-selector");

    whenElementsDefined({ controller, favourites }).then(() => {
      this.$controller.value = controller;
      this.$favourites.value = favourites;
    });
  }

  // ACTIONS

  toggleFavourite = () => {
    const track = this.currentTrack();
    if (track) this.$favourites.value?.toggle(track);
  };

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    const track = this.currentTrack();
    const isFav = track ? (this.$favourites.value?.isFavourite(track) ?? false) : false;

    const title = track?.tags?.title ?? "—";
    const artist = track?.tags?.artist ?? "—";
    const album = track?.tags?.album ?? "—";
    const year = track?.tags?.year ?? null;

    const durationMs = track?.stats?.duration ?? null;
    const duration = durationMs != null ? formatDuration(durationMs) : "—";

    const bitrate = track?.stats?.bitrate != null
      ? `${Math.round(track.stats.bitrate / 1000)} kbps`
      : "—";

    const sampleRate = track?.stats?.sampleRate != null
      ? `${Math.round(track.stats.sampleRate / 1000)} kHz`
      : "—";

    const codec = track?.stats?.codec ?? "—";
    const channels = track?.stats?.numberOfChannels ?? null;

    return html`
      <link rel="stylesheet" href="vendor/98.css" />
      <link rel="stylesheet" href="facets/themes/winamp/98-scrollbar.css" />

      <style>
        @import "./facets/themes/winamp/98-vars.css";

        :host {
          display: block;
          height: 100%;
          width: 100%;
        }

        .window-body {
          display: flex;
          flex-direction: column;
          gap: var(--grouped-element-spacing);
          height: calc(100% - 16px);
          overflow-y: auto;
        }

        .empty {
          align-items: center;
          color: var(--button-shadow);
          display: flex;
          height: 100%;
          justify-content: center;
        }

        .row {
          display: flex;
          gap: 4px;
          line-height: 16px;
        }

        .label {
          color: var(--button-shadow);
          flex: 0 0 70px;
        }

        .value {
          flex: 1;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }

        button.fav {
          color: #800000;
        }
      </style>

      <div class="window-body">
        ${track
          ? html`
          <fieldset>
            <legend>Track</legend>
            <div class="row">
              <span class="label">Title</span>
              <span class="value" title="${title}">${title}</span>
            </div>
            <div class="row">
              <span class="label">Artist</span>
              <span class="value" title="${artist}">${artist}</span>
            </div>
            <div class="row">
              <span class="label">Album</span>
              <span class="value" title="${album}">${album}</span>
            </div>
            ${year != null
              ? html`<div class="row">
                  <span class="label">Year</span>
                  <span class="value">${year}</span>
                </div>`
              : html``}
          </fieldset>

          <fieldset>
            <legend>Audio</legend>
            <div class="row">
              <span class="label">Duration</span>
              <span class="value">${duration}</span>
            </div>
            <div class="row">
              <span class="label">Bitrate</span>
              <span class="value">${bitrate}</span>
            </div>
            <div class="row">
              <span class="label">Sample rate</span>
              <span class="value">${sampleRate}</span>
            </div>
            <div class="row">
              <span class="label">Codec</span>
              <span class="value">${codec}</span>
            </div>
            ${channels != null
              ? html`<div class="row">
                  <span class="label">Channels</span>
                  <span class="value">${channels}</span>
                </div>`
              : html``}
          </fieldset>

          <button
            class="${isFav ? "fav" : ""}"
            @click="${this.toggleFavourite}"
          >${isFav ? "★ Unfavourite" : "☆ Favourite"}</button>
        `
          : html`<div class="empty">No track playing</div>`}
      </div>
    `;
  }
}

export default TrackDetailsElement;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = TrackDetailsElement;
export const NAME = "dtw-winamp-track-details";

defineElement(NAME, TrackDetailsElement);

/**
 * @param {number} ms
 * @returns {string}
 */
function formatDuration(ms) {
  const totalSeconds = Math.floor(ms / 1000);
  const minutes = Math.floor(totalSeconds / 60);
  const seconds = totalSeconds % 60;
  return `${minutes}:${seconds.toString().padStart(2, "0")}`;
}
