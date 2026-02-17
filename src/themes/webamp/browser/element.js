import {
  DiffuseElement,
  nothing,
  query,
  whenElementsDefined,
} from "@common/element.js";
import { signal } from "@common/signal.js";
import { highlightTableEntry } from "../common/ui.js";

/**
 * @import {RenderArg} from "@common/element.d.ts"
 * @import {SignalReader} from "@common/signal.d.ts";
 * @import {Track} from "@definitions/types.d.ts"
 * @import {OutputElement} from "@components/output/types.d.ts"
 */

class Browser extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
  }

  // SIGNALS

  $output = signal(
    /** @type {OutputElement | undefined} */ (undefined),
  );

  $queue = signal(
    /** @type {import("@components/engine/queue/element.js").CLASS | undefined} */ (undefined),
  );

  $scope = signal(
    /** @type {import("@components/engine/scope/element.js").CLASS | undefined} */ (undefined),
  );

  $provider = signal(
    /** @type {DiffuseElement & { tracks: SignalReader<Track[]> } | undefined} */ (undefined),
  );

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    /** @type {OutputElement} */
    const output = query(this, "output-selector");

    /** @type {DiffuseElement & { tracks: SignalReader<Track[]> }} */
    const provider = query(this, "tracks-selector");

    /** @type {import("@components/engine/queue/element.js").CLASS} */
    const queue = query(this, "queue-engine-selector");

    /** @type {import("@components/engine/scope/element.js").CLASS} */
    const scope = query(this, "scope-engine-selector");

    // Wait for the above dependencies to be defined, then render again.
    whenElementsDefined({ output, provider, queue, scope }).then(() => {
      this.$output.value = output;
      this.$provider.value = provider;
      this.$queue.value = queue;
      this.$scope.value = scope;
    });

    // Effects
    this.effect(() => {
      const _results = this.$provider.value?.tracks();
      this.root().querySelector(".sunken-panel")?.scrollTo(0, 0);
    });

    this.effect(() => {
      const playlistId = this.$scope.value?.playlistId();
      const select = this.root().querySelector("#playlist-select");

      if (select) {
        /** @type {HTMLSelectElement} */ (select).value = playlistId ?? "";
      }
    });
  }

  // EVENTS

  /**
   * @param {Track} track
   */
  playTrack(track) {
    this.$queue.value?.add({
      inFront: true,
      tracks: [track],
    });

    this.$queue.value?.shift();
  }

  setSearchTerm = () => {
    /** @type {HTMLInputElement | null} */
    const input = this.root().querySelector("#search-input");
    const term = input?.value?.trim();

    this.$scope.value?.setSearchTerm(term);
  };

  /**
   * @param {Event} event
   */
  setSelectedPlaylistId = (event) => {
    const id = /** @type {HTMLSelectElement} */ (event.currentTarget).value;

    this.$scope.value?.setPlaylistId(id === "" ? undefined : id);
  };

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    const isLoading = this.$output.value?.tracks?.state() !== "loaded";
    const tracks = this.$provider.value?.tracks() ?? [];
    const playlistId = this.$scope.value?.playlistId();

    return html`
      <link rel="stylesheet" href="styles/vendor/98.css" />

      <style>
      @import "./themes/webamp/98-vars.css";

      :host {
        display: flex;
        flex-direction: column;
        height: 100%;
      }

      /***********************************
      * SEARCH
      ***********************************/

      search {
        margin-bottom: var(--grouped-button-spacing);
      }

      search input {
        flex: 1;
      }

      search select {
        max-width: 33%;
      }

      /***********************************
      * TABLE
      ***********************************/

      .sunken-panel {
        flex: 1;
        min-height: 80px;
      }

      :host([resizable]) .sunken-panel {
        resize: both;
      }

      table {
        color: var(--text-color);
        table-layout: fixed;
        width: 100%;
      }

      table th {
        width: 30%;

        &:first-child {
          width: 40%;
        }
      }

      table tbody tr {
        cursor: pointer;
        content-visibility: auto;
      }

      table td {
        contain-intrinsic-size: auto 14px;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      </style>

      <search class="field-row">
        <label for="search-input">Search:</label>
        <input id="search-input" type="search" @change="${this
          .setSearchTerm}" />
        <label for="playlist-select">Playlist:</label>
        <select id="playlist-select" @change="${this.setSelectedPlaylistId}">
          <option value="" ?selected="${!playlistId ||
            playlistId === ``}">All tracks</option>
          ${this.$output.value?.playlists.collection().map((p) =>
            html`
              <option
                value="${p.id}"
                ?selected="${p.id === playlistId}"
              >
                ${p.name}
              </option>
            `
          ) ?? nothing}
        </select>
      </search>

      <div class="sunken-panel">
        <table>
          <thead>
            <tr>
              <th>Title</th>
              <th>Artist</th>
              <th>Album</th>
            </tr>
          </thead>
          <tbody>
            ${isLoading
              ? html`
                <tr>
                  <td>Loading ...</td>
                  <td></td>
                  <td></td>
                </tr>
              `
              : tracks.map((track) => {
                return html`
                  <tr @click="${highlightTableEntry}" @dblclick="${() =>
                    this.playTrack(track)}">
                    <td>${track.tags?.title}</td>
                    <td>${track.tags?.artist}</td>
                    <td>${track.tags?.album}</td>
                  </tr>
                `;
              })}
          </tbody>
        </table>
      </div>
    `;
  }
}

export default Browser;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = Browser;
export const NAME = "dtw-browser";

customElements.define(NAME, CLASS);
