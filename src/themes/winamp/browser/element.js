import {
  defineElement,
  DiffuseElement,
  query,
  whenElementsDefined,
} from "~/common/element.js";
import { computed, signal, untracked } from "~/common/signal.js";
import * as Playlist from "~/common/playlist.js";

/**
 * @import {RenderArg} from "~/common/element.d.ts"
 * @import {SignalReader} from "~/common/signal.d.ts";
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {OutputElement} from "~/components/output/types.d.ts"
 */

const ROW_HEIGHT = 14;
const OVERSCAN = 20;

/** @type {Record<string, string[]>} */
const COLUMN_SORT = {
  title: ["tags.title"],
  artist: ["tags.artist", "tags.album", "tags.disc.no", "tags.track.no"],
  album: ["tags.album", "tags.disc.no", "tags.track.no"],
};

class Browser extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
  }

  // SIGNALS

  $output = signal(
    /** @type {OutputElement | undefined} */ (undefined),
  );

  $provider = signal(
    /** @type {DiffuseElement & { tracks: SignalReader<Track[]> } | undefined} */ (undefined),
  );

  $queue = signal(
    /** @type {import("~/components/engine/queue/element.js").CLASS | undefined} */ (undefined),
  );

  $scope = signal(
    /** @type {import("~/components/engine/scope/element.js").CLASS | undefined} */ (undefined),
  );

  $highlightedTrack = signal(/** @type {string | null} */ (null));

  $groupedPlaylists = computed(() => {
    const col = this.$output.value?.playlistItems.collection();
    if (!col || col.state !== "loaded" || !col.data.length) return [];
    const items = col.data;

    // Group items by playlist name
    /** @type {Map<string, { name: string, unordered: boolean }>} */
    const playlistMap = Playlist.gather(items);

    const all = [...playlistMap.values()].sort((a, b) =>
      a.name.localeCompare(b.name)
    );

    const ordered = all.filter((p) => !p.unordered);
    const unordered = all.filter((p) => p.unordered);

    return [
      { label: "Ordered", playlists: ordered },
      { label: "Unordered", playlists: unordered },
    ].filter((g) => g.playlists.length > 0);
  });

  // STATE

  #scrollTop = 0;
  #viewportHeight = 0;
  #renderedStartIndex = -1;
  #renderedEndIndex = -1;

  /** @type {ResizeObserver | undefined} */
  #resizeObserver;

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

    /** @type {import("~/components/engine/queue/element.js").CLASS} */
    const queue = query(this, "queue-engine-selector");

    /** @type {import("~/components/engine/scope/element.js").CLASS} */
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

      untracked(() => {
        const panel = this.root().querySelector(".sunken-panel");
        if (panel) {
          panel.scrollTo(0, 0);
          this.#scrollTop = 0;
        }
      });
    });

    // Scroll & resize tracking (set up once after first render)
    this.#setupScrollTracking();

    this.effect(() => {
      const playlist = this.$scope.value?.playlist();
      const select = this.root().querySelector("#playlist-select");

      if (select) {
        /** @type {HTMLSelectElement} */ (select).value = playlist ?? "";
      }
    });
  }

  /**
   * @override
   */
  disconnectedCallback() {
    super.disconnectedCallback();
    this.#resizeObserver?.disconnect();
  }

  // SCROLL

  #setupScrollTracking() {
    requestAnimationFrame(() => {
      const panel = this.root().querySelector(".sunken-panel");
      if (!panel) return;

      panel.addEventListener(
        "scroll",
        () => {
          this.#scrollTop = panel.scrollTop;
          this.#renderIfWindowChanged(panel);
        },
        { passive: true },
      );

      this.#resizeObserver = new ResizeObserver((entries) => {
        this.#viewportHeight = entries[0].contentRect.height;
        this.#renderIfWindowChanged(panel);
      });

      this.#resizeObserver.observe(panel);
    });
  }

  #computeWindow() {
    const startIndex = Math.max(
      0,
      Math.floor(this.#scrollTop / ROW_HEIGHT) - OVERSCAN,
    );
    const visibleCount = Math.ceil(this.#viewportHeight / ROW_HEIGHT) +
      2 * OVERSCAN;

    return { startIndex, endIndex: startIndex + visibleCount };
  }

  /**
   * @param {Element} panel
   */
  #renderIfWindowChanged(panel) {
    const { startIndex, endIndex } = this.#computeWindow();

    if (
      startIndex === this.#renderedStartIndex &&
      endIndex === this.#renderedEndIndex
    ) {
      return;
    }

    const scrollTop = panel.scrollTop;
    this.forceRender();
    panel.scrollTop = scrollTop;
  }

  // EVENTS

  /**
   * @param {Track} track
   */
  playTrack(track) {
    this.$queue.value?.add({
      inFront: true,
      trackIds: [track.id],
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
  setSelectedPlaylist = (event) => {
    const value = /** @type {HTMLSelectElement} */ (event.currentTarget).value;

    this.$scope.value?.setPlaylist(value === "" ? undefined : value);
  };

  /**
   * @param {string} column
   */
  sortByColumn = (column) => {
    const scope = this.$scope.value;
    if (!scope) return;

    const isActive = JSON.stringify(COLUMN_SORT[column]) ===
      JSON.stringify(scope.sortBy());

    if (isActive) {
      if (scope.sortDirection() === "desc") {
        scope.revertToDefaultSort();
      } else {
        scope.setSortDirection("desc");
      }
    } else {
      scope.setSortBy(COLUMN_SORT[column] ?? []);
      scope.setSortDirection(undefined);
    }
  };

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    const highlighted = this.$highlightedTrack.value;
    const isLoading =
      this.$output.value?.tracks?.collection().state !== "loaded";

    const tracks = this.$provider.value?.tracks() ?? [];
    const playlist = this.$scope.value?.playlist();
    const searchTerm = this.$scope.value?.searchTerm() ?? "";
    const sortBy = this.$scope.value?.sortBy() ?? [];
    const sortDirection = this.$scope.value?.sortDirection();
    const sortedColumn = Object.entries(COLUMN_SORT).find(
      ([, v]) => JSON.stringify(v) === JSON.stringify(sortBy),
    )?.[0];

    const ariaSort = /** @param {string} col */ (col) =>
      sortedColumn === col
        ? (sortDirection === "desc" ? "descending" : "ascending")
        : "none";

    // Virtual list
    const totalTracks = tracks.length;
    const { startIndex, endIndex: rawEnd } = this.#computeWindow();
    const endIndex = Math.min(totalTracks, rawEnd);

    this.#renderedStartIndex = startIndex;
    this.#renderedEndIndex = endIndex;

    const visibleTracks = tracks.slice(startIndex, endIndex);
    const totalHeight = totalTracks * ROW_HEIGHT;
    const topPad = startIndex * ROW_HEIGHT;

    return html`
      <link rel="stylesheet" href="vendor/98.css" />

      <style>
      @import "./themes/winamp/98-vars.css";

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
        color: inherit;
        flex: 1;
      }

      search select {
        color: inherit;
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

      .virtual-header {
        position: sticky;
        top: 0;
        z-index: 1;
      }

      table {
        color: var(--text-color);
        table-layout: fixed;
        width: 100%;
      }

      table th {
        cursor: pointer;
        user-select: none;
        width: 30%;

        &:after {
          font-size: 82%;
          position: absolute;
          right: 6px;
        }

        &:first-child {
          width: 40%;
        }
      }

      table th[aria-sort="ascending"]::after {
        content: " ▲";
      }

      table th[aria-sort="descending"]::after {
        content: " ▼";
      }

      .virtual-scroll table {
        will-change: transform;
      }

      table tbody tr {
        cursor: pointer;
      }

      table td {
        overflow: hidden;
        text-overflow: ellipsis;
      }
      </style>

      <search class="field-row">
        <label for="search-input">Search:</label>
        <input
          id="search-input"
          type="search"
          @change="${this
            .setSearchTerm}"
          .value="${searchTerm}"
        />
        <label for="playlist-select">Playlist:</label>
        <select id="playlist-select" @change="${this.setSelectedPlaylist}">
          <option value="" ?selected="${!playlist ||
            playlist === ``}">All tracks</option>
          ${this.$groupedPlaylists().map((group) =>
            html`
              <optgroup label="${group.label}">
                ${group.playlists.map((p) =>
                  html`
                    <option
                      value="${p.name}"
                      ?selected="${p.name === playlist}"
                    >
                      ${p.name}
                    </option>
                  `
                )}
              </optgroup>
            `
          )}
        </select>
      </search>

      <div class="sunken-panel">
        <table class="virtual-header">
          <thead>
            <tr>
              <th
                aria-sort="${ariaSort(`title`)}"
                @click="${() => this.sortByColumn(`title`)}"
              >
                Title
              </th>
              <th
                aria-sort="${ariaSort(`artist`)}"
                @click="${() => this.sortByColumn(`artist`)}"
              >
                Artist
              </th>
              <th
                aria-sort="${ariaSort(`album`)}"
                @click="${() => this.sortByColumn(`album`)}"
              >
                Album
              </th>
            </tr>
          </thead>
        </table>
        <div class="virtual-scroll" style="height:${totalHeight}px">
          <table style="transform: translateY(${topPad}px)">
            <colgroup>
              <col style="width:40%">
              <col style="width:30%">
              <col style="width:30%">
            </colgroup>
            <tbody>
              ${isLoading
                ? html`
                  <tr>
                    <td>Loading ...</td>
                    <td></td>
                    <td></td>
                  </tr>
                `
                : visibleTracks.map((track) =>
                  html`
                    <tr
                      class="${highlighted === track.id ? `highlighted` : ``}"
                      @click="${() => this.$highlightedTrack.value = track.id}"
                      @dblclick="${() => this.playTrack(track)}"
                    >
                      <td>${track.tags?.title}</td>
                      <td>${track.tags?.artist}</td>
                      <td>${track.tags?.album}</td>
                    </tr>
                  `
                )}
            </tbody>
          </table>
        </div>
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

defineElement(NAME, CLASS);
