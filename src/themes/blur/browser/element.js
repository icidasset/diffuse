import {
  defineElement,
  DiffuseElement,
  query,
  whenElementsDefined,
} from "~/common/element.js";
import { computed, signal, untracked } from "~/common/signal.js";
import * as Playlist from "~/common/playlist.js";
import {
  elementScroll,
  observeElementOffset,
  observeElementRect,
  Virtualizer,
} from "@tanstack/virtual-core";

/**
 * @import {RenderArg} from "~/common/element.d.ts"
 * @import {SignalReader} from "~/common/signal.d.ts";
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {OutputElement} from "~/components/output/types.d.ts"
 */

const TRACK_ROW_HEIGHT = 44;
const GROUP_HEADER_HEIGHT = 52;
const OVERSCAN = 10;

/** @type {Record<string, string[]>} */
const COLUMN_SORT = {
  title: ["tags.title"],
  artist: ["tags.artist", "tags.album", "tags.disc.no", "tags.track.no"],
  album: ["tags.album", "tags.disc.no", "tags.track.no"],
};

const DEFAULT_SORT = ["createdAt"];

const GROUP_BY_OPTIONS = [
  { value: "firstLetter", label: "Group by first letter", icon: "ph-text-aa", sortBy: null, sortDirection: /** @type {"asc" | "desc" | undefined} */ (undefined) },
  { value: "directory", label: "Group by path", icon: "ph-folder", sortBy: ["uri"], sortDirection: /** @type {"asc" | "desc" | undefined} */ (undefined) },
  { value: "createdAt", label: "Group by processing date", icon: "ph-clock", sortBy: ["createdAt"], sortDirection: /** @type {"asc" | "desc" | undefined} */ ("desc") },
  { value: "tags.year", label: "Group by track year", icon: "ph-calendar", sortBy: ["tags.year"], sortDirection: /** @type {"asc" | "desc" | undefined} */ ("desc") },
];

/**
 * @typedef {{ type: "group"; label: string }} GroupItem
 * @typedef {{ type: "track"; track: Track }} TrackItem
 * @typedef {GroupItem | TrackItem} VirtualItem
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

  $provider = signal(
    /** @type {DiffuseElement & { tracks: SignalReader<Track[]> } | undefined} */ (undefined),
  );

  $queue = signal(
    /** @type {import("~/components/engine/queue/element.js").CLASS | undefined} */ (undefined),
  );

  $scope = signal(
    /** @type {import("~/components/engine/scope/element.js").CLASS | undefined} */ (undefined),
  );

  $favourites = signal(
    /** @type {import("~/components/orchestrator/favourites/element.js").CLASS | undefined} */ (undefined),
  );

  $groupedPlaylists = computed(() => {
    const col = this.$output.value?.playlistItems.collection();
    if (!col || col.state !== "loaded" || !col.data.length) return [];
    const items = col.data;

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

  /** @type {VirtualItem[]} */
  #flatItems = [];

  /** @type {Track[] | undefined} */
  #lastTracks = undefined;

  /** @type {{ label: string; tracks: Track[] }[] | undefined} */
  #lastGroups = undefined;

  /** @type {Virtualizer<Element, Element> | undefined} */
  #virtualizer;

  /** @type {(() => void) | undefined} */
  #virtualizerCleanup;

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

    /** @type {import("~/components/orchestrator/favourites/element.js").CLASS} */
    const favourites = query(this, "favourites-orchestrator-selector");

    whenElementsDefined({ output, provider, queue, scope, favourites }).then(() => {
      this.$output.value = output;
      this.$provider.value = provider;
      this.$queue.value = queue;
      this.$scope.value = scope;
      this.$favourites.value = favourites;
    });

    // Reset scroll when track list changes
    this.effect(() => {
      const _results = this.$provider.value?.tracks();

      untracked(() => {
        const panel = this.root().querySelector(".scroll-panel");
        if (panel) panel.scrollTo(0, 0);
      });
    });

    // Set up the virtualizer after the first render, when .scroll-panel exists in the DOM.
    // This mirrors the winamp browser's #setupScrollTracking pattern.
    requestAnimationFrame(() => {
      const panel = this.root().querySelector(".scroll-panel");
      if (!panel) return;

      this.#virtualizer = new Virtualizer({
        count: 0,
        getScrollElement: () => panel,
        estimateSize: (i) =>
          this.#flatItems[i]?.type === "group"
            ? GROUP_HEADER_HEIGHT
            : TRACK_ROW_HEIGHT,
        overscan: OVERSCAN,
        observeElementRect,
        observeElementOffset,
        scrollToFn: elementScroll,
        onChange: () => {
          requestAnimationFrame(() => this.forceRender());
        },
      });

      this.#virtualizerCleanup = this.#virtualizer._didMount();
      this.#virtualizer._willUpdate();

      // Render now that the virtualizer is wired up
      this.forceRender();
    });
  }

  /**
   * @override
   */
  disconnectedCallback() {
    super.disconnectedCallback();
    this.#virtualizerCleanup?.();
    this.#virtualizerCleanup = undefined;
    this.#virtualizer = undefined;
  }

  // EVENTS

  /**
   * @param {Track} track
   */
  playTrack(track) {
    this.$queue.value?.add({ inFront: true, trackIds: [track.id] });
    this.$queue.value?.shift();
  }

  setSearchTerm = () => {
    /** @type {HTMLInputElement | null} */
    const input = this.root().querySelector("#search-input");
    const term = input?.value?.trim();
    this.$scope.value?.setSearchTerm(term);
  };

  /**
   * @param {string | undefined} value
   */
  setGroupBy = (value) => {
    this.$scope.value?.setGroupBy(value);
  };

  /**
   * @param {string | undefined} value
   */
  setSelectedPlaylist = (value) => {
    this.$scope.value?.setPlaylist(value);
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

  /**
   * @param {Track} track
   */
  toggleFavourite = (track) => {
    this.$favourites.value?.toggle(track);
  };

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    const isLoading =
      this.$output.value?.tracks?.collection().state !== "loaded";

    const tracks = this.$provider.value?.tracks() ?? [];
    const playlist = this.$scope.value?.playlist();
    const groupBy = this.$scope.value?.groupBy();
    const searchTerm = this.$scope.value?.searchTerm() ?? "";
    const sortBy = this.$scope.value?.sortBy() ?? DEFAULT_SORT;
    const sortDirection = this.$scope.value?.sortDirection();

    const sortedColumn = Object.entries(COLUMN_SORT).find(
      ([, v]) => JSON.stringify(v) === JSON.stringify(sortBy),
    )?.[0];

    const ariaSort = /** @param {string} col */ (col) =>
      sortedColumn === col
        ? (sortDirection === "desc" ? "descending" : "ascending")
        : "none";

    const groups = /** @type {any} */ (this.$provider.value)?.groups?.();

    // Rebuild flat items only when data reference changes
    if (groups !== this.#lastGroups || tracks !== this.#lastTracks) {
      this.#flatItems = groups ? buildFlatList(groups) : [];
      this.#lastGroups = groups;
      this.#lastTracks = tracks;
    }

    const count = groups ? this.#flatItems.length : tracks.length;

    // Update virtualizer count whenever data changes.
    // The virtualizer is set up in connectedCallback after first render;
    // until then virtualItems is empty and totalSize is 0.
    if (this.#virtualizer) {
      this.#virtualizer.setOptions({
        ...this.#virtualizer.options,
        count,
      });
      this.#virtualizer._willUpdate();
    }

    const virtualItems = this.#virtualizer?.getVirtualItems() ?? [];
    const totalSize = this.#virtualizer?.getTotalSize() ?? 0;

    // Build O(1) favourite lookup — one subscription regardless of visible row count
    const favItems = this.$favourites.value?.playlistItems() ?? [];
    const favSet = new Set(
      favItems.map((item) => {
        const a = item.criteria.find((c) => c.field === "tags.artist");
        const t = item.criteria.find((c) => c.field === "tags.title");
        return `${String(a?.value ?? "").toLowerCase()}|${String(t?.value ?? "").toLowerCase()}`;
      }),
    );

    /**
     * @param {Track} track
     * @param {number} top
     */
    const renderTrackRow = (track, top) => {
      const key = `${String(track.tags?.artist ?? "").toLowerCase()}|${String(track.tags?.title ?? "").toLowerCase()}`;
      const isFav = favSet.has(key);
      return html`
        <div
          class="track-row"
          style="transform: translateY(${top}px);"
          @dblclick="${() => this.playTrack(track)}"
        >
          <div class="col-fav">
            <button
              class="fav-btn ${isFav ? `fav-btn--active` : ``}"
              @click="${(/** @type {Event} */ e) => { e.stopPropagation(); this.toggleFavourite(track); }}"
              title="${isFav ? `Remove from favourites` : `Add to favourites`}"
            >
              <i class="ph-${isFav ? `fill ph-heart` : `bold ph-heart`}"></i>
            </button>
          </div>
          <div class="col-title">
            <span class="track-title">${track.tags?.title}</span>
          </div>
          <div class="col-artist">
            <span>${track.tags?.artist}</span>
          </div>
          <div class="col-album">
            <span>${track.tags?.album}</span>
          </div>
        </div>
      `;
    };

    return html`
      <link rel="stylesheet" href="styles/base.css" />
      <link rel="stylesheet" href="styles/diffuse/facet.css" />
      <link rel="stylesheet" href="vendor/@phosphor-icons/web/bold/style.css" />
      <link rel="stylesheet" href="vendor/@phosphor-icons/web/fill/style.css" />
      <link rel="stylesheet" href="themes/blur/browser/element.css" />

      <div class="toolbar">
        <label class="search-field">
          <i class="ph-bold ph-magnifying-glass"></i>
          <input
            id="search-input"
            type="search"
            placeholder="Search"
            @change="${this.setSearchTerm}"
            .value="${searchTerm}"
          />
        </label>

        <div class="toolbar-actions">
          <button class="toolbar-icon-btn" popovertarget="groupby-menu" title="Group by">
            <i class="ph-fill ph-intersect-three"></i>
          </button>
          <div id="groupby-menu" class="dropdown" popover>
            ${GROUP_BY_OPTIONS.map(({ value, label, icon, sortBy: optSortBy, sortDirection: optSortDirection }) => {
              // "firstLetter" is dynamic — its stored value includes the sort field
              const primarySortField = sortBy[0] ?? "tags.title";
              const resolvedValue = value === "firstLetter"
                ? `firstLetter:${primarySortField}`
                : value;
              const isActive = value === "firstLetter"
                ? groupBy?.startsWith("firstLetter:")
                : groupBy === value;
              return html`
                <button
                  class="${isActive ? `dropdown-item--active` : ``}"
                  @click="${() => {
                    const scope = this.$scope.value;
                    if (isActive) {
                      this.setGroupBy(undefined);
                    } else {
                      this.setGroupBy(resolvedValue);
                      if (optSortBy && scope) {
                        scope.setSortBy(optSortBy);
                        scope.setSortDirection(optSortDirection);
                      }
                    }
                    /** @type {HTMLElement | null} */ (this.root().querySelector(`#groupby-menu`))?.hidePopover();
                  }}"
                >
                  <i class="ph-${isActive ? `bold ph-x` : `fill ${icon}`}"></i>
                  ${label}
                </button>
              `;
            })}
          </div>
          <button class="playlist-btn" popovertarget="playlist-menu">
            <i class="ph-fill ph-playlist"></i>
            ${playlist ?? `All tracks`}
          </button>
          <div id="playlist-menu" class="dropdown" popover>
            <button @click="${() => { this.setSelectedPlaylist(undefined); /** @type {HTMLElement | null} */ (this.root().querySelector(`#playlist-menu`))?.hidePopover(); }}">
              All tracks
            </button>
            ${this.$groupedPlaylists().map((group) =>
              group.playlists.map((p) =>
                html`
                  <button @click="${() => { this.setSelectedPlaylist(p.name); /** @type {HTMLElement | null} */ (this.root().querySelector(`#playlist-menu`))?.hidePopover(); }}">
                    ${p.name}
                  </button>
                `
              )
            )}
          </div>
        </div>
      </div>

      <div class="table-header">
        <div class="col-fav"></div>
        <div
          class="col-title ${sortedColumn === `title` ? `col--sorted` : ``}"
          @click="${() => this.sortByColumn(`title`)}"
          aria-sort="${ariaSort(`title`)}"
        >
          Title
          ${sortedColumn === `title`
            ? html`<i class="ph-bold ${sortDirection === `desc` ? `ph-caret-down` : `ph-caret-up`}"></i>`
            : ``}
        </div>
        <div
          class="col-artist ${sortedColumn === `artist` ? `col--sorted` : ``}"
          @click="${() => this.sortByColumn(`artist`)}"
          aria-sort="${ariaSort(`artist`)}"
        >
          Artist
          ${sortedColumn === `artist`
            ? html`<i class="ph-bold ${sortDirection === `desc` ? `ph-caret-down` : `ph-caret-up`}"></i>`
            : ``}
        </div>
        <div
          class="col-album ${sortedColumn === `album` ? `col--sorted` : ``}"
          @click="${() => this.sortByColumn(`album`)}"
          aria-sort="${ariaSort(`album`)}"
        >
          Album
          ${sortedColumn === `album`
            ? html`<i class="ph-bold ${sortDirection === `desc` ? `ph-caret-down` : `ph-caret-up`}"></i>`
            : ``}
        </div>
      </div>

      <div class="scroll-panel">
        <div class="virtual-scroll" style="height: ${totalSize}px;">
          ${isLoading
            ? html`<div class="loading">Loading ...</div>`
            : virtualItems.map((vItem) => {
                const item = groups
                  ? this.#flatItems[vItem.index]
                  : { type: /** @type {"track"} */ ("track"), track: tracks[vItem.index] };

                return item?.type === "group"
                  ? html`
                    <div
                      class="group-header"
                      style="transform: translateY(${vItem.start}px);"
                    >
                      <i class="ph-fill ph-vinyl-record"></i>
                      <span>${item.label}</span>
                    </div>
                  `
                  : item?.type === "track"
                  ? renderTrackRow(item.track, vItem.start)
                  : ``;
              })
          }
        </div>
      </div>
    `;
  }
}

export default Browser;

////////////////////////////////////////////
// HELPERS
////////////////////////////////////////////

/**
 * @param {{ label: string; tracks: Track[] }[]} groups
 * @returns {VirtualItem[]}
 */
function buildFlatList(groups) {
  /** @type {VirtualItem[]} */
  const items = [];
  for (const group of groups) {
    items.push({ type: "group", label: group.label });
    for (const track of group.tracks) {
      items.push({ type: "track", track });
    }
  }
  return items;
}

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = Browser;
export const NAME = "db-browser";

defineElement(NAME, CLASS);
