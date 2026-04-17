import {
  elementScroll,
  observeElementOffset,
  observeElementRect,
  Virtualizer,
} from "@tanstack/virtual-core";

import {
  defineElement,
  DiffuseElement,
  query,
  queryOptional,
  whenElementsDefined,
} from "~/common/element.js";
import { computed, signal, untracked } from "~/common/signal.js";
import * as Playlist from "~/common/playlist.js";

/**
 * @import {RenderArg} from "~/common/element.d.ts"
 * @import {SignalReader} from "~/common/signal.d.ts";
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {OutputElement} from "~/components/output/types.d.ts"
 * @import {ArtworkElement} from "~/components/artwork/types.d.ts"
 */

const MAX_ART_CONCURRENT = 8;

const TRACK_ROW_HEIGHT = 40;
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
  { value: "firstLetter", label: "Group by first letter", icon: "ph-text-aa" },
  { value: "directory", label: "Group by path", icon: "ph-folder" },
  { value: "createdAt", label: "Group by processing date", icon: "ph-clock" },
  { value: "tags.year", label: "Group by track year", icon: "ph-calendar" },
];

/**
 * @typedef {{ type: "group"; label: string }} GroupItem
 * @typedef {{ type: "track"; track: Track }} TrackItem
 * @typedef {GroupItem | TrackItem} VirtualItem
 */

/**
 * @typedef {{ type: "album"; albumKey: string; albumName: string; artist: string; track: Track }} OpenAlbumItem
 * @typedef {{ type: "artist"; artistKey: string; artistName: string; trackCount: number; track: Track }} OpenArtistItem
 * @typedef {OpenAlbumItem | OpenArtistItem} OpenCoverItem
 */

class Browser extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
  }

  // SIGNALS

  $artwork = signal(
    /** @type {ArtworkElement | undefined} */ (undefined),
  );

  $coverGroups = signal(
    /** @type {import("~/components/orchestrator/cover-groups/element.js").CLASS | undefined} */ (undefined),
  );

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

  // SIGNALS - Pt. 2

  #viewMode = signal(
    /** @type {"list" | "cover"} */ (
      localStorage.getItem("diffuse:browser:view-mode") === "cover"
        ? "cover"
        : "list"
    ),
  );

  #coverViewMode = signal(/** @type {"albums" | "artists"} */ ("albums"));

  #openCoverItem = signal(/** @type {OpenCoverItem | null} */ (null));

  // SIGNALS - Pt. 3

  $albumTrackMap = computed(() => {
    /** @type {Map<string, Track>} */
    const map = new Map();
    for (const { groups } of this.$coverGroups.value?.coverGroups() ?? []) {
      for (const { albumKey, track } of groups) {
        map.set(albumKey, track);
      }
    }
    for (const { groups } of this.$coverGroups.value?.artistGroups() ?? []) {
      for (const { artistKey, track } of groups) {
        map.set(artistKey, track);
      }
    }
    return map;
  });

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

  /** @type {Map<string, string | null>} */
  #coverArtCache = new Map();

  /** @type {Set<string>} */
  #pendingArtFetch = new Set();

  /** @type {{ albumKey: string; track: Track }[]} */
  #artFetchQueue = [];

  #artFetchActive = 0;
  #artRenderScheduled = false;

  /** @type {IntersectionObserver | undefined} */
  #coverObserver = undefined;

  #observedCards = new WeakSet();

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

    /** @type {import("~/components/configurator/artwork/element.js").CLASS | null} */
    const artwork = queryOptional(this, "artwork-selector");

    /** @type {import("~/components/orchestrator/cover-groups/element.js").CLASS | null} */
    const coverGroups = queryOptional(
      this,
      "cover-groups-orchestrator-selector",
    );

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

    whenElementsDefined({ output, provider, queue, scope, favourites }).then(
      () => {
        this.$output.value = output;
        this.$provider.value = provider;
        this.$queue.value = queue;
        this.$scope.value = scope;
        this.$favourites.value = favourites;
      },
    );

    if (artwork) {
      whenElementsDefined({ artwork }).then(() => {
        this.$artwork.value = artwork;
      });
    }

    if (coverGroups) {
      whenElementsDefined({ coverGroups }).then(() => {
        this.$coverGroups.value = coverGroups;
      });
    }

    // Reset scroll when track list changes
    this.effect(() => {
      const _results = this.$provider.value?.tracks();

      untracked(() => {
        const panel = this.root().querySelector(".scroll-panel");
        if (panel) panel.scrollTo(0, 0);
      });
    });

    // Set up the virtualizer after the first render, when .scroll-panel exists in the DOM.
    requestAnimationFrame(() => this.#setupVirtualizer());
  }

  /**
   * @override
   */
  disconnectedCallback() {
    super.disconnectedCallback();
    this.#virtualizerCleanup?.();
    this.#virtualizerCleanup = undefined;
    this.#virtualizer = undefined;
    this.#disconnectCoverObserver();
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

  clearSearch = () => {
    /** @type {HTMLInputElement | null} */
    const input = this.root().querySelector("#search-input");
    if (input) input.value = "";
    this.$scope.value?.setSearchTerm(undefined);
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

  toggleViewMode = () => {
    if (this.#viewMode.value === "cover") {
      this.#disconnectCoverObserver();
      this.#openCoverItem.value = null;
      requestAnimationFrame(() => this.#setupVirtualizer());
    }
    const next = this.#viewMode.value === "list" ? "cover" : "list";
    localStorage.setItem("diffuse:browser:view-mode", next);
    this.#viewMode.value = next;
  };

  /**
   * @param {"albums" | "artists"} mode
   */
  setCoverViewMode = (mode) => {
    this.#openCoverItem.value = null;
    this.#disconnectCoverObserver();
    this.#coverViewMode.value = mode;
  };

  // HELPERS

  /**
   * Enqueue an artwork fetch if not already pending or cached.
   * @param {string} albumKey
   * @param {Track} track
   */
  #fetchAlbumArt(albumKey, track) {
    if (this.#coverArtCache.has(albumKey)) return;
    if (this.#pendingArtFetch.has(albumKey)) return;
    this.#pendingArtFetch.add(albumKey);
    this.#artFetchQueue.push({ albumKey, track });
    this.#drainArtQueue();
  }

  #drainArtQueue() {
    while (
      this.#artFetchActive < MAX_ART_CONCURRENT &&
      this.#artFetchQueue.length > 0
    ) {
      const job = this.#artFetchQueue.shift();
      if (!job) break;
      this.#artFetchActive++;
      this.#doFetchAlbumArt(job.albumKey, job.track);
    }
  }

  /**
   * @param {string} albumKey
   * @param {Track} track
   */
  async #doFetchAlbumArt(albumKey, track) {
    const artwork = this.$artwork.value;
    try {
      const bytes = artwork ? await artwork.get(track) : null;
      if (bytes) {
        const mime = detectMime(bytes);
        const url = URL.createObjectURL(
          new Blob([/** @type {ArrayBuffer} */ (bytes.buffer)], { type: mime }),
        );
        this.#coverArtCache.set(albumKey, url);
      } else {
        this.#coverArtCache.set(albumKey, null);
      }
    } catch {
      this.#coverArtCache.set(albumKey, null);
    } finally {
      this.#artFetchActive--;
      this.#drainArtQueue();
    }
    this.#scheduleArtRender();
  }

  #scheduleArtRender() {
    if (this.#artRenderScheduled) return;
    this.#artRenderScheduled = true;
    requestAnimationFrame(() => {
      this.#artRenderScheduled = false;
      this.forceRender();
    });
  }

  #setupCoverObserver() {
    const root = this.root().querySelector(".cover-scroll-panel");
    if (!root) return;

    if (!this.#coverObserver) {
      this.#coverObserver = new IntersectionObserver(
        (entries) => {
          for (const entry of entries) {
            if (!entry.isIntersecting) continue;
            const albumKey =
              /** @type {HTMLElement} */ (entry.target).dataset.albumKey;
            if (!albumKey) continue;
            const track = this.$albumTrackMap().get(albumKey);
            if (track) this.#fetchAlbumArt(albumKey, track);
            this.#coverObserver?.unobserve(entry.target);
          }
        },
        { root, rootMargin: "200px" },
      );
    }

    for (
      const card of this.root().querySelectorAll(".cover-card[data-album-key]")
    ) {
      if (this.#observedCards.has(card)) continue;
      this.#observedCards.add(card);
      this.#coverObserver.observe(card);
    }
  }

  #setupVirtualizer() {
    const panel = this.root().querySelector(".scroll-panel");
    if (!panel) return;

    this.#virtualizerCleanup?.();

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
    this.forceRender();
  }

  #disconnectCoverObserver() {
    this.#coverObserver?.disconnect();
    this.#coverObserver = undefined;
    this.#observedCards = new WeakSet();
  }

  // RENDER

  /**
   * @param {Function} html
   * @param {boolean} isLoading
   */
  #renderCoverView(html, isLoading) {
    if (this.#openCoverItem.value) return this.#renderCoverDetail(html);

    const coverViewMode = this.#coverViewMode.value;
    const sortDirection = this.$scope.value?.sortDirection() ?? "asc";

    const totalCount = coverViewMode === "artists"
      ? (this.$coverGroups.value?.artistGroups() ?? []).reduce(
        (n, g) => n + g.groups.length,
        0,
      )
      : (this.$coverGroups.value?.coverGroups() ?? []).reduce(
        (n, g) => n + g.groups.length,
        0,
      );

    const countLabel = coverViewMode === "artists"
      ? `${totalCount} ${totalCount === 1 ? "artist" : "artists"}`
      : `${totalCount} ${totalCount === 1 ? "album" : "albums"}`;

    const tabs = html`
      <div class="cover-tabs">
        <div class="cover-tabs-start">
          <button
            class="browser-button ${coverViewMode === `artists`
              ? `browser-button--active`
              : ``}"
            @click="${() => this.setCoverViewMode(`artists`)}"
          >
            <i class="ph-bold ph-user-list"></i>
            Artists
          </button>
          <button
            class="browser-button ${coverViewMode === `albums`
              ? `browser-button--active`
              : ``}"
            @click="${() => this.setCoverViewMode(`albums`)}"
          >
            <i class="ph-bold ph-vinyl-record"></i>
            Albums
          </button>
        </div>
        <div class="cover-tabs-end">
          <span class="cover-count">${countLabel}</span>
          <button
            class="toolbar-icon-btn"
            @click="${() => {
              const scope = this.$scope.value;
              if (!scope) return;
              scope.setSortDirection(
                scope.sortDirection() === `asc` ? `desc` : `asc`,
              );
            }}"
            title="${sortDirection === `desc`
              ? `Sort ascending`
              : `Sort descending`}"
          >
            <i class="ph-bold ph-arrow-${sortDirection === `desc`
              ? `down`
              : `up`}"></i>
          </button>
        </div>
      </div>
    `;

    if (isLoading) {
      return html`
        ${tabs}
        <div class="scroll-panel"><div class="loading">Loading ...</div></div>
      `;
    }

    if (coverViewMode === "artists") {
      const artistGroups = this.$coverGroups.value?.artistGroups() ?? [];

      requestAnimationFrame(() => this.#setupCoverObserver());

      return html`
        ${tabs}
        <div class="scroll-panel cover-scroll-panel">
          ${artistGroups.map(({ label, groups }, groupIndex) =>
            html`
              ${label
                ? html`
                  <div class="cover-group-header ${groupIndex === 0
                    ? `cover-group-header--top`
                    : ``}">
                    <i class="ph-fill ph-vinyl-record"></i>
                    <span>${label}</span>
                  </div>
                `
                : ``}
              <div class="cover-grid">
                ${groups.map(({ artistKey, artistName, trackCount, track }) => {
                  const artUrl = this.#coverArtCache.get(artistKey);
                  return html`
                    <div
                      class="cover-card"
                      data-album-key="${artistKey}"
                      @click="${() => {
                        this.#openCoverItem.value = {
                          type: "artist",
                          artistKey,
                          artistName,
                          trackCount,
                          track,
                        };
                        if (!this.#coverArtCache.has(artistKey)) {
                          this.#fetchAlbumArt(artistKey, track);
                        }
                      }}"
                      title="${artistName}"
                    >
                      <div class="cover-art">
                        ${artUrl
                          ? html`
                            <img src="${artUrl}" alt="${artistName}" loading="lazy" />
                          `
                          : html`
                            <div class="cover-art-placeholder"><i class="ph-fill ph-vinyl-record"></i></div>
                          `}
                      </div>
                      <div class="cover-info">
                        <span class="cover-album">${artistName}</span>
                        <span class="cover-artist">${trackCount} ${trackCount ===
                            1
                          ? `track`
                          : `tracks`}</span>
                      </div>
                    </div>
                  `;
                })}
              </div>
            `
          )}
        </div>
      `;
    }

    // Albums mode
    const coverGroups = this.$coverGroups.value?.coverGroups() ?? [];

    requestAnimationFrame(() => this.#setupCoverObserver());

    return html`
      ${tabs}
      <div class="scroll-panel cover-scroll-panel">
        ${coverGroups.map(({ label, groups }, groupIndex) =>
          html`
            ${label
              ? html`
                <div class="cover-group-header ${groupIndex === 0
                  ? `cover-group-header--top`
                  : ``}">
                  <i class="ph-fill ph-vinyl-record"></i>
                  <span>${label}</span>
                </div>
              `
              : ``}
            <div class="cover-grid">
              ${groups.map(({ albumKey, albumName, artist, track }) => {
                const artUrl = this.#coverArtCache.get(albumKey);
                return html`
                  <div
                    class="cover-card"
                    data-album-key="${albumKey}"
                    @click="${() => {
                      this.#openCoverItem.value = {
                        type: "album",
                        albumKey,
                        albumName,
                        artist,
                        track,
                      };
                      if (!this.#coverArtCache.has(albumKey)) {
                        this.#fetchAlbumArt(albumKey, track);
                      }
                    }}"
                    title="${albumName} — ${artist}"
                  >
                    <div class="cover-art">
                      ${artUrl
                        ? html`
                          <img src="${artUrl}" alt="${albumName}" loading="lazy" />
                        `
                        : html`
                          <div class="cover-art-placeholder"><i class="ph-fill ph-vinyl-record"></i></div>
                        `}
                    </div>
                    <div class="cover-info">
                      <span class="cover-album">${albumName}</span>
                      <span class="cover-artist">${artist}</span>
                    </div>
                  </div>
                `;
              })}
            </div>
          `
        )}
      </div>
    `;
  }

  /**
   * @param {Function} html
   * @param {boolean} isLoading
   * @param {string[]} sortBy
   */
  #renderListView(html, isLoading, sortBy) {
    const tracks = this.$provider.value?.tracks() ?? [];
    const groups = /** @type {any} */ (this.$provider.value)?.groups?.();
    const sortDirection = this.$scope.value?.sortDirection();

    const sortedColumn = Object.entries(COLUMN_SORT).find(
      ([, v]) => JSON.stringify(v) === JSON.stringify(sortBy),
    )?.[0];

    const ariaSort = /** @param {string} col */ (col) =>
      sortedColumn === col
        ? (sortDirection === "desc" ? "descending" : "ascending")
        : "none";

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
        return `${String(a?.value ?? "").toLowerCase()}|${
          String(t?.value ?? "").toLowerCase()
        }`;
      }),
    );

    /**
     * @param {Track} track
     * @param {number} top
     * @param {number} index
     */
    const renderTrackRow = (track, top, index) => {
      const key = `${String(track.tags?.artist ?? "").toLowerCase()}|${
        String(track.tags?.title ?? "").toLowerCase()
      }`;
      const isFav = favSet.has(key);
      return html`
        <div
          class="track-row ${index === 0
            ? `track-row--top`
            : ``} ${index % 2 === 1 ? `track-row--alt` : ``}"
          style="transform: translateY(${top}px);"
          @dblclick="${() => this.playTrack(track)}"
        >
          <div class="col-fav">
            <button
              class="fav-btn ${isFav ? `fav-btn--active` : ``}"
              @click="${(/** @type {Event} */ e) => {
                e.stopPropagation();
                this.toggleFavourite(track);
              }}"
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
      <div class="table-header">
        <div class="col-fav"></div>
        <div
          class="col-title ${sortedColumn === `title` ? `col--sorted` : ``}"
          @click="${() => this.sortByColumn(`title`)}"
          aria-sort="${ariaSort(`title`)}"
        >
          Title ${sortedColumn === `title`
            ? html`
              <i class="ph-bold ${sortDirection === `desc`
                ? `ph-caret-down`
                : `ph-caret-up`}"></i>
            `
            : ``}
        </div>
        <div
          class="col-artist ${sortedColumn === `artist` ? `col--sorted` : ``}"
          @click="${() => this.sortByColumn(`artist`)}"
          aria-sort="${ariaSort(`artist`)}"
        >
          Artist ${sortedColumn === `artist`
            ? html`
              <i class="ph-bold ${sortDirection === `desc`
                ? `ph-caret-down`
                : `ph-caret-up`}"></i>
            `
            : ``}
        </div>
        <div
          class="col-album ${sortedColumn === `album` ? `col--sorted` : ``}"
          @click="${() => this.sortByColumn(`album`)}"
          aria-sort="${ariaSort(`album`)}"
        >
          Album ${sortedColumn === `album`
            ? html`
              <i class="ph-bold ${sortDirection === `desc`
                ? `ph-caret-down`
                : `ph-caret-up`}"></i>
            `
            : ``}
        </div>
      </div>

      <div class="scroll-panel">
        <div class="virtual-scroll" style="height: ${totalSize}px;">
          ${isLoading
            ? html`
              <div class="loading">Loading ...</div>
            `
            : virtualItems.map((vItem) => {
              const item = groups ? this.#flatItems[vItem.index] : {
                type: /** @type {"track"} */ ("track"),
                track: tracks[vItem.index],
              };

              return item?.type === "group"
                ? html`
                  <div
                    class="group-header ${vItem.index === 0
                      ? `group-header--top`
                      : ``}"
                    style="transform: translateY(${vItem.start}px);"
                  >
                    <i class="ph-fill ph-vinyl-record"></i>
                    <span>${item.label}</span>
                  </div>
                `
                : item?.type === "track"
                ? renderTrackRow(item.track, vItem.start, vItem.index)
                : ``;
            })}
        </div>
      </div>
    `;
  }

  /**
   * @param {Function} html
   */
  #renderCoverDetail(html) {
    const item = this.#openCoverItem.value;
    if (!item) {
      return html`

      `;
    }

    const allTracks = this.$provider.value?.tracks() ?? [];

    let key = "",
      name = "",
      subtitle = "",
      detailTracks = /** @type {Track[]} */ ([]);

    if (item.type === "album") {
      key = item.albumKey;
      name = item.albumName;
      subtitle = item.artist;
      detailTracks = allTracks.filter((t) => {
        return String(t.tags?.album ?? "").toLowerCase() === key;
      });
    } else {
      key = item.artistKey;
      name = item.artistName;
      subtitle = `${item.trackCount} ${
        item.trackCount === 1 ? "track" : "tracks"
      }`;
      detailTracks = allTracks.filter((t) =>
        String(t.tags?.artist ?? "").toLowerCase() === key
      );
    }

    const artUrl = this.#coverArtCache.get(key);

    const favItems = this.$favourites.value?.playlistItems() ?? [];
    const favSet = new Set(
      favItems.map((fav) => {
        const a = fav.criteria.find((c) => c.field === "tags.artist");
        const t = fav.criteria.find((c) => c.field === "tags.title");
        return `${String(a?.value ?? "").toLowerCase()}|${
          String(t?.value ?? "").toLowerCase()
        }`;
      }),
    );

    const menuLabel = item.type === "album" ? "Play album" : "Play all";

    return html`
      <div class="album-detail">
        <div class="album-detail-actions">
          <button
            class="toolbar-icon-btn"
            @click="${() => {
              this.#openCoverItem.value = null;
            }}"
            title="Back"
          >
            <i class="ph-bold ph-arrow-left"></i>
          </button>
          <button
            class="toolbar-icon-btn"
            popovertarget="album-actions-menu"
            title="More options"
          >
            <i class="ph-fill ph-dots-three-outline"></i>
          </button>
          <div id="album-actions-menu" class="dropdown" popover>
            <button @click="${() => {
              if (!detailTracks.length) return;
              this.$queue.value?.add({
                inFront: true,
                trackIds: detailTracks.map((t) => t.id),
              });
              this.$queue.value?.shift();
              /** @type {HTMLElement | null} */ (this.root().querySelector(
                `#album-actions-menu`,
              ))?.hidePopover();
            }}">
              ${menuLabel}
            </button>
            <button @click="${() => {
              if (!detailTracks.length) return;
              this.$queue.value?.add({
                trackIds: detailTracks.map((t) => t.id),
              });
              /** @type {HTMLElement | null} */ (this.root().querySelector(
                `#album-actions-menu`,
              ))?.hidePopover();
            }}">
              Add to queue
            </button>
          </div>
        </div>
        <div class="album-detail-main">
          <div class="album-detail-sidebar">
            <div class="album-detail-art">
              ${artUrl
                ? html`
                  <img src="${artUrl}" alt="${name}" />
                `
                : html`
                  <div class="cover-art-placeholder"><i class="ph-fill ph-vinyl-record"></i></div>
                `}
            </div>
            <div class="album-detail-info">
              <span class="album-detail-name">${name}</span>
              <span class="album-detail-artist">${subtitle}</span>
            </div>
          </div>
          <div class="album-detail-tracks">
            ${detailTracks.map((t, i) => {
              const favKey = `${String(t.tags?.artist ?? "").toLowerCase()}|${
                String(t.tags?.title ?? "").toLowerCase()
              }`;
              const isFav = favSet.has(favKey);
              return html`
                <div
                  class="album-track-row ${i % 2 === 1
                    ? `album-track-row--alt`
                    : ``}"
                  @dblclick="${() => this.playTrack(t)}"
                >
                  <div class="col-fav">
                    <button
                      class="fav-btn ${isFav ? `fav-btn--active` : ``}"
                      @click="${(/** @type {Event} */ e) => {
                        e.stopPropagation();
                        this.toggleFavourite(t);
                      }}"
                      title="${isFav
                        ? `Remove from favourites`
                        : `Add to favourites`}"
                    >
                      <i class="ph-${isFav
                        ? `fill ph-heart`
                        : `bold ph-heart`}"></i>
                    </button>
                  </div>
                  <div class="col-title">
                    <span class="track-title">${t.tags?.title}</span>
                  </div>
                  <div class="col-artist">
                    <span>${t.tags?.artist}</span>
                  </div>
                </div>
              `;
            })}
          </div>
        </div>
      </div>
    `;
  }

  /**
   * @param {RenderArg} _
   */
  /**
   * @param {Function} html
   * @param {string[]} sortBy
   * @param {string | undefined} groupBy
   */
  #renderGroupByMenu(html, sortBy, groupBy) {
    return html`
      <button class="toolbar-icon-btn" popovertarget="groupby-menu" title="Group by">
        <i class="ph-fill ph-stack"></i>
      </button>
      <div id="groupby-menu" class="dropdown" popover>
        ${GROUP_BY_OPTIONS.map(
          ({ value, label }) => {
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
                  if (isActive) {
                    this.setGroupBy(undefined);
                  } else {
                    this.setGroupBy(resolvedValue);
                  }
                  /** @type {HTMLElement | null} */ (this.root().querySelector(
                    `#groupby-menu`,
                  ))?.hidePopover();
                }}"
              >
                ${isActive
                  ? html`
                    <i class="ph-bold ph-x"></i>
                  `
                  : ``} ${label}
              </button>
            `;
          },
        )}
      </div>
    `;
  }

  /**
   * @param {Function} html
   * @param {string | undefined} playlist
   */
  #renderPlaylistMenu(html, playlist) {
    return html`
      <button
        class="browser-button browser-button--playlist ${playlist
          ? `browser-button--active`
          : ``}"
        popovertarget="playlist-menu"
      >
        <i class="ph-fill ph-playlist"></i>
        <span>${playlist ?? `All tracks`}</span>
      </button>
      <div id="playlist-menu" class="dropdown" popover>
        <button @click="${() => {
          this.setSelectedPlaylist(undefined);
          /** @type {HTMLElement | null} */ (this.root().querySelector(
            `#playlist-menu`,
          ))?.hidePopover();
        }}">
          All tracks
        </button>
        ${this.$groupedPlaylists().map((group) =>
          group.playlists.map((p) =>
            html`
              <button @click="${() => {
                this.setSelectedPlaylist(p.name);
                /** @type {HTMLElement | null} */ (this.root().querySelector(
                  `#playlist-menu`,
                ))?.hidePopover();
              }}">
                ${p.name}
              </button>
            `
          )
        )}
      </div>
    `;
  }

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    const isLoading =
      this.$output.value?.tracks?.collection().state !== "loaded";

    const playlist = this.$scope.value?.playlist();
    const groupBy = this.$scope.value?.groupBy();
    const searchTerm = this.$scope.value?.searchTerm() ?? "";
    const sortBy = this.$scope.value?.sortBy() ?? DEFAULT_SORT;
    const viewMode = this.#viewMode.value;

    return html`
      <link rel="stylesheet" href="styles/base.css" />
      <link rel="stylesheet" href="styles/diffuse/facet.css" />
      <link rel="stylesheet" href="vendor/@phosphor-icons/web/bold/style.css" />
      <link rel="stylesheet" href="vendor/@phosphor-icons/web/fill/style.css" />
      <link rel="stylesheet" href="themes/blur/variables.css" />
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
          <!-- CLEAR SEARCH -->
          ${searchTerm
            ? html`
              <button class="toolbar-icon-btn" @click="${this
                .clearSearch}" title="Clear search">
                <i class="ph-bold ph-x"></i>
              </button>
            `
            : ``}

          <!-- VIEW KIND -->
          <button
            class="toolbar-icon-btn"
            @click="${this.toggleViewMode}"
            title="${viewMode === `cover`
              ? `Switch to list view`
              : `Switch to cover view`}"
          >
            <i class="${viewMode === `cover`
              ? `ph-bold ph-list-heart`
              : `ph-fill ph-images-square`}"></i>
          </button>

          <!-- MENUS -->
          ${this.#renderGroupByMenu(html, sortBy, groupBy)} ${this
            .#renderPlaylistMenu(html, playlist)}
        </div>
      </div>

      ${viewMode === `cover`
        ? this.#renderCoverView(html, isLoading)
        : this.#renderListView(html, isLoading, sortBy)}
    `;
  }
}

export default Browser;

////////////////////////////////////////////
// HELPERS
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
