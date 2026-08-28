import {
  defineElement,
  DiffuseElement,
  nothing,
  query,
  queryOptional,
  whenElementsDefined,
} from "~/common/element.js";
import { batch, computed, signal, untracked } from "~/common/signal.js";
import * as Playlist from "~/common/playlist.js";
import { repeat } from "~/vendor/lit-html/directives/repeat.js";

/**
 * @import {RenderArg} from "~/common/element.d.ts"
 * @import {SignalReader} from "~/common/signal.d.ts";
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {OutputElement} from "@specs/components/output/types.d.ts"
 * @import {ArtworkElement} from "@specs/components/artwork/types.d.ts"
 */

const TRACK_ROW_HEIGHT = 48;
const TRACK_ROW_STRIDE = 48;
const OVERSCAN = 10;
const MAX_ART_CONCURRENT = 6;

/**
 * @param {Track} track
 */
function trackTitle(track) {
  if (track.tags?.title) return track.tags.title;
  const path = track.uri.split("?")[0];
  const filename = path.split("/").filter(Boolean).at(-1);
  return filename ? decodeURIComponent(filename) : track.uri;
}

/**
 * @typedef {{ type: "album"; albumKey: string; albumName: string; artist: string; track: Track }} AlbumItem
 * @typedef {{ type: "artist"; artistKey: string; artistName: string; trackCount: number; track: Track }} ArtistItem
 * @typedef {{ type: "tracks"; playlist: string | undefined }} TracksView
 * @typedef {{ type: "albums" } | { type: "artists" } | AlbumItem | ArtistItem | TracksView } View
 */

class Browser extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
  }

  // SIGNALS - dependencies

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

  // SIGNALS - state

  #view = signal(
    /** @type {View} */ ({ type: "tracks", playlist: undefined }),
  );

  #history = signal(/** @type {View[]} */ ([]));
  #future = signal(/** @type {View[]} */ ([]));

  // Cover art cache
  /** @type {Map<string, string | null>} */
  #coverArtCache = new Map();
  /** @type {Set<string>} */
  #pendingArtFetch = new Set();
  /** @type {{ key: string; track: Track }[]} */
  #artFetchQueue = [];
  #artFetchActive = 0;
  #artRenderScheduled = false;
  /** @type {IntersectionObserver | undefined} */
  #coverObserver = undefined;
  /** @type {Map<string, Track>} */
  #pendingVisibleCards = new Map();
  /** @type {ReturnType<typeof setTimeout> | undefined} */
  #artFetchDebounce = undefined;
  #coverScrollTop = 0;

  // Track list virtual scroll state
  #scrollTop = 0;
  #viewportHeight = 0;
  #renderedStartIndex = -1;
  #renderedEndIndex = -1;
  #itemCount = 0;
  #rowHeight = TRACK_ROW_HEIGHT;
  #rowStride = TRACK_ROW_STRIDE;
  /** @type {ResizeObserver | undefined} */
  #resizeObserver;
  /** @type {AbortController | undefined} */
  #scrollAbort;
  /** @type {IntersectionObserver | undefined} */
  #scrollVisibilityObserver;
  /** @type {number | undefined} */
  #scrollRenderRaf = undefined;
  /** @type {Track[] | undefined} */
  #renderedTracks = undefined;

  // COMPUTED

  $currentTracks = computed(() => {
    const view = this.#view.value;
    if (view.type === "tracks") {
      return this.$provider.value?.tracks() ?? [];
    }
    return /** @type {Track[]} */ ([]);
  });

  $detailTracks = computed(() => {
    const view = this.#view.value;
    if (view.type === "album") {
      return this.$tracksByAlbum().get(view.albumKey) ?? [];
    }
    if (view.type === "artist") {
      return this.$tracksByArtist().get(view.artistKey) ?? [];
    }
    return /** @type {Track[]} */ ([]);
  });

  $tracksByAlbum = computed(() => {
    /** @type {Map<string, Track[]>} */
    const map = new Map();
    for (const t of this.$provider.value?.tracks() ?? []) {
      const key = String(t.tags?.album ?? "").toLowerCase();
      if (!map.has(key)) map.set(key, []);
      map.get(key)?.push(t);
    }
    return map;
  });

  $tracksByArtist = computed(() => {
    /** @type {Map<string, Track[]>} */
    const map = new Map();
    for (const t of this.$provider.value?.tracks() ?? []) {
      const key = String(t.tags?.artist ?? "").toLowerCase();
      if (!map.has(key)) map.set(key, []);
      map.get(key)?.push(t);
    }
    return map;
  });

  $sortedCoverGroups = computed(() => {
    const groups = this.$coverGroups.value?.coverGroups() ?? [];
    return this.#sortGroups(groups);
  });

  $sortedArtistGroups = computed(() => {
    const groups = this.$coverGroups.value?.artistGroups() ?? [];
    return this.#sortGroups(groups);
  });

  $groupedPlaylists = computed(() => {
    const col = this.$output.value?.playlistItems.collection();
    if (!col || col.state !== "loaded" || !col.data.length) return [];
    const items = col.data;

    /** @type {Map<string, { name: string; unordered: boolean }>} */
    const playlistMap = Playlist.gather(items);

    const all = [...playlistMap.values()].sort((a, b) =>
      a.name.localeCompare(b.name)
    );

    return [{ label: "Playlists", playlists: all }];
  });

  $favouritesSet = computed(() => {
    const items = this.$favourites.value?.playlistItems() ?? [];
    return new Set(
      items.map((item) => {
        const a = item.criteria.find((c) => c.field === "tags.artist");
        const t = item.criteria.find((c) => c.field === "tags.title");
        return `${String(a?.value ?? "").toLowerCase()}|${
          String(t?.value ?? "").toLowerCase()
        }`;
      }),
    );
  });

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

    /** @type {import("~/components/orchestrator/favourites/element.js").CLASS | null} */
    const favourites = queryOptional(
      this,
      "favourites-orchestrator-selector",
    );

    whenElementsDefined({ output, provider, queue, scope }).then(() => {
      batch(() => {
        this.$output.value = output;
        this.$provider.value = provider;
        this.$queue.value = queue;
        this.$scope.value = scope;
      });
    });

    if (favourites) {
      whenElementsDefined({ favourites }).then(() => {
        this.$favourites.value = favourites;
      });
    }

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
      const _ = this.$currentTracks();
      const _d = this.$detailTracks();
      untracked(() => {
        const panel = this.root().querySelector(".td-tracks-panel");
        if (panel) {
          panel.scrollTo(0, 0);
          this.#scrollTop = 0;
        }
      });
    });

    // Re-attach scroll tracking when returning to a track-listing view
    // from the albums/artists grid (where .td-tracks-panel is removed)
    this.effect(() => {
      const view = this.#view.value;
      if (view.type === "albums" || view.type === "artists") return;

      untracked(() => {
        requestAnimationFrame(() => this.#setupScrollTracking());
      });
    });

    // Re-observe cover cards when groups change
    this.effect(() => {
      const _cover = this.$sortedCoverGroups();
      const _artist = this.$sortedArtistGroups();
      const view = this.#view.value;
      if (view.type !== "albums" && view.type !== "artists") return;

      untracked(() => {
        requestAnimationFrame(() => {
          this.#setupCoverObserver();
          this.#fetchVisibleCoverArt();
          requestAnimationFrame(() => this.#fetchVisibleCoverArt());
        });
      });
    });

    this.#setupScrollTracking();
  }

  /**
   * @override
   */
  disconnectedCallback() {
    super.disconnectedCallback();
    this.#scrollAbort?.abort();
    this.#scrollAbort = undefined;
    this.#scrollVisibilityObserver?.disconnect();
    this.#scrollVisibilityObserver = undefined;
    this.#resizeObserver?.disconnect();
    this.#resizeObserver = undefined;
    this.#disconnectCoverObserver();
    if (this.#scrollRenderRaf !== undefined) {
      cancelAnimationFrame(this.#scrollRenderRaf);
      this.#scrollRenderRaf = undefined;
    }
  }

  // HELPERS

  /**
   * @template {{ label: string; groups: { track: Track }[] }} G
   * @param {G[]} groups
   * @returns {G[]}
   */
  #sortGroups(groups) {
    const dir = this.$scope.value?.sortDirection() ?? "asc";
    if (dir !== "desc") return groups;
    return groups.map((g) => ({ ...g, groups: [...g.groups].reverse() }));
  }

  // ACTIONS

  /**
   * @param {string | undefined} playlist
   */
  setSelectedPlaylist = (playlist) => {
    this.#disconnectCoverObserver();
    this.#clearHistory();
    this.$scope.value?.setPlaylist(playlist);
    this.#view.value = { type: "tracks", playlist };
  };

  /**
   * @param {View} view
   */
  #navigateTo(view) {
    this.#disconnectCoverObserver();
    this.#history.value = [...this.#history.value, this.#view.value];
    this.#future.value = [];
    this.#view.value = view;
    this.#renderedTracks = undefined;
  }

  goBack = () => {
    const hist = this.#history.value;
    if (hist.length === 0) return;
    const prev = hist[hist.length - 1];
    if (!prev) return;
    this.#disconnectCoverObserver();
    this.#future.value = [this.#view.value, ...this.#future.value];
    this.#history.value = hist.slice(0, -1);
    this.#view.value = prev;
    this.#renderedTracks = undefined;
  };

  goForward = () => {
    const fut = this.#future.value;
    if (fut.length === 0) return;
    const next = fut[0];
    if (!next) return;
    this.#disconnectCoverObserver();
    this.#history.value = [...this.#history.value, this.#view.value];
    this.#future.value = fut.slice(1);
    this.#view.value = next;
    this.#renderedTracks = undefined;
  };

  #clearHistory() {
    this.#history.value = [];
    this.#future.value = [];
  }

  /**
   * @param {Track} track
   */
  playTrack = (track) => {
    this.$queue.value?.add({ inFront: true, trackIds: [track.id] });
    this.$queue.value?.shift();
  };

  /**
   * @param {Track} track
   */
  addToQueue = (track) => {
    this.$queue.value?.add({ trackIds: [track.id] });
  };

  /**
   * @param {Track} track
   */
  toggleFavourite = (track) => {
    this.$favourites.value?.toggle(track);
  };

  setSearchTerm = () => {
    /** @type {HTMLInputElement | null} */
    const input = this.root().querySelector("#td-search-input");
    const term = input?.value?.trim();
    this.$scope.value?.setSearchTerm(term || undefined);
  };

  clearSearch = () => {
    /** @type {HTMLInputElement | null} */
    const input = this.root().querySelector("#td-search-input");
    if (input) input.value = "";
    this.$scope.value?.setSearchTerm(undefined);
  };

  /**
   * @param {AlbumItem} item
   */
  openAlbum = (item) => {
    const panel = this.root().querySelector(".td-covers-panel");
    if (panel) this.#coverScrollTop = panel.scrollTop;
    this.#navigateTo(item);
  };

  /**
   * @param {ArtistItem} item
   */
  openArtist = (item) => {
    const panel = this.root().querySelector(".td-covers-panel");
    if (panel) this.#coverScrollTop = panel.scrollTop;
    this.#navigateTo(item);
  };

  browseAlbums = () => {
    this.#navigateTo({ type: "albums" });
  };

  browseArtists = () => {
    this.#navigateTo({ type: "artists" });
  };

  // ARTWORK CACHE

  /**
   * @param {string} key
   * @param {Track} track
   */
  #fetchAlbumArt(key, track) {
    if (this.#coverArtCache.has(key)) return;
    if (this.#pendingArtFetch.has(key)) return;
    this.#pendingArtFetch.add(key);
    this.#artFetchQueue.push({ key, track });
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
      this.#doFetchAlbumArt(job.key, job.track);
    }
  }

  /**
   * @param {string} key
   * @param {Track} track
   */
  async #doFetchAlbumArt(key, track) {
    const artwork = this.$artwork.value;
    try {
      const timeout = new Promise(
        (resolve) => setTimeout(() => resolve(null), 30_000),
      );
      const bytes = artwork
        ? await Promise.race([artwork.get(track), timeout])
        : null;
      if (bytes) {
        const mime = detectMime(bytes);
        const url = URL.createObjectURL(
          new Blob([bytes], { type: mime }),
        );
        this.#coverArtCache.set(key, url);
      } else {
        this.#coverArtCache.set(key, null);
      }
    } catch {
      // don't cache on error — let it be retried
    } finally {
      this.#pendingArtFetch.delete(key);
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

  #fetchVisibleCoverArt() {
    const panel = this.root().querySelector(".td-covers-panel");
    if (!panel) return;
    const panelRect = panel.getBoundingClientRect();
    const margin = 200;
    for (
      const card of this.root().querySelectorAll(
        ".td-cover-card[data-cover-key]",
      )
    ) {
      const el = /** @type {HTMLElement} */ (card);
      const key = el.dataset.coverKey;
      if (
        !key || this.#coverArtCache.has(key) ||
        this.#pendingArtFetch.has(key)
      ) continue;
      const cardRect = el.getBoundingClientRect();
      if (
        cardRect.bottom + margin < panelRect.top ||
        cardRect.top - margin > panelRect.bottom
      ) continue;
      const trackId = el.dataset.coverTrackId;
      /** @type {Track | undefined} */
      const track = trackId
        ? this.$provider.value?.tracks().find((t) => t.id === trackId)
        : undefined;
      if (track) this.#fetchAlbumArt(key, track);
    }
  }

  #setupCoverObserver() {
    const root = this.root().querySelector(".td-covers-panel");
    if (!root) return;

    if (!this.#coverObserver) {
      this.#coverObserver = new IntersectionObserver(
        (entries) => {
          let hasNew = false;
          for (const entry of entries) {
            if (!entry.isIntersecting) continue;
            hasNew = true;
            const key =
              /** @type {HTMLElement} */ (entry.target).dataset.coverKey;
            if (!key) continue;
            const trackId =
              /** @type {HTMLElement} */ (entry.target).dataset.coverTrackId;
            /** @type {Track | undefined} */
            const track = trackId
              ? this.$provider.value?.tracks().find((t) => t.id === trackId)
              : undefined;
            if (track) this.#pendingVisibleCards.set(key, track);
            this.#coverObserver?.unobserve(entry.target);
          }
          if (!hasNew) return;
          clearTimeout(this.#artFetchDebounce);
          this.#artFetchDebounce = setTimeout(() => {
            for (const [key, track] of this.#pendingVisibleCards) {
              this.#fetchAlbumArt(key, track);
            }
            this.#pendingVisibleCards.clear();
          }, 150);
        },
        { root, rootMargin: "200px" },
      );
    }

    for (
      const card of this.root().querySelectorAll(
        ".td-cover-card[data-cover-key]",
      )
    ) {
      const key = /** @type {HTMLElement} */ (card).dataset.coverKey;
      if (
        key &&
        (this.#coverArtCache.has(key) || this.#pendingArtFetch.has(key))
      ) continue;
      this.#coverObserver.observe(card);
    }
  }

  #disconnectCoverObserver() {
    this.#coverObserver?.disconnect();
    this.#coverObserver = undefined;
    clearTimeout(this.#artFetchDebounce);
    this.#pendingVisibleCards.clear();
  }

  // TRACK LIST VIRTUAL SCROLL

  #setupScrollTracking() {
    this.#scrollAbort?.abort();
    this.#scrollAbort = new AbortController();
    this.#scrollVisibilityObserver?.disconnect();
    this.#scrollVisibilityObserver = undefined;
    this.#resizeObserver?.disconnect();
    this.#resizeObserver = undefined;
    this.#scrollTop = 0;

    const abort = this.#scrollAbort;

    const attach = () => {
      if (abort.signal.aborted) return;
      const panel = this.root().querySelector(".td-tracks-panel");
      if (!panel) return;

      panel.addEventListener(
        "scroll",
        () => {
          this.#scrollTop = panel.scrollTop;
          this.#renderIfWindowChanged();
        },
        { passive: true, signal: abort.signal },
      );

      this.#resizeObserver = new ResizeObserver((entries) => {
        this.#viewportHeight = Math.min(
          entries[0].contentRect.height,
          window.innerHeight,
        );
        this.#renderIfWindowChanged();
      });

      this.#resizeObserver.observe(panel);
    };

    this.#scrollVisibilityObserver = new IntersectionObserver(
      (entries, observer) => {
        if (!entries[0].isIntersecting) return;
        observer.disconnect();
        this.#scrollVisibilityObserver = undefined;
        requestAnimationFrame(attach);
      },
    );

    this.#scrollVisibilityObserver.observe(this);
  }

  #renderIfWindowChanged() {
    const { startIndex, endIndex } = this.#computeWindow(this.#itemCount);

    if (
      startIndex === this.#renderedStartIndex &&
      endIndex === this.#renderedEndIndex
    ) return;

    this.forceRender();
  }

  /**
   * @param {number} count
   * @returns {{ startIndex: number; endIndex: number }}
   */
  #computeWindow(count) {
    const scrollTop = this.#scrollTop;
    const viewportHeight = this.#viewportHeight;
    const stride = this.#rowStride;

    const startIndex = Math.max(
      0,
      Math.floor(scrollTop / stride) - OVERSCAN,
    );
    const visibleCount = Math.ceil(viewportHeight / stride) + 2 * OVERSCAN;
    return {
      startIndex,
      endIndex: Math.min(count, startIndex + visibleCount),
    };
  }

  // RENDER

  /**
   * @param {Function} html
   */
  #renderSidebar(html) {
    const currentPlaylist = this.$scope.value?.playlist();
    const groups = this.$groupedPlaylists();
    const playlists = groups[0]?.playlists ?? [];

    return html`
      <aside class="td-sidebar">
        <div class="td-sidebar__scroll">
          <button
            class="td-playlist-row td-playlist-row--all ${currentPlaylist === undefined ? `td-playlist-row--active` : ""}"
            @click="${() => this.setSelectedPlaylist(undefined)}"
          >
            <span>All tracks</span>
          </button>

          ${playlists.length > 0
            ? html`
              <div class="td-sidebar__label">Playlists</div>
              ${playlists.map((p) => {
                const isActive = currentPlaylist === p.name;
                return html`
                  <button
                    class="td-playlist-row ${isActive ? `td-playlist-row--active` : ""}"
                    @click="${() => this.setSelectedPlaylist(p.name)}"
                    title="${p.name}"
                  >
                    <span>${p.name}</span>
                  </button>
                `;
              })}
            `
            : nothing}
        </div>
      </aside>
    `;
  }

  /**
   * @param {Function} html
   */
  #renderToolbar(html) {
    const searchTerm = this.$scope.value?.searchTerm() ?? "";
    const canBack = this.#history.value.length > 0;
    const canForward = this.#future.value.length > 0;

    return html`
      <div class="td-toolbar">
        <div class="td-toolbar__nav">
          <button
            class="td-toolbar__icon-btn"
            ?disabled="${!canBack}"
            @click="${this.goBack}"
            title="Back"
          >
            <i class="ph-bold ph-caret-left"></i>
          </button>
          <button
            class="td-toolbar__icon-btn"
            ?disabled="${!canForward}"
            @click="${this.goForward}"
            title="Forward"
          >
            <i class="ph-bold ph-caret-right"></i>
          </button>
        </div>
        <div class="td-toolbar__search">
          <i class="ph-bold ph-magnifying-glass"></i>
          <input
            id="td-search-input"
            type="search"
            placeholder="Search"
            .value="${searchTerm}"
            @change="${this.setSearchTerm}"
          />
          ${searchTerm
            ? html`
              <button
                class="td-toolbar__icon-btn td-toolbar__icon-btn--inline"
                @click="${this.clearSearch}"
                title="Clear search"
              >
                <i class="ph-bold ph-x"></i>
              </button>
            `
            : nothing}
        </div>
      </div>
    `;
  }

  /**
   * @param {Function} html
   */
  #renderSubCategories(html) {
    const view = this.#view.value;
    if (view.type !== "tracks") return nothing;

    return html`
      <div class="td-subcategories">
        <button
          class="td-subcat"
          @click="${this.browseAlbums}"
          title="Browse albums"
        >
          <i class="ph-fill ph-vinyl-record"></i>
          <span>Albums</span>
        </button>
          <button
            class="td-subcat"
            @click="${this.browseArtists}"
            title="Browse artists"
          >
            <i class="ph-fill ph-users"></i>
            <span>Artists</span>
          </button>
      </div>
    `;
  }

  /**
   * @param {Function} html
   */
  #renderDetailHeader(html) {
    const view = this.#view.value;
    if (view.type !== "album" && view.type !== "artist") return nothing;

    /** @type {string} */
    let key = "";
    /** @type {string} */
    let name = "";
    /** @type {string} */
    let subtitle = "";
    /** @type {Track} */
    let track;
    if (view.type === "album") {
      key = view.albumKey;
      name = view.albumName;
      subtitle = view.artist;
      track = view.track;
    } else {
      key = view.artistKey;
      name = view.artistName;
      subtitle = `${view.trackCount} ${view.trackCount === 1 ? "track" : "tracks"}`;
      track = view.track;
    }

    const artUrl = this.#coverArtCache.get(key);

    return html`
      <div class="td-detail-header">
        <div class="td-detail-art">
          ${artUrl
            ? html`<img src="${artUrl}" alt="${name}" />`
            : html`
              <div class="td-cover-placeholder">
                <i class="ph-fill ph-music-notes"></i>
              </div>
            `}
        </div>
        <div class="td-detail-meta">
          <div class="td-detail-name">${name}</div>
          <div class="td-detail-subtitle">${subtitle}</div>
        </div>
      </div>
    `;
  }

  /**
   * @param {Function} html
   */
  #renderTrackList(html) {
    const view = this.#view.value;
    const tracks = view.type === "tracks"
      ? this.$currentTracks()
      : this.$detailTracks();

    if (tracks.length === 0) {
      this.#itemCount = 0;
      this.#renderedTracks = undefined;
      this.#renderedStartIndex = -1;
      this.#renderedEndIndex = -1;
      return html`
        <div class="td-tracks-list">
          <div class="td-tracks-header">
            <div class="td-track-header__title">Title</div>
            <div class="td-track-header__artist">Artist</div>
            <div class="td-track-header__album">Album</div>
            <div class="td-track-header__time">
              <i class="ph-bold ph-clock"></i>
            </div>
            <div class="td-track-header__actions"></div>
          </div>
          <div class="td-tracks-panel">
            <div class="td-empty">
              <i class="ph-fill ph-music-notes"></i>
              <p>No tracks</p>
            </div>
          </div>
        </div>
      `;
    }

    if (tracks !== this.#renderedTracks) {
      this.#renderedStartIndex = -1;
      this.#renderedEndIndex = -1;
      this.#renderedTracks = tracks;
    }

    const count = tracks.length;
    this.#itemCount = count;
    const { startIndex, endIndex } = this.#computeWindow(count);
    this.#renderedStartIndex = startIndex;
    this.#renderedEndIndex = endIndex;
    const totalSize = count * TRACK_ROW_STRIDE;

    return html`
      <div class="td-tracks-list">
        <div class="td-tracks-header">
          <div class="td-track-header__title">Title</div>
          <div class="td-track-header__artist">Artist</div>
          <div class="td-track-header__album">Album</div>
          <div class="td-track-header__time">
            <i class="ph-bold ph-clock"></i>
          </div>
          <div class="td-track-header__actions"></div>
        </div>
        <div class="td-tracks-panel">
          <div class="td-tracks-virtual" style="height: ${totalSize}px;">
            ${repeat(
              tracks.slice(startIndex, endIndex).map((track, i) => ({
                track,
                index: startIndex + i,
                top: (startIndex + i) * TRACK_ROW_STRIDE,
              })),
              (entry) => `td-tr-${entry.track.id}`,
              (entry) => this.#renderTrackRow(html, entry.track, entry.top, entry.index),
            )}
          </div>
        </div>
      </div>
    `;
  }

  /**
   * @param {Function} html
   * @param {Track} track
   * @param {number} top
   * @param {number} index
   */
  #renderTrackRow(html, track, top, index) {
    const albumKey = String(track.tags?.album ?? "").toLowerCase();
    this.#fetchAlbumArt(albumKey, track);
    const artUrl = this.#coverArtCache.get(albumKey);

    const favKey = `${String(track.tags?.artist ?? "").toLowerCase()}|${
      String(track.tags?.title ?? "").toLowerCase()
    }`;
    const isFav = this.$favouritesSet().has(favKey);

    return html`
      <div
        class="td-track-row"
        style="transform: translateY(${top}px);"
        @dblclick="${() => this.playTrack(track)}"
      >
        <div class="td-track__title">
          <div class="td-track__art">
            ${artUrl
              ? html`<img src="${artUrl}" alt="" loading="lazy" />`
              : html`
                <div class="td-track-art-placeholder">
                  <i class="ph-fill ph-music-notes"></i>
                </div>
              `}
          </div>
          <span class="td-track__title-text">${trackTitle(track)}</span>
        </div>
        <div class="td-track__artist">
          <span>${track.tags?.artist ?? ""}</span>
        </div>
        <div class="td-track__album">
          <span>${track.tags?.album ?? ""}</span>
        </div>
        <div class="td-track__time">${formatDuration(track)}</div>
        <div class="td-track__actions">
          <button
            class="td-track__action"
            @click="${(/** @type {Event} */ e) => {
              e.stopPropagation();
              this.addToQueue(track);
            }}"
            title="Add to queue"
          >
            <i class="ph-bold ph-plus"></i>
          </button>
          <button
            class="td-track__action ${isFav ? `td-track__action--active` : ""}"
            @click="${(/** @type {Event} */ e) => {
              e.stopPropagation();
              this.toggleFavourite(track);
            }}"
            title="${isFav ? `Remove from favourites` : `Add to favourites`}"
          >
            <i class="${isFav
              ? `ph-fill ph-heart`
              : `ph-bold ph-heart`}"></i>
          </button>
        </div>
      </div>
    `;
  }

  /**
   * @param {Function} html
   */
  #renderCoverGrid(html) {
    const view = this.#view.value;
    if (view.type === "albums") return this.#renderAlbumsGrid(html);
    if (view.type === "artists") return this.#renderArtistsGrid(html);
    return nothing;
  }

  /**
   * @param {Function} html
   */
  #renderAlbumsGrid(html) {
    const groups = this.$sortedCoverGroups();
    const totalCount = groups.reduce((n, g) => n + g.groups.length, 0);
    const totalLabel = `${totalCount} ${totalCount === 1 ? "album" : "albums"}`;

    requestAnimationFrame(() => this.#setupCoverObserver());

    if (totalCount === 0) {
      return html`
        <div class="td-covers-panel td-empty">
          <i class="ph-fill ph-vinyl-record"></i>
          <p>No albums</p>
        </div>
      `;
    }

    return html`
      <div class="td-covers-header">
        <span class="td-covers-count">${totalLabel}</span>
      </div>
      <div class="td-covers-panel">
        ${groups.map(({ label, groups: items }, groupIndex) => html`
          ${label
            ? html`
              <div class="td-cover-group ${groupIndex === 0
                ? `td-cover-group--top`
                : ""}">
                <span>${label}</span>
              </div>
            `
            : nothing}
          <div class="td-cover-grid">
            ${items.map((/** @type {any} */ item) => {
              const artUrl = this.#coverArtCache.get(item.albumKey);
              return html`
                <div
                  class="td-cover-card"
                  data-cover-key="${item.albumKey}"
                  data-cover-track-id="${item.track.id}"
                  @click="${() => this.openAlbum({
                    type: "album",
                    albumKey: item.albumKey,
                    albumName: item.albumName,
                    artist: item.artist,
                    track: item.track,
                  })}"
                  title="${item.albumName} — ${item.artist}"
                >
                  <div class="td-cover-art">
                    ${artUrl
                      ? html`
                        <img
                          src="${artUrl}"
                          alt="${item.albumName}"
                          loading="lazy"
                          @error="${() => {
                            this.#coverArtCache.set(item.albumKey, null);
                            this.#scheduleArtRender();
                          }}"
                        />
                      `
                      : html`
                        <div class="td-cover-placeholder">
                          <i class="ph-fill ph-music-notes"></i>
                        </div>
                      `}
                  </div>
                  <div class="td-cover-info">
                    <span class="td-cover-album">${item.albumName}</span>
                    <span class="td-cover-artist">${item.artist}</span>
                  </div>
                </div>
              `;
            })}
          </div>
        `)}
      </div>
    `;
  }

  /**
   * @param {Function} html
   */
  #renderArtistsGrid(html) {
    const groups = this.$sortedArtistGroups();
    const totalCount = groups.reduce((n, g) => n + g.groups.length, 0);
    const totalLabel = `${totalCount} ${totalCount === 1
      ? "artist"
      : "artists"}`;

    requestAnimationFrame(() => this.#setupCoverObserver());

    if (totalCount === 0) {
      return html`
        <div class="td-covers-panel td-empty">
          <i class="ph-fill ph-user"></i>
          <p>No artists</p>
        </div>
      `;
    }

    return html`
      <div class="td-covers-header">
        <span class="td-covers-count">${totalLabel}</span>
      </div>
      <div class="td-covers-panel">
        ${groups.map(({ label, groups: items }, groupIndex) => html`
          ${label
            ? html`
              <div class="td-cover-group ${groupIndex === 0
                ? `td-cover-group--top`
                : ""}">
                <span>${label}</span>
              </div>
            `
            : nothing}
          <div class="td-cover-grid">
            ${items.map((/** @type {any} */ item) => {
              const artUrl = this.#coverArtCache.get(item.artistKey);
              return html`
                <div
                  class="td-cover-card"
                  data-cover-key="${item.artistKey}"
                  data-cover-track-id="${item.track.id}"
                  @click="${() => this.openArtist({
                    type: "artist",
                    artistKey: item.artistKey,
                    artistName: item.artistName,
                    trackCount: item.trackCount,
                    track: item.track,
                  })}"
                  title="${item.artistName}"
                >
                  <div class="td-cover-art">
                    ${artUrl
                      ? html`
                        <img
                          src="${artUrl}"
                          alt="${item.artistName}"
                          loading="lazy"
                          @error="${() => {
                            this.#coverArtCache.set(item.artistKey, null);
                            this.#scheduleArtRender();
                          }}"
                        />
                      `
                      : html`
                        <div class="td-cover-placeholder">
                          <i class="ph-fill ph-user"></i>
                        </div>
                      `}
                  </div>
                  <div class="td-cover-info">
                    <span class="td-cover-album">${item.artistName}</span>
                    <span class="td-cover-artist">${item.trackCount}
                      ${item.trackCount === 1 ? `track` : `tracks`}</span>
                  </div>
                </div>
              `;
            })}
          </div>
        `)}
      </div>
    `;
  }

  /**
   * @param {Function} html
   */
  #renderMain(html) {
    const view = this.#view.value;

    return html`
      <main class="td-main">
        ${this.#renderToolbar(html)} ${view.type === "albums" ||
            view.type === "artists"
          ? this.#renderCoverGrid(html)
          : html`
            ${this.#renderSubCategories(html)} ${view.type === "album" ||
                view.type === "artist"
              ? this.#renderDetailHeader(html)
              : nothing} ${this.#renderTrackList(html)}
          `}
      </main>
    `;
  }

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    return html`
      <link rel="stylesheet" href="styles/base.css" />
      <link rel="stylesheet" href="vendor/@phosphor-icons/web/bold/style.css" />
      <link rel="stylesheet" href="vendor/@phosphor-icons/web/fill/style.css" />
      <link rel="stylesheet" href="facets/themes/tidal/variables.css" />
      <link rel="stylesheet" href="facets/themes/tidal/browser/element.css" />

      <div class="td-shell">
        ${this.#renderSidebar(html)} ${this.#renderMain(html)}
      </div>
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
 * @param {Track} track
 * @returns {string}
 */
function formatDuration(track) {
  const ms = track.stats?.duration;
  if (!ms) return "—";
  const totalSec = Math.round(ms / 1000);
  const m = Math.floor(totalSec / 60);
  const s = totalSec % 60;
  return `${m}:${String(s).padStart(2, "0")}`;
}

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = Browser;
export const NAME = "db-tidal-browser";

defineElement(NAME, CLASS);
