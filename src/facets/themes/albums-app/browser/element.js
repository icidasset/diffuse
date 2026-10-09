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

// Cover grid virtualization: cards are chunked into fixed-column rows,
// which are windowed like track rows. Column count and row height are
// measured from the rendered panel.
const GRID_MIN_CARD = 140; // 8.75rem, matches the CSS card minimum
const GRID_GAP_X = 16; // 1rem column gap
const GRID_ROW_GAP = 20; // 1.25rem gap between grid rows
const GRID_OVERSCAN_ROWS = 2;
const GRID_GROUP_HEIGHT = 37; // letter header, corrected after measure

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
 * @param {number | undefined} ms
 * @returns {string}
 */
function formatDuration(ms) {
  if (!ms || ms < 0) return "–:––";
  const totalSec = Math.round(ms / 1000);
  const h = Math.floor(totalSec / 3600);
  const m = Math.floor((totalSec % 3600) / 60);
  const s = totalSec % 60;
  if (h > 0) return `${h}:${String(m).padStart(2, "0")}:${String(s).padStart(2, "0")}`;
  return `${m}:${String(s).padStart(2, "0")}`;
}

/**
 * @param {number | undefined} seconds
 * @returns {string}
 */
function formatClock(seconds) {
  if (seconds === undefined || Number.isNaN(seconds)) return "0:00";
  const totalSec = Math.max(0, Math.round(seconds));
  const m = Math.floor(totalSec / 60);
  const s = totalSec % 60;
  return `${m}:${String(s).padStart(2, "0")}`;
}

/**
 * Groups sorted rows by the first letter of their label. Rows without a key
 * (the "Unknown album/artist" pseudo-entries for untagged tracks) sort before
 * everything else, but belong under ♯ together with digit/symbol-named rows.
 * @template {{ key: string; label: string }} R
 * @param {R[]} rows
 * @returns {{ letter: string; rows: R[] }[]}
 */
function groupByLetter(rows) {
  /** @type {Map<string, R[]>} */
  const groups = new Map();

  for (const row of rows) {
    const letter = row.key ? row.label.charAt(0).toUpperCase() : "#";
    const key = /[A-Z]/.test(letter) ? letter : "#";
    if (!groups.has(key)) groups.set(key, []);
    groups.get(key)?.push(row);
  }

  return [...groups.entries()].map(([letter, rows]) => ({
    letter,
    rows,
  }));
}

/**
 * @typedef {{ type: "album"; albumKey: string; albumName: string; artist: string; track: Track }} AlbumItem
 * @typedef {{ type: "artist"; artistKey: string; artistName: string; trackCount: number; track: Track }} ArtistItem
 * @typedef {{ type: "albums" } | { type: "artists" } | { type: "tracks" } | { type: "playlist-tracks" } | AlbumItem | ArtistItem } View
 */

const LIBRARY_ITEMS = [
  { type: "albums", label: "Albums", icon: "ph-vinyl-record" },
  { type: "artists", label: "Artists", icon: "ph-microphone-stage" },
  { type: "playlists", label: "Playlists", icon: "ph-playlist" },
  { type: "tracks", label: "Songs", icon: "ph-music-notes" },
];

/** Which library item a view belongs to (drives rail + catalog state). */
const RAIL_FOR_VIEW = /** @type {const} */ ({
  albums: "albums",
  album: "albums",
  artists: "artists",
  artist: "artists",
  "playlist-tracks": "playlists",
  tracks: "tracks",
});

/** Sort fields per list column. */
const COLUMN_SORT = {
  title: ["tags.title"],
  artist: ["tags.artist", "tags.album", "tags.disc.no", "tags.track.no"],
  album: ["tags.album", "tags.disc.no", "tags.track.no"],
};

class Browser extends DiffuseElement {
  static observedAttributes = ["group", "group-label"];

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

  $controller = signal(
    /** @type {import("~/components/orchestrator/controller/element.js").CLASS | undefined} */ (undefined),
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

  #view = signal(/** @type {View} */ ({ type: "albums" }));

  #catalogCollapsed = signal(false);

  #playlistFilter = signal("");

  #history = signal(/** @type {View[]} */ ([]));
  #future = signal(/** @type {View[]} */ ([]));

  // Cover art cache (albums, artists and playlist thumbnails share it)
  /** @type {Map<string, string | null | undefined>} */
  #coverArtCache = new Map();
  /** @type {Set<string>} */
  #pendingArtFetch = new Set();
  /** @type {{ key: string; track: Track }[]} */
  #artFetchQueue = [];
  #artFetchActive = 0;
  #artRenderScheduled = false;
  /** @type {IntersectionObserver | undefined} */
  #rowArtObserver = undefined;

  // Cover grid virtual scroll state
  #gridScrollTop = 0;
  #gridViewportHeight = window.innerHeight;
  #gridLabelHeight = 0;
  #gridCols = 4;
  #gridStride = 240;
  #gridGroupHeight = GRID_GROUP_HEIGHT;
  #gridLayoutDirty = false;
  /** @type {{ label: string; groups: any[] }[] | null} */
  #gridSource = null;
  #gridMode = "";
  /** @type {({ type: "group"; label: string } | { type: "row"; items: any[] })[]} */
  #gridItems = [];
  /** @type {number[]} */
  #gridOffsets = [];
  #renderedGridStart = -1;
  #renderedGridEnd = -1;

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
  /** @type {HTMLElement | null} */
  #observedPanel = null;
  /** @type {AbortController | undefined} */
  #scrollAbort;
  /** @type {Track[] | undefined} */
  #renderedTracks = undefined;

  // COMPUTED

  $currentTracks = computed(() => {
    const view = this.#view.value;
    if (view.type === "album") {
      return this.$tracksByAlbum().get(view.albumKey) ?? [];
    }
    if (view.type === "artist") {
      return this.$tracksByArtist().get(view.artistKey) ?? [];
    }
    return this.$provider.value?.tracks() ?? [];
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

  $albumDurations = computed(() => {
    /** @type {Map<string, { ms: number; count: number }>} */
    const map = new Map();
    for (const t of this.$provider.value?.tracks() ?? []) {
      const key = String(t.tags?.album ?? "").toLowerCase();
      const entry = map.get(key) ?? { ms: 0, count: 0 };
      entry.ms += t.stats?.duration ?? 0;
      entry.count++;
      map.set(key, entry);
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

  /** Flattened album rows for the catalog column. */
  $albumRows = computed(() =>
    this.$sortedCoverGroups().flatMap((g) => g.groups));

  /** Flattened artist rows for the catalog column. */
  $artistRows = computed(() =>
    this.$sortedArtistGroups().flatMap((g) => g.groups));

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

    /** @type {import("~/components/orchestrator/controller/element.js").CLASS | null} */
    const controller = queryOptional(
      this,
      "controller-orchestrator-selector",
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

    if (controller) {
      whenElementsDefined({ controller }).then(() => {
        this.$controller.value = controller;
      });
    }

    // Reset scroll when the track list changes
    this.effect(() => {
      const _ = this.$currentTracks();
      untracked(() => {
        const panel = this.root().querySelector(".da-tracks-panel");
        if (panel) {
          panel.scrollTo(0, 0);
          this.#scrollTop = 0;
        }
      });
    });

    // Observe catalog rows for lazy thumbnail loading
    this.effect(() => {
      const _albums = this.$albumRows();
      const _artists = this.$artistRows();
      const _filter = this.#playlistFilter.value;
      const _view = this.#view.value;
      const col = this.$output.value?.playlistItems.collection();
      const _playlists = col?.state === "loaded" ? col.data.length : undefined;

      untracked(() => {
        requestAnimationFrame(() => this.#setupRowArtObserver());
      });
    });

    this.#setupScrollListener();
  }

  /**
   * @override
   */
  disconnectedCallback() {
    super.disconnectedCallback();
    this.#scrollAbort?.abort();
    this.#scrollAbort = undefined;
    this.#resizeObserver?.disconnect();
    this.#resizeObserver = undefined;
    this.#observedPanel = null;
    this.#disconnectRowArtObserver();
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

  /**
   * All tracks covered by the current view — used by the shuffle action.
   * @returns {Track[]}
   */
  #currentViewTracks() {
    const view = this.#view.value;
    if (view.type === "album") {
      return this.$tracksByAlbum().get(view.albumKey) ?? [];
    }
    if (view.type === "artist") {
      return this.$tracksByArtist().get(view.artistKey) ?? [];
    }
    return this.$provider.value?.tracks() ?? [];
  }

  /**
   * @returns {boolean}
   */
  #isCollectionLoading() {
    const col = this.$output.value?.tracks.collection();
    return !col || col.state !== "loaded";
  }

  // ACTIONS

  /**
   * @param {View} view
   */
  #navigateTo(view) {
    this.#disconnectRowArtObserver();
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
    this.#disconnectRowArtObserver();
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
    this.#disconnectRowArtObserver();
    this.#history.value = [...this.#history.value, this.#view.value];
    this.#future.value = fut.slice(1);
    this.#view.value = next;
    this.#renderedTracks = undefined;
  };

  /**
   * @param {string | undefined} playlist
   */
  setSelectedPlaylist = (playlist) => {
    this.$scope.value?.setPlaylist(playlist);
    this.#view.value = { type: "playlist-tracks" };
    this.#renderedTracks = undefined;
  };

  /**
   * @param {"albums" | "artists" | "playlists" | "tracks"} type
   */
  #browse(type) {
    const current = this.#view.value;
    if (RAIL_FOR_VIEW[current.type] === type) return;

    // Library navigation means the whole collection, so lift any
    // playlist filter that was active.
    this.$scope.value?.setPlaylist(undefined);
    this.#playlistFilter.value = "";

    // The covers panel is shared between the albums/artists views —
    // start from the top for the new section
    const panel = /** @type {HTMLElement | null} */ (
      this.root().querySelector(".da-covers-panel")
    );
    panel?.scrollTo(0, 0);
    this.#gridScrollTop = 0;

    if (type === "albums") this.#navigateTo({ type: "albums" });
    else if (type === "artists") this.#navigateTo({ type: "artists" });
    else if (type === "playlists") {
      this.#navigateTo({ type: "playlist-tracks" });
    } else this.#navigateTo({ type: "tracks" });
  }

  browseAlbums = () => this.#browse("albums");

  browseArtists = () => this.#browse("artists");

  browsePlaylists = () => this.#browse("playlists");

  browseTracks = () => this.#browse("tracks");

  toggleCatalog = () => {
    this.#catalogCollapsed.value = !this.#catalogCollapsed.value;
  };

  /**
   * @param {AlbumItem} item
   */
  openAlbum = (item) => {
    this.#navigateTo(item);
  };

  /**
   * @param {ArtistItem} item
   */
  openArtist = (item) => {
    this.#navigateTo(item);
  };

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

  /**
   * Appends every track of the current view to the end of the queue.
   */
  addViewToQueue = () => {
    const tracks = this.#currentViewTracks();
    if (!tracks.length) return;
    this.$queue.value?.add({ trackIds: tracks.map((t) => t.id) });
  };

  /**
   * Sort the track list by a column. Clicking the active column toggles the
   * direction; clicking it again when descending reverts to the default sort.
   * @param {"title" | "artist" | "album"} column
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

  setSearchTerm = () => {
    /** @type {HTMLInputElement | null} */
    const input = this.root().querySelector("#da-search-input");
    const term = input?.value?.trim();
    this.$scope.value?.setSearchTerm(term || undefined);
  };

  clearSearch = () => {
    /** @type {HTMLInputElement | null} */
    const input = this.root().querySelector("#da-search-input");
    if (input) input.value = "";
    this.$scope.value?.setSearchTerm(undefined);
  };

  setPlaylistFilter = () => {
    /** @type {HTMLInputElement | null} */
    const input = this.root().querySelector("#da-playlist-filter");
    this.#playlistFilter.value = input?.value ?? "";
  };

  /**
   * @param {string} letter
   */
  jumpToLetter = (letter) => {
    const panel = this.root().querySelector(".da-catalog__scroll");
    const section = this.root().querySelector(
      `.da-playlist-section[data-letter="${letter}"]`,
    );
    if (!panel || !section) return;
    panel.scrollTo({
      top: /** @type {HTMLElement} */ (section).offsetTop,
      behavior: "smooth",
    });
  };

  // MINI PLAYER

  currentTrack = () => this.$controller.value?.currentTrack();

  isPlaying = () => this.$controller.value?.isPlaying() ?? false;

  playPause = () => {
    const audioId = this.$controller.value?.$queue.value?.now()?.id;
    if (this.isPlaying() && audioId) {
      this.$controller.value?.$audio.value?.pause({ audioId });
    } else if (audioId) {
      this.$controller.value?.$audio.value?.play({ audioId });
    }
  };

  next = () => {
    this.$controller.value?.$queue.value?.shift();
  };

  previous = () => {
    this.$controller.value?.$queue.value?.unshift();
  };

  /**
   * @param {MouseEvent} event
   */
  seek = (event) => {
    const target = event.target
      ? /** @type {HTMLProgressElement} */ (event.target)
      : null;
    const percentage = target ? event.offsetX / target.clientWidth : 0;
    const audioId = this.$controller.value?.$queue.value?.now()?.id;

    if (audioId) {
      this.$controller.value?.$audio.value?.seek({ audioId, percentage });
    }
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
    this.#coverArtCache.set(key, undefined);
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
      this.#coverArtCache.delete(key);
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

  // COVER GRID VIRTUAL SCROLL

  /**
   * Chunks the grouped cover entries into virtual rows of `#gridCols`
   * cards and recomputes the cumulative height offsets.
   * @param {{ label: string; groups: any[] }[]} groups
   */
  #rebuildGridItems(groups) {
    const cols = this.#gridCols;

    /** @type {({ type: "group"; label: string } | { type: "row"; items: any[] })[]} */
    const items = [];

    for (const { label, groups: entries } of groups) {
      if (label) items.push({ type: "group", label });
      for (let i = 0; i < entries.length; i += cols) {
        items.push({ type: "row", items: entries.slice(i, i + cols) });
      }
    }

    this.#gridItems = items;

    const offsets = new Array(items.length + 1);
    offsets[0] = 0;
    let acc = 0;
    for (let i = 0; i < items.length; i++) {
      acc += items[i].type === "group" ? this.#gridGroupHeight : this.#gridStride;
      offsets[i + 1] = acc;
    }
    this.#gridOffsets = offsets;
  }

  /**
   * Visible window of grid items for the current scroll position.
   * Pure math — no DOM reads, so it's safe to call from the scroll
   * handler. The `.da-covers-label` heading scrolls with the content,
   * so its (cached) height offsets the virtual container.
   * @returns {{ startIndex: number; endIndex: number }}
   */
  #computeGridRange() {
    const virtualTop = this.#gridScrollTop - this.#gridLabelHeight;
    const over = GRID_OVERSCAN_ROWS * this.#gridStride;

    const items = this.#gridItems;
    const offsets = this.#gridOffsets;
    const top = virtualTop - over;
    const bottom = virtualTop + this.#gridViewportHeight + over;

    let startIndex = 0;
    while (startIndex < items.length && offsets[startIndex + 1] <= top) {
      startIndex++;
    }

    let endIndex = startIndex;
    while (endIndex < items.length && offsets[endIndex] < bottom) {
      endIndex++;
    }

    return { startIndex, endIndex };
  }

  #renderIfGridWindowChanged() {
    if (!this.#gridItems.length) return;

    const { startIndex, endIndex } = this.#computeGridRange();
    if (
      startIndex === this.#renderedGridStart &&
      endIndex === this.#renderedGridEnd
    ) return;

    this.forceRender();
  }

  /**
   * Measures the panel to derive the grid column count, and corrects the
   * row/group heights against the rendered DOM.
   */
  #measureGrid() {
    const panel = this.root().querySelector(".da-covers-panel");
    if (!panel) return;

    let dirty = false;

    const label = /** @type {HTMLElement | null} */ (
      this.root().querySelector(".da-covers-label")
    );
    if (label) {
      const height = label.offsetHeight;
      if (height && height !== this.#gridLabelHeight) {
        this.#gridLabelHeight = height;
        dirty = true;
      }
    }

    const styles = getComputedStyle(panel);
    const avail = panel.clientWidth -
      parseFloat(styles.paddingLeft) - parseFloat(styles.paddingRight);
    const cols = Math.max(
      2,
      Math.floor((avail + GRID_GAP_X) / (GRID_MIN_CARD + GRID_GAP_X)),
    );
    if (cols !== this.#gridCols) {
      this.#gridCols = cols;
      dirty = true;
    }

    const row = /** @type {HTMLElement | null} */ (
      this.root().querySelector(".da-cover-row")
    );
    if (row) {
      const stride = row.offsetHeight + GRID_ROW_GAP;
      if (stride > GRID_ROW_GAP && stride !== this.#gridStride) {
        this.#gridStride = stride;
        dirty = true;
      }
    }

    const group = /** @type {HTMLElement | null} */ (
      this.root().querySelector(".da-cover-group")
    );
    if (group) {
      const height = group.offsetHeight;
      if (height && height !== this.#gridGroupHeight) {
        this.#gridGroupHeight = height;
        dirty = true;
      }
    }

    if (dirty) {
      this.#gridLayoutDirty = true;
      this.forceRender();
    }
  }

  /**
   * Lazily fetch the thumbnail for a catalog row (playlist, album or artist).
   * The row resolves its representative track off the render path.
   * @param {string} key  Art cache key
   * @param {() => Track | undefined} resolveTrack
   */
  #ensureRowArt(key, resolveTrack) {
    if (this.#coverArtCache.has(key)) return;
    if (this.#pendingArtFetch.has(key)) return;

    // Resolve off the render path — filtering can be expensive.
    queueMicrotask(() => {
      if (this.#coverArtCache.has(key)) return;
      const track = resolveTrack();
      if (!track) {
        this.#coverArtCache.set(key, null);
        this.#scheduleArtRender();
        return;
      }
      this.#fetchAlbumArt(key, track);
    });
  }

  #setupRowArtObserver() {
    const root = this.root().querySelector(".da-catalog__scroll");
    if (!root) return;

    this.#rowArtObserver?.disconnect();

    this.#rowArtObserver = new IntersectionObserver(
      (entries) => {
        for (const entry of entries) {
          if (!entry.isIntersecting) continue;
          const el = /** @type {HTMLElement} */ (entry.target);
          const key = el.dataset.artKey;
          const trackId = el.dataset.artTrackId;
          if (key) {
            this.#ensureRowArt(key, () =>
              trackId
                ? this.$provider.value?.tracks().find((t) => t.id === trackId)
                : this.#resolvePlaylistTrack(el.dataset.playlist ?? ""));
            this.#rowArtObserver?.unobserve(entry.target);
          }
        }
      },
      { root, rootMargin: "100px" },
    );

    for (
      const row of this.root().querySelectorAll("[data-art-key]")
    ) {
      this.#rowArtObserver.observe(row);
    }
  }

  /**
   * First track matching a playlist, used as its thumbnail.
   * @param {string} name
   * @returns {Track | undefined}
   */
  #resolvePlaylistTrack(name) {
    const col = this.$output.value?.playlistItems.collection();
    const tracks = this.$provider.value?.tracks() ?? [];
    const items = col?.state === "loaded"
      ? col.data.filter((i) => i.playlist === name)
      : [];

    if (!items.length) return undefined;
    return Playlist.filterByPlaylist(tracks, items)[0];
  }

  #disconnectRowArtObserver() {
    this.#rowArtObserver?.disconnect();
    this.#rowArtObserver = undefined;
  }

  // SCROLL TRACKING

  /**
   * Listen for scrolls of either scroll panel on the shadow root, in the
   * capture phase (scroll doesn't bubble, but capture reaches
   * descendants). The shadow root is stable across renders, so the
   * listener survives panels being replaced.
   */
  #setupScrollListener() {
    if (this.#scrollAbort) return;

    this.#scrollAbort = new AbortController();

    this.root().addEventListener(
      "scroll",
      (event) => {
        const panel = event.target;
        if (!(panel instanceof HTMLElement)) return;

        if (panel.classList.contains("da-covers-panel")) {
          this.#gridScrollTop = panel.scrollTop;
          this.#renderIfGridWindowChanged();
        } else if (panel.classList.contains("da-tracks-panel")) {
          this.#scrollTop = panel.scrollTop;
          this.#renderIfWindowChanged();
        }
      },
      { capture: true, passive: true, signal: this.#scrollAbort.signal },
    );
  }

  /**
   * Keep the resize observer pointed at the current scroll panel (it can
   * be replaced when switching views) and pick up restored scroll
   * positions.
   */
  #syncViewportObserver() {
    const panel = /** @type {HTMLElement | null} */ (
      this.root().querySelector(".da-tracks-panel") ??
        this.root().querySelector(".da-covers-panel")
    );

    if (!panel || panel === this.#observedPanel) return;
    this.#observedPanel = panel;

    const isGrid = panel.classList.contains("da-covers-panel");

    // The browser may have restored a scroll position
    if (isGrid) {
      this.#gridScrollTop = panel.scrollTop;
    } else {
      this.#scrollTop = panel.scrollTop;
    }

    this.#resizeObserver?.disconnect();
    this.#resizeObserver = new ResizeObserver(() => {
      // Defer past transient layouts — a panel mid-update can report 0
      requestAnimationFrame(() => {
        const height = Math.min(panel.clientHeight, window.innerHeight);
        if (height <= 0) return;

        if (isGrid) {
          if (this.#gridViewportHeight === height) return;
          this.#gridViewportHeight = height;
          this.#renderIfGridWindowChanged();
        } else {
          if (this.#viewportHeight === height) return;
          this.#viewportHeight = height;
          this.#renderIfWindowChanged();
        }
      });
    });

    this.#resizeObserver.observe(panel);
  }

  // TRACK LIST VIRTUAL SCROLL

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
   * @param {RenderArg} _
   */
  render({ html }) {
    const viewTitle = this.#viewTitle();

    // Keep observers pointed at the current scroll panel — it can be
    // replaced by any re-render (e.g. when the library loads)
    requestAnimationFrame(() => this.#syncViewportObserver());

    return html`
      <link rel="stylesheet" href="styles/base.css" />
      <link rel="stylesheet" href="vendor/@phosphor-icons/web/bold/style.css" />
      <link rel="stylesheet" href="vendor/@phosphor-icons/web/fill/style.css" />
      <link rel="stylesheet" href="facets/themes/albums-app/variables.css" />
      <link rel="stylesheet" href="facets/themes/albums-app/browser/element.css" />

      <div class="da-window">
        ${this.#renderTitlebar(html, viewTitle)}
        <div class="da-shell">
          ${this.#renderRail(html)}
          ${this.#renderCatalog(html)}
          ${this.#renderMain(html)}
        </div>
      </div>
    `;
  }

  /**
   * Window title for the current view, shown in the titlebar.
   * @returns {string}
   */
  #viewTitle() {
    const view = this.#view.value;
    const playlist = this.$scope.value?.playlist();

    if (view.type === "album") return view.albumName;
    if (view.type === "artist") return view.artistName;
    if (view.type === "artists") return "Artists";
    if (view.type === "tracks") return "Songs";
    if (view.type === "playlist-tracks") return playlist ?? "Playlists";
    return "Albums";
  }

  /**
   * @param {Function} html
   * @param {string} title
   */
  #renderTitlebar(html, title) {
    return html`
      <header class="da-titlebar">
        <a
          class="da-titlebar__btn"
          href="l/?path=facets%2Fdata%2Fsources%2Findex.tile"
          target="_blank"
          title="Audio inputs"
        >
          <i class="ph-bold ph-archive-box"></i>
        </a>

        <a
          class="da-titlebar__btn"
          href="l/?path=facets%2Fplayback%2Fqueue%2Findex.tile"
          target="_blank"
          title="Queue"
        >
          <i class="ph-bold ph-queue"></i>
        </a>

        <button
          class="da-titlebar__btn ${this.#catalogCollapsed.value
          ? `da-titlebar__btn--active`
          : ""}"
          @click="${this.toggleCatalog}"
          title="Toggle sidebar"
        >
          <i class="ph-bold ph-sidebar-simple"></i>
        </button>

        <span class="da-titlebar__divider"></span>
        <span class="da-titlebar__title">${title}</span>

        <div class="da-titlebar__spacer"></div>

        <span class="da-titlebar__group">${this.getAttribute("group-label") ??
        this.group}</span>

        <button class="da-titlebar__btn" @click="${this.#emitNewDeck}" title="Open a new deck">
          <i class="ph-bold ph-circles-three-plus"></i>
        </button>
      </header>
    `;
  }

  #emitNewDeck() {
    this.dispatchEvent(
      new CustomEvent("da-new-deck", { bubbles: true, composed: true }),
    );
  }

  /**
   * @param {Function} html
   */
  #renderRail(html) {
    const view = this.#view.value;
    const railItem = RAIL_FOR_VIEW[view.type];

    return html`
      <aside class="da-rail">
        <div class="da-rail__scroll">
          <div class="da-rail__search">
            <i class="ph-bold ph-magnifying-glass"></i>
            <input
              id="da-search-input"
              type="search"
              placeholder="Album, artist, or song..."
              .value="${this.$scope.value?.searchTerm() ?? ""}"
              @change="${this.setSearchTerm}"
            />
          </div>

          <div class="da-rail__label">
            <i class="ph-bold ph-house"></i>
            <span>Library</span>
          </div>

          ${LIBRARY_ITEMS.map((item) => {
            const isActive = railItem === item.type;
            return html`
              <button
                class="da-rail__item ${isActive ? `da-rail__item--active` : ""}"
                @click="${item.type === "albums"
                ? this.browseAlbums
                : item.type === "artists"
                ? this.browseArtists
                : item.type === "playlists"
                ? this.browsePlaylists
                : this.browseTracks}"
              >
                <i class="ph-${isActive ? `fill` : `bold`} ${item.icon}"></i>
                <span>${item.label}</span>
              </button>
            `;
          })}
        </div>

        <div class="da-rail__player">${this.#renderMiniPlayer(html)}</div>
      </aside>
    `;
  }

  /**
   * @param {Function} html
   */
  #renderMiniPlayer(html) {
    const track = this.currentTrack();
    const isPlaying = this.isPlaying();
    const audioState = this.$controller.value?.audio();
    const progress = audioState?.progress() ?? 0;
    const currentTime = audioState?.currentTime();
    const duration = audioState?.duration();
    const remaining = duration !== undefined && currentTime !== undefined
      ? -(Math.max(0, duration - currentTime))
      : -0;

    const albumKey = String(track?.tags?.album ?? "").toLowerCase();
    const artUrl = track ? this.#coverArtCache.get(albumKey) : undefined;

    if (track && this.#coverArtCache.get(albumKey) === undefined) {
      this.#fetchAlbumArt(albumKey, track);
    }

    return html`
      <div class="da-mini">
        <div class="da-mini__head">
          <div class="da-mini__art">
            ${artUrl
              ? html`<img src="${artUrl}" alt="" loading="lazy" />`
              : html`
                <div class="da-mini__art-placeholder">
                  <i class="ph-fill ph-music-notes"></i>
                </div>
              `}
          </div>
          <div class="da-mini__meta">
            <span class="da-mini__title">${track?.tags?.title ?? "Not playing"}</span>
            <span class="da-mini__artist">${track?.tags?.artist ?? "—"}</span>
            ${track?.tags?.album
              ? html`<span class="da-mini__album">${track.tags.album}</span>`
              : nothing}
          </div>
        </div>

        <div class="da-mini__progress" @click="${this.seek}">
          <progress max="100" value="${progress * 100}"></progress>
          <div class="da-mini__timestamps">
            <time>${formatClock(currentTime)}</time>
            <time>${formatClock(remaining)}</time>
          </div>
        </div>

        <div class="da-mini__controls">
          <button @click="${this.previous}" title="Previous track">
            <i class="ph-fill ph-rewind"></i>
          </button>
          <button class="da-mini__play" @click="${this.playPause}" title="${isPlaying ? `Pause` : `Play`}">
            <i class="ph-fill ${isPlaying ? `ph-pause` : `ph-play`}"></i>
          </button>
          <button @click="${this.next}" title="Next track">
            <i class="ph-fill ph-fast-forward"></i>
          </button>
        </div>
      </div>
    `;
  }

  /**
   * Which content the catalog column shows for the current view.
   * `null` means the column is hidden (Songs view).
   * @returns {"albums" | "artists" | "playlists" | null}
   */
  #catalogMode() {
    const railItem = RAIL_FOR_VIEW[this.#view.value.type];
    return railItem === "tracks" ? null : railItem;
  }

  /**
   * Filtered, letter-grouped rows for the catalog column.
   * @returns {{ label: string; placeholder: string; emptyLabel: string; sections: { letter: string; rows: { key: string; label: string; artKey: string; trackId: string | undefined }[] }[] } | null}
   */
  #catalogData() {
    const mode = this.#catalogMode();
    if (!mode) return null;

    const filter = this.#playlistFilter.value.trim().toLowerCase();

    /** @type {{ key: string; label: string; artKey: string; trackId: string | undefined }[]} */
    let rows;
    let label;
    let placeholder;
    let emptyLabel;

    if (mode === "albums") {
      rows = this.$albumRows().map((a) => ({
        key: a.albumKey,
        label: a.albumName,
        artKey: a.albumKey,
        trackId: a.track.id,
      }));
      label = "Albums";
      placeholder = "Album name...";
      emptyLabel = "No albums";
    } else if (mode === "artists") {
      rows = this.$artistRows().map((a) => ({
        key: a.artistKey,
        label: a.artistName,
        artKey: a.artistKey,
        trackId: a.track.id,
      }));
      label = "Artists";
      placeholder = "Artist name...";
      emptyLabel = "No artists";
    } else {
      const col = this.$output.value?.playlistItems.collection();
      rows = col?.state === "loaded"
        ? [...Playlist.gather(col.data).values()]
          .map((p) => p.name)
          .sort((a, b) => a.localeCompare(b))
          .map((name) => ({
            key: name,
            label: name,
            artKey: `playlist:${name}`,
            trackId: undefined,
          }))
        : [];
      label = "Playlists";
      placeholder = "Playlist name...";
      emptyLabel = "No playlists yet";
    }

    if (filter) {
      rows = rows.filter((r) => r.label.toLowerCase().includes(filter));
      emptyLabel = "No matches";
    }

    return {
      label,
      placeholder,
      emptyLabel,
      sections: groupByLetter(rows),
    };
  }

  /**
   * @param {Function} html
   */
  #renderCatalog(html) {
    const data = this.#catalogData();
    const collapsed = this.#catalogCollapsed.value;

    if (!data) return nothing;

    const currentView = this.#view.value;
    const currentPlaylist = this.$scope.value?.playlist();
    const letters = data.sections.map((s) => s.letter);

    return html`
      <section class="da-catalog ${collapsed ? `da-catalog--collapsed` : ""}">
        <div class="da-catalog__scroll">
          <div class="da-catalog__search">
            <i class="ph-bold ph-magnifying-glass"></i>
            <input
              id="da-playlist-filter"
              type="search"
              placeholder="${data.placeholder}"
              .value="${this.#playlistFilter.value}"
              @input="${this.setPlaylistFilter}"
            />
          </div>

          ${data.sections.length > 0
            ? html`
              <div class="da-catalog__label">${data.label}</div>
              ${data.sections.map(({ letter, rows }) => html`
                <div class="da-playlist-section" data-letter="${letter}">
                  <div class="da-playlist-section__letter">${letter}</div>
                  ${rows.map((row) => {
                    const isActive = currentView.type === "album"
                      ? currentView.albumKey === row.key
                      : currentView.type === "artist"
                      ? currentView.artistKey === row.key
                      : currentView.type === "playlist-tracks" &&
                          currentPlaylist === row.key;
                    return html`
                      <button
                        class="da-playlist-row ${isActive
                        ? `da-playlist-row--active`
                        : ""}"
                        data-art-key="${row.artKey}"
                        data-art-track-id="${row.trackId ?? ""}"
                        data-playlist="${row.key}"
                        @click="${() => this.#activateCatalogRow(row)}"
                        title="${row.label}"
                      >
                        ${this.#renderCatalogThumb(html, row)}
                        <span>${row.label}</span>
                      </button>
                    `;
                  })}
                </div>
              `)}
            `
            : html`
              <div class="da-catalog__empty">
                <p>${data.emptyLabel}</p>
              </div>
            `}
        </div>

        ${letters.length > 1
          ? html`
            <div class="da-catalog__index" aria-hidden="true">
              ${letters.map((letter) => html`
                <button
                  class="da-catalog__index-letter"
                  @click="${() => this.jumpToLetter(letter)}"
                >
                  ${letter === "#" ? "♯" : letter}
                </button>
              `)}
            </div>
          `
          : nothing}
      </section>
    `;
  }

  /**
   * Opens a catalog row: album detail, artist detail or playlist tracks.
   * @param {{ key: string; label: string; artKey: string; trackId: string | undefined }} row
   */
  #activateCatalogRow(row) {
    const mode = this.#catalogMode();

    if (mode === "albums") {
      const item = this.$albumRows().find((a) => a.albumKey === row.key);
      if (item) {
        this.openAlbum({ type: "album", ...item });
      }
    } else if (mode === "artists") {
      const item = this.$artistRows().find((a) => a.artistKey === row.key);
      if (item) {
        this.openArtist({ type: "artist", ...item });
      }
    } else {
      this.setSelectedPlaylist(row.key);
    }
  }

  /**
   * @param {Function} html
   * @param {{ key: string; label: string; artKey: string; trackId: string | undefined }} row
   */
  #renderCatalogThumb(html, row) {
    const artUrl = this.#coverArtCache.get(row.artKey);
    this.#ensureRowArt(row.artKey, () =>
      row.trackId
        ? this.$provider.value?.tracks().find((t) => t.id === row.trackId)
        : this.#resolvePlaylistTrack(row.key));

    return html`
      <div class="da-playlist-thumb">
        ${artUrl
          ? html`<img src="${artUrl}" alt="" loading="lazy" />`
          : html`
            <div class="da-playlist-thumb__placeholder">
              <i class="ph-fill ph-music-notes"></i>
            </div>
          `}
      </div>
    `;
  }

  /**
   * @param {Function} html
   */
  #renderMain(html) {
    const view = this.#view.value;

    // Playlists view with nothing selected yet — prompt instead of a
    // misleading all-tracks list.
    if (view.type === "playlist-tracks" && !this.$scope.value?.playlist()) {
      return html`
        <main class="da-main">
          <div class="da-empty">
            <i class="ph-fill ph-playlist"></i>
            <p>Select a playlist</p>
          </div>
        </main>
      `;
    }

    return html`
      <main class="da-main">
        ${this.#renderHeader(html)}
        ${view.type === "albums" || view.type === "artists"
          ? this.#renderCoverGrid(html)
          : this.#renderTrackList(html)}
      </main>
    `;
  }

  /**
   * @param {Function} html
   */
  #renderHeader(html) {
    const view = this.#view.value;
    const playlist = this.$scope.value?.playlist();
    const searchTerm = this.$scope.value?.searchTerm();

    /** @type {string} */
    let title;
    /** @type {string} */
    let badge;
    /** @type {string} */
    let subtitle;

    if (view.type === "album") {
      title = view.albumName;
      badge = "Album";
      const dur = this.$albumDurations().get(view.albumKey);
      subtitle = [
        view.artist,
        dur?.ms ? formatDuration(dur.ms) : `${dur?.count ?? 0} tracks`,
      ].filter(Boolean).join(" — ");
    } else if (view.type === "artist") {
      title = view.artistName;
      badge = "Artist";
      subtitle = `${view.trackCount} ${view.trackCount === 1 ? "track" : "tracks"}`;
    } else if (view.type === "artists") {
      const groups = this.$sortedArtistGroups();
      const count = groups.reduce((n, g) => n + g.groups.length, 0);
      title = "Artists";
      badge = "";
      subtitle = `${count} ${count === 1 ? "artist" : "artists"}`;
    } else if (view.type === "tracks") {
      title = "Songs";
      badge = "";
      const count = this.$currentTracks().length;
      subtitle = `${count} ${count === 1 ? "song" : "songs"}`;
    } else if (view.type === "playlist-tracks") {
      title = playlist ?? "Playlists";
      badge = "Playlist";
      const count = this.$currentTracks().length;
      subtitle = `${count} ${count === 1 ? "song" : "songs"}`;
    } else {
      title = "Albums";
      badge = "";
      const count = this.$sortedCoverGroups().reduce(
        (n, g) => n + g.groups.length,
        0,
      );
      subtitle = `${count} ${count === 1 ? "release" : "releases"}`;
    }

    return html`
      <div class="da-header">
        <div class="da-header__row">
          <h1 class="da-header__title" title="${title}">${title}</h1>
          ${badge ? html`<span class="da-header__badge">${badge}</span>` : nothing}
        </div>
        <div class="da-header__subtitle">${subtitle}</div>

        <div class="da-header__actions">
          <button class="da-pill" @click="${this.addViewToQueue}">
            <i class="ph-bold ph-list-plus"></i>
            <span>Add to Queue</span>
          </button>

          ${searchTerm
            ? html`
              <button class="da-pill da-pill--active" @click="${this.clearSearch}">
                <i class="ph-bold ph-magnifying-glass"></i>
                <span>${searchTerm}</span>
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
  #renderCoverGrid(html) {
    const view = this.#view.value;
    if (view.type === "artists") return this.#renderArtistsGrid(html);
    return this.#renderAlbumsGrid(html);
  }

  /**
   * @param {Function} html
   */
  #renderAlbumsGrid(html) {
    return this.#renderCardsGrid(html, "albums");
  }

  /**
   * @param {Function} html
   */
  #renderArtistsGrid(html) {
    return this.#renderCardsGrid(html, "artists");
  }

  /**
   * Virtualized cover grid: cards are chunked into fixed-column rows
   * (#rebuildGridItems) which are windowed by scroll position, like the
   * track list.
   * @param {Function} html
   * @param {"albums" | "artists"} mode
   */
  #renderCardsGrid(html, mode) {
    const isAlbums = mode === "albums";
    const groups = isAlbums
      ? this.$sortedCoverGroups()
      : this.$sortedArtistGroups();
    const totalCount = groups.reduce((n, g) => n + g.groups.length, 0);

    if (totalCount === 0) {
      this.#renderedGridStart = -1;
      this.#renderedGridEnd = -1;
      return html`
        <div class="da-covers-panel">
          ${this.#renderEmptyState(html, isAlbums ? "No albums" : "No artists")}
        </div>
      `;
    }

    // Rebuild the virtual item list when the data or layout changed
    if (
      groups !== this.#gridSource || mode !== this.#gridMode ||
      this.#gridLayoutDirty
    ) {
      this.#rebuildGridItems(groups);
      this.#gridSource = groups;
      this.#gridMode = mode;
      this.#gridLayoutDirty = false;
    }

    const { startIndex, endIndex } = this.#computeGridRange();
    this.#renderedGridStart = startIndex;
    this.#renderedGridEnd = endIndex;

    const items = this.#gridItems;
    const totalHeight = this.#gridOffsets[items.length] ?? 0;

    requestAnimationFrame(() => this.#measureGrid());

    return html`
      <div class="da-covers-panel">
        <div class="da-covers-label">${isAlbums ? "Albums" : "Artists"}</div>
        <div class="da-covers-virtual" style="height: ${totalHeight}px;">
          ${repeat(
            items.slice(startIndex, endIndex).map((item, i) => ({
              item,
              top: this.#gridOffsets[startIndex + i],
            })),
            (entry) => entry.item.type === "group"
              ? `group-${entry.item.label}`
              : `row-${mode}-${
                entry.item.items.map((
                  /** @type {any} */ c,
                ) => c.albumKey ?? c.artistKey).join("|")}`,
            (entry) => entry.item.type === "group"
              ? html`<div class="da-cover-group" style="top: ${entry.top}px;">${entry.item.label}</div>`
              : html`
                <div
                  class="da-cover-row"
                  style="top: ${entry.top}px; grid-template-columns: repeat(${this.#gridCols}, 1fr);"
                >
                  ${entry.item.items.map((/** @type {any} */ item) =>
                    this.#renderGridCard(html, mode, item))}
                </div>
              `,
          )}
        </div>
      </div>
    `;
  }

  /**
   * @param {Function} html
   * @param {"albums" | "artists"} mode
   * @param {any} item  CoverGroup or ArtistGroup
   */
  #renderGridCard(html, mode, item) {
    const isAlbums = mode === "albums";
    const key = isAlbums ? item.albumKey : item.artistKey;
    this.#fetchAlbumArt(key, item.track);
    const artUrl = this.#coverArtCache.get(key);

    let time;
    if (isAlbums) {
      const dur = this.$albumDurations().get(item.albumKey);
      time = html`<span class="da-cover-time">${
        dur?.ms ? formatDuration(dur.ms) : `${dur?.count ?? 0} tracks`
      }</span>`;
    }

    const onOpen = isAlbums
      ? () => this.openAlbum({
        type: "album",
        albumKey: item.albumKey,
        albumName: item.albumName,
        artist: item.artist,
        track: item.track,
      })
      : () => this.openArtist({
        type: "artist",
        artistKey: item.artistKey,
        artistName: item.artistName,
        trackCount: item.trackCount,
        track: item.track,
      });

    return html`
      <div
        class="da-cover-card"
        data-cover-key="${key}"
        data-cover-track-id="${item.track.id}"
        @click="${onOpen}"
        title="${isAlbums ? `${item.albumName} — ${item.artist}` : item.artistName}"
      >
        <div class="da-cover-art">
          ${artUrl
            ? html`
              <img
                src="${artUrl}"
                alt="${isAlbums ? item.albumName : item.artistName}"
                loading="lazy"
                @error="${() => {
                  this.#coverArtCache.set(key, null);
                  this.#scheduleArtRender();
                }}"
              />
            `
            : html`
              <div class="da-cover-placeholder">
                <i class="ph-fill ${isAlbums ? `ph-music-notes` : `ph-user`}"></i>
              </div>
            `}
        </div>
        <div class="da-cover-info">
          <span class="da-cover-album">${isAlbums ? item.albumName : item.artistName}</span>
          ${isAlbums
            ? html`
              <span class="da-cover-artist">${item.artist}</span>
              ${time}
            `
            : html`
              <span class="da-cover-artist">${item.trackCount}
                ${item.trackCount === 1 ? `track` : `tracks`}</span>
            `}
        </div>
      </div>
    `;
  }

  /**
   * @param {Function} html
   * @param {string} message
   */
  #renderEmptyState(html, message) {
    const isLoading = this.#isCollectionLoading();

    return html`
      <div class="da-empty">
        <i class="ph-fill ${isLoading ? `ph-vinyl-record` : `ph-music-notes`}"></i>
        <p>${isLoading ? `Loading your library…` : message}</p>
      </div>
    `;
  }

  /**
   * @param {Function} html
   */
  #renderTrackList(html) {
    const tracks = this.$currentTracks();

    if (tracks.length === 0) {
      this.#itemCount = 0;
      this.#renderedTracks = undefined;
      this.#renderedStartIndex = -1;
      this.#renderedEndIndex = -1;
      return html`
        <div class="da-tracks-list">
          <div class="da-tracks-panel">
            ${this.#renderEmptyState(html, "No tracks")}
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

    // Column sort state (hidden when an ordered playlist dictates the order)
    const playlistOrdered = /** @type {any} */ (this.$provider.value)
      ?.playlistIsOrdered?.() ?? false;
    const sortBy = this.$scope.value?.sortBy() ?? [];
    const sortDirection = this.$scope.value?.sortDirection();
    const sortedColumn = playlistOrdered
      ? undefined
      : Object.entries(COLUMN_SORT).find(
        ([, v]) => JSON.stringify(v) === JSON.stringify(sortBy),
      )?.[0];

    const ariaSort = /** @param {string} col */ (col) =>
      sortedColumn === col
        ? (sortDirection === "desc" ? "descending" : "ascending")
        : "none";

    const sortIcon = /** @param {string} col */ (col) =>
      sortedColumn === col
        ? html`<i class="ph-bold ph-caret-${
          sortDirection === "desc" ? "down" : "up"
        }"></i>`
        : nothing;

    return html`
      <div class="da-tracks-list">
        <div class="da-tracks-header">
          <button
            class="da-track-header__title ${sortedColumn === "title"
            ? `da-track-header--active`
            : ""}"
            aria-sort="${ariaSort("title")}"
            @click="${() => this.sortByColumn("title")}"
          >
            Title ${sortIcon("title")}
          </button>
          <button
            class="da-track-header__artist ${sortedColumn === "artist"
            ? `da-track-header--active`
            : ""}"
            aria-sort="${ariaSort("artist")}"
            @click="${() => this.sortByColumn("artist")}"
          >
            Artist ${sortIcon("artist")}
          </button>
          <button
            class="da-track-header__album ${sortedColumn === "album"
            ? `da-track-header--active`
            : ""}"
            aria-sort="${ariaSort("album")}"
            @click="${() => this.sortByColumn("album")}"
          >
            Album ${sortIcon("album")}
          </button>
          <div class="da-track-header__time">
            <i class="ph-bold ph-clock"></i>
          </div>
          <div class="da-track-header__actions"></div>
        </div>
        <div class="da-tracks-panel">
          <div class="da-tracks-virtual" style="height: ${totalSize}px;">
            ${repeat(
              tracks.slice(startIndex, endIndex).map((track, i) => ({
                track,
                index: startIndex + i,
                top: (startIndex + i) * TRACK_ROW_STRIDE,
              })),
              (entry) => `da-tr-${entry.track.id}`,
              (entry) => this.#renderTrackRow(html, entry.track, entry.top),
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
   */
  #renderTrackRow(html, track, top) {
    const albumKey = String(track.tags?.album ?? "").toLowerCase();
    this.#fetchAlbumArt(albumKey, track);
    const artUrl = this.#coverArtCache.get(albumKey);

    const favKey = `${String(track.tags?.artist ?? "").toLowerCase()}|${
      String(track.tags?.title ?? "").toLowerCase()
    }`;
    const isFav = this.$favouritesSet().has(favKey);

    const currentId = this.currentTrack()?.id;
    const isCurrent = currentId === track.id;

    return html`
      <div
        class="da-track-row ${isCurrent ? `da-track-row--current` : ""}"
        style="transform: translateY(${top}px);"
        @dblclick="${() => this.playTrack(track)}"
      >
        <div class="da-track__title">
          <div class="da-track__art">
            ${artUrl
              ? html`<img src="${artUrl}" alt="" loading="lazy" />`
              : html`
                <div class="da-track-art-placeholder">
                  <i class="ph-fill ph-music-notes"></i>
                </div>
              `}
          </div>
          <span class="da-track__title-text">${trackTitle(track)}</span>
          ${isCurrent
            ? html`<i class="ph-fill ph-speaker-high da-track__playing"></i>`
            : nothing}
        </div>
        <div class="da-track__artist">
          <span>${track.tags?.artist ?? ""}</span>
        </div>
        <div class="da-track__album">
          <span>${track.tags?.album ?? ""}</span>
        </div>
        <div class="da-track__time">${formatDuration(track.stats?.duration)}</div>
        <div class="da-track__actions">
          <button
            class="da-track__action"
            @click="${(/** @type {Event} */ e) => {
              e.stopPropagation();
              this.addToQueue(track);
            }}"
            title="Add to queue"
          >
            <i class="ph-bold ph-plus"></i>
          </button>
          <button
            class="da-track__action ${isFav ? `da-track__action--active` : ""}"
            @click="${(/** @type {Event} */ e) => {
              e.stopPropagation();
              this.toggleFavourite(track);
            }}"
            title="${isFav ? `Remove from favourites` : `Add to favourites`}"
          >
            <i class="${isFav ? `ph-fill ph-heart` : `ph-bold ph-heart`}"></i>
          </button>
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
 * @param {Uint8Array} bytes
 * @returns {string}
 */
function detectMime(bytes) {
  if (bytes[0] === 0xff && bytes[1] === 0xd8) return "image/jpeg";
  if (bytes[0] === 0x89 && bytes[1] === 0x50) return "image/png";
  if (bytes.length > 11 && bytes[8] === 0x57 && bytes[9] === 0x45 &&
    bytes[10] === 0x42 && bytes[11] === 0x50) {
    return "image/webp";
  }
  if (bytes[0] === 0x47 && bytes[1] === 0x49) return "image/gif";
  return "image/jpeg";
}

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = Browser;
export const NAME = "da-browser";

defineElement(NAME, CLASS);
