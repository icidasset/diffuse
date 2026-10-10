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

// Catalog column virtual scroll (fixed row heights, like the track list).
// These are only initial estimates — the real rendered heights are
// measured per render, since rem-based sizes scale with the root font.
const CATALOG_ROW_STRIDE = 44; // 2.75rem
const CATALOG_LETTER_HEIGHT = 44; // 2.75rem
const CATALOG_OVERSCAN = 10;

// UI state is keyed per facet group (deck): da.theme.albums-app/<group>/…
const STORAGE_PREFIX = "da.theme.albums-app";

const collator = new Intl.Collator();

/**
 * Restores the last active view for a deck group from localStorage. The
 * representative `track` of detail views is not serialized — it's
 * re-attached from the library once tracks are available.
 * @param {string} group
 * @returns {View | null}
 */
function restoreView(group) {
  try {
    const raw = localStorage.getItem(`${STORAGE_PREFIX}/${group}/view`);
    if (!raw) return null;

    const parsed = JSON.parse(raw);
    if (typeof parsed?.type !== "string") return null;

    const validTypes = [
      "albums",
      "artists",
      "tracks",
      "playlist-tracks",
      "album",
      "artist",
      "group-tracks",
    ];

    if (!validTypes.includes(parsed.type)) return null;

    if (parsed.type === "group-tracks") {
      if (parsed.groupBy !== "tags.year" && parsed.groupBy !== "createdAt") {
        return null;
      }
    }

    return /** @type {View} */ (parsed);
  } catch {
    return null;
  }
}

/**
 * Restores the catalog filter for a deck group across reloads.
 * @param {string} group
 * @returns {string}
 */
function restoreFilter(group) {
  try {
    return localStorage.getItem(`${STORAGE_PREFIX}/${group}/filter`) ?? "";
  } catch {
    return "";
  }
}

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
 * @typedef {{ type: "group-tracks"; groupBy: "tags.year" | "createdAt"; value: string | undefined }} GroupTracksView
 * @typedef {{ type: "albums" } | { type: "artists" } | { type: "tracks" } | { type: "playlist-tracks" } | GroupTracksView | AlbumItem | ArtistItem } View
 */

/** Library rail entries, ordered alphabetically. */
/** @type {{ type: "albums" | "artists" | "years" | "added" | "playlists" | "tracks"; label: string; icon: string }[]} */
const LIBRARY_ITEMS = [
  { type: "added", label: "Added on", icon: "ph-clock" },
  { type: "albums", label: "Albums", icon: "ph-vinyl-record" },
  { type: "artists", label: "Artists", icon: "ph-microphone-stage" },
  { type: "playlists", label: "Playlists", icon: "ph-playlist" },
  { type: "tracks", label: "Songs", icon: "ph-music-notes" },
  { type: "years", label: "Years", icon: "ph-calendar" },
];

const MONTHS = [
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December",
];

/**
 * "2025-03" → "March 2025"
 * @param {string} key
 */
function monthLabel(key) {
  const month = Number(key.slice(5, 7));
  return `${MONTHS[month - 1] ?? ""} ${key.slice(0, 4)}`.trim();
}

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

  /**
   * The facet sets the deck's `group` attribute right after the element
   * upgrades, so the per-group UI state (last view, catalog filter) is
   * loaded from localStorage at that point, not at construction.
   * @override
   * @param {string} name
   * @param {string} oldValue
   * @param {string} newValue
   */
  attributeChangedCallback(name, oldValue, newValue) {
    if (name === "group" && newValue && oldValue !== newValue) {
      batch(() => {
        const view = restoreView(newValue);
        if (view) this.#view.value = view;
        this.#playlistFilter.value = restoreFilter(newValue);
      });
    }

    super.attributeChangedCallback(name, oldValue, newValue);
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

  $repeatShuffle = signal(
    /** @type {import("~/components/engine/repeat-shuffle/element.js").CLASS | undefined} */ (undefined),
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

  // Restored per deck group once the `group` attribute lands (see
  // attributeChangedCallback) — it isn't known at construction time
  #view = signal(/** @type {View} */ ({ type: "albums" }));

  /** Tracks picked via their cover in list views (insertion-ordered). */
  #selectedTracks = signal(/** @type {Set<string>} */ (new Set()));

  #catalogCollapsed = signal(false);

  #playlistFilter = signal("");

  /** Live value of the rail search field (the × button tracks typing). */
  #railSearchValue = signal("");

  #history = signal(/** @type {View[]} */ ([]));
  #future = signal(/** @type {View[]} */ ([]));

  // Mini player audio state — loading is debounced so quick track
  // switches don't flash the spinner
  /** @type {ReturnType<typeof setTimeout> | undefined} */
  #isLoadingTimeout = undefined;

  #isLoading = signal(false);

  #audioError = signal(false);

  // Cover art cache (albums, artists and playlist thumbnails share it)
  /** @type {Map<string, string | null | undefined>} */
  #coverArtCache = new Map();
  /** @type {Set<string>} */
  #pendingArtFetch = new Set();
  /** @type {{ key: string; track: Track }[]} */
  #artFetchQueue = [];
  #artFetchActive = 0;
  #artRenderScheduled = false;

  // Catalog column virtual scroll state
  #catalogScrollTop = 0;
  #catalogViewportHeight = window.innerHeight;
  #catalogTopOffset = 0;
  #catalogRowStride = CATALOG_ROW_STRIDE;
  #catalogLetterHeight = CATALOG_LETTER_HEIGHT;
  /** @type {{ letter: string; rows: any[] }[] | null} */
  #catalogSections = null;
  /** @type {({ type: "letter"; label: string } | { type: "row"; row: any })[]} */
  #catalogItems = [];
  /** @type {number[]} */
  #catalogOffsets = [];
  #renderedCatalogStart = -1;
  #renderedCatalogEnd = -1;

  // Cover grid virtual scroll state
  #gridScrollTop = 0;
  #gridViewportHeight = window.innerHeight;
  #gridPanelWidth = 0;
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
    if (view.type === "group-tracks") {
      return this.#groupTracks(view);
    }
    return this.$provider.value?.tracks() ?? [];
  });

  /**
   * Tracks belonging to the selected year / month of a group view.
   * @param {{ groupBy: "tags.year" | "createdAt"; value: string | undefined }} view
   * @returns {Track[]}
   */
  #groupTracks(view) {
    if (!view.value) return [];
    const groups = view.groupBy === "createdAt"
      ? this.$addedGroups()
      : this.$yearGroups();
    return groups.find((g) => g.label === view.value)?.tracks ?? [];
  }

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

  /** Tracks grouped by release year, newest first. */
  $yearGroups = computed(() => {
    const map = new Map();
    for (const t of this.$provider.value?.tracks() ?? []) {
      const year = t.tags?.year;
      if (year == null) continue;
      const key = String(year);
      if (!map.has(key)) map.set(key, []);
      map.get(key)?.push(t);
    }
    return [...map.entries()]
      .sort((a, b) => collator.compare(b[0], a[0]))
      .map(([label, groupTracks]) => ({ label, tracks: groupTracks }));
  });

  /** Tracks grouped by the month they were processed, newest first. */
  $addedGroups = computed(() => {
    const map = new Map();
    for (const t of this.$provider.value?.tracks() ?? []) {
      const iso = t.createdAt;
      if (!iso) continue;
      const key = `${iso.slice(0, 4)}-${iso.slice(5, 7)}`;
      if (!map.has(key)) map.set(key, []);
      map.get(key)?.push(t);
    }
    return [...map.entries()]
      .sort((a, b) => collator.compare(b[0], a[0]))
      .map(([key, groupTracks]) => ({
        label: monthLabel(key),
        tracks: groupTracks,
      }));
  });

  /**
   * Representative (first matching) track per playlist, computed in one
   * pass. Criteria shapes are deduped across playlists, so the cost is
   * O(tracks × distinct shapes) instead of a per-playlist library filter.
   */
  $playlistFirstTracks = computed(() => {
    const col = this.$output.value?.playlistItems.collection();
    const tracks = this.$provider.value?.tracks() ?? [];

    /** @type {Map<string, Track | undefined>} */
    const first = new Map();
    if (!col || col.state !== "loaded" || !tracks.length) return first;

    const items = col.data;
    if (!items.length) return first;

    // Dedupe criteria shapes across playlists: shape key → fields +
    // value key → playlists using it
    const shapes = /**
      @type {Map<string, { fields: { parts: string[]; transformations: string[] | undefined }[]; keys: Map<string, Set<string>> }>}
    */ (new Map());
    for (const item of items) {
      const shapeKey = item.criteria
        .map((c) => `${c.field}\0${(c.transformations ?? []).join(",")}`)
        .join("\0\0");

      let shape = shapes.get(shapeKey);
      if (!shape) {
        shape = {
          fields: item.criteria.map((c) => ({
            parts: c.field.split("."),
            transformations: c.transformations,
          })),
          keys: new Map(),
        };
        shapes.set(shapeKey, shape);
      }

      const valueKey = item.criteria
        .map((c) => Playlist.transform(c.value, c.transformations))
        .join("\0");

      let playlists = shape.keys.get(valueKey);
      if (!playlists) {
        playlists = new Set();
        shape.keys.set(valueKey, playlists);
      }
      playlists.add(item.playlist);
    }

    // One pass over the tracks attributes each to the first playlist
    // (in track order) that matches it
    for (const track of tracks) {
      for (const shape of shapes.values()) {
        const valueKey = shape.fields
          .map(({ parts, transformations }) =>
            Playlist.transform(
              parts.reduce((v, f) => v?.[f], /** @type {any} */ (track)),
              transformations,
            )
          )
          .join("\0");

        const playlists = shape.keys.get(valueKey);
        if (!playlists) continue;

        for (const name of playlists) {
          if (!first.has(name)) first.set(name, track);
        }
      }
    }

    return first;
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

    /** @type {import("~/components/orchestrator/controller/element.js").CLASS | null} */
    const controller = queryOptional(
      this,
      "controller-orchestrator-selector",
    );

    /** @type {import("~/components/engine/repeat-shuffle/element.js").CLASS | null} */
    const repeatShuffle = queryOptional(
      this,
      "repeat-shuffle-engine-selector",
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

    if (repeatShuffle) {
      whenElementsDefined({ repeatShuffle }).then(() => {
        this.$repeatShuffle.value = repeatShuffle;
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

    // Remember the current screen across reloads, keyed per deck group
    this.effect(() => {
      const view = this.#view.value;
      // Don't persist anything until the deck identity is known
      if (!this.hasAttribute("group")) return;
      try {
        const { track, ...rest } = /** @type {any} */ (view);
        localStorage.setItem(
          `${STORAGE_PREFIX}/${this.group}/view`,
          JSON.stringify(rest),
        );
      } catch {
        // storage unavailable — non-fatal
      }
    });

    // Remember the catalog filter across reloads, keyed per deck group
    this.effect(() => {
      const filter = this.#playlistFilter.value;
      if (!this.hasAttribute("group")) return;
      try {
        const key = `${STORAGE_PREFIX}/${this.group}/filter`;
        if (filter) localStorage.setItem(key, filter);
        else localStorage.removeItem(key);
      } catch {
        // storage unavailable — non-fatal
      }
    });

    // Keep the scope engine's group-by in lockstep with the view: the
    // Years / Added on sections own it, and every other view unsets it.
    // The scope value is persisted (and replicated across scopes), and
    // scoped-tracks re-sorts `tracks()` by the group key while it's set
    // — so a stale value would otherwise keep other views grouped.
    this.effect(() => {
      const view = this.#view.value;
      untracked(() => {
        this.$scope.value?.setGroupBy(
          view.type === "group-tracks" ? view.groupBy : undefined,
        );
      });
    });

    // Mini player loading / error state. The loading spinner only
    // surfaces after a short delay so fast track switches don't flash
    // it; errors surface immediately (pattern from blur / winamp).
    this.effect(() => {
      const now = !!this.$controller.value?.$queue.value?.now();
      const loadingState = this.$controller.value?.audio()?.loadingState();
      const isError = now && typeof loadingState === "object" &&
        loadingState !== null && "error" in loadingState;
      const isLoading = now && !isError && loadingState !== "loaded";

      if (this.#isLoadingTimeout) clearTimeout(this.#isLoadingTimeout);

      if (isLoading) {
        this.#isLoadingTimeout = setTimeout(
          () => this.#isLoading.value = true,
          2000,
        );
      } else {
        this.#isLoading.value = false;
      }

      this.#audioError.value = isError;
    });

    // Re-attach the representative track to a restored detail view once
    // the library is available; fall back to the overview when the
    // album / artist no longer exists
    this.effect(() => {
      const tracks = this.$provider.value?.tracks() ?? [];
      if (!tracks.length) return;

      const view = this.#view.value;
      if (view.type !== "album" && view.type !== "artist") return;
      if (view.track) return;

      untracked(() => {
        if (view.type === "album") {
          const list = this.$tracksByAlbum().get(view.albumKey);
          if (list?.length) {
            this.#view.value = { ...view, track: list[0] };
          } else {
            this.#view.value = { type: "albums" };
          }
        } else {
          const list = this.$tracksByArtist().get(view.artistKey);
          if (list?.length) {
            this.#view.value = { ...view, track: list[0] };
          } else {
            this.#view.value = { type: "artists" };
          }
        }
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
    if (this.#isLoadingTimeout) {
      clearTimeout(this.#isLoadingTimeout);
      this.#isLoadingTimeout = undefined;
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
    if (view.type === "group-tracks" && view.value) {
      return this.#groupTracks(view);
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
    this.#selectedTracks.value = new Set();
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
    this.#history.value = [...this.#history.value, this.#view.value];
    this.#future.value = fut.slice(1);
    this.#view.value = next;
    this.#renderedTracks = undefined;
  };

  /** Tracks the rail search field's live value so the × can show/hide. */
  updateRailSearchValue = () => {
    /** @type {HTMLInputElement | null} */
    const input = this.root().querySelector("#da-search-input");
    this.#railSearchValue.value = input?.value ?? "";
  };

  /**
   * @param {string | undefined} playlist
   */
  setSelectedPlaylist = (playlist) => {
    this.$scope.value?.setPlaylist(playlist);
    this.#selectedTracks.value = new Set();
    this.#view.value = { type: "playlist-tracks" };
    this.#renderedTracks = undefined;
  };

  /**
   * The library rail item a view belongs to.
   * @param {View} view
   * @returns {"albums" | "artists" | "years" | "added" | "playlists" | "tracks"}
   */
  #railItemFor(view) {
    if (view.type === "group-tracks") {
      return view.groupBy === "createdAt" ? "added" : "years";
    }
    return RAIL_FOR_VIEW[view.type];
  }

  /**
   * @param {"albums" | "artists" | "years" | "added" | "playlists" | "tracks"} type
   */
  #browse(type) {
    const current = this.#view.value;
    if (this.#railItemFor(current) === type) return;

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
    else if (type === "years") {
      this.#navigateTo({
        type: "group-tracks",
        groupBy: "tags.year",
        value: undefined,
      });
    } else if (type === "added") {
      this.#navigateTo({
        type: "group-tracks",
        groupBy: "createdAt",
        value: undefined,
      });
    } else if (type === "playlists") {
      this.#navigateTo({ type: "playlist-tracks" });
    } else this.#navigateTo({ type: "tracks" });
  }

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
   * Tracks picked via their cover in the current list view, in list
   * order. Falls back to every track of the view when nothing is
   * selected.
   * @returns {Track[]}
   */
  #queueTargets() {
    const selected = this.#selectedTracks.value;
    const tracks = this.#currentViewTracks();
    return selected.size
      ? tracks.filter((t) => selected.has(t.id))
      : tracks;
  }

  /**
   * Appends the selected tracks (or the whole view) to the end of the
   * queue, then clears the selection.
   */
  addViewToQueue = () => {
    const tracks = this.#queueTargets();
    if (!tracks.length) return;
    this.$queue.value?.add({ trackIds: tracks.map((t) => t.id) });
    this.#selectedTracks.value = new Set();
  };

  /**
   * Inserts the selected tracks (or the whole view) right after the
   * currently playing track, then clears the selection.
   */
  playNext = () => {
    const tracks = this.#queueTargets();
    if (!tracks.length) return;
    this.$queue.value?.add({ inFront: true, trackIds: tracks.map((t) => t.id) });
    this.#selectedTracks.value = new Set();
  };

  /**
   * Toggles a track's selection (cover click in list views).
   * @param {Track} track
   */
  toggleTrackSelection = (track) => {
    const selected = new Set(this.#selectedTracks.value);
    selected.has(track.id) ? selected.delete(track.id) : selected.add(track.id);
    this.#selectedTracks.value = selected;
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
    this.#railSearchValue.value = "";
    this.$scope.value?.setSearchTerm(undefined);
  };

  /** Clears the catalog column filter (the input re-renders from the signal). */
  clearCatalogFilter = () => {
    this.#playlistFilter.value = "";
  };

  setPlaylistFilter = () => {
    /** @type {HTMLInputElement | null} */
    const input = this.root().querySelector("#da-playlist-filter");
    this.#playlistFilter.value = input?.value ?? "";
  };

  /**
   * @param {string} letter
   */
  jumpToLetter(letter) {
    const panel = this.root().querySelector(".da-catalog__scroll");
    if (!panel) return;

    const index = this.#catalogItems.findIndex(
      (item) => item.type === "letter" && item.label === letter,
    );
    if (index < 0) return;

    panel.scrollTo({
      top: this.#catalogOffsets[index] + this.#catalogTopOffset,
      behavior: "smooth",
    });
  }

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

  /** Retry the current track after a load error, resuming at its position. */
  reload = () => {
    const audioId = this.$controller.value?.$queue.value?.now()?.id;
    if (audioId) {
      const progress = this.$controller.value?.audio()?.progress();
      this.$controller.value?.$audio.value?.reload({
        audioId,
        play: true,
        progress,
      });
    }
  };

  next = () => {
    this.$controller.value?.$queue.value?.shift();
  };

  toggleShuffle = () => {
    const rs = this.$repeatShuffle.value;
    if (rs) rs.setShuffle(!rs.shuffle());
  };

  toggleRepeat = () => {
    const rs = this.$repeatShuffle.value;
    if (rs) rs.setRepeat(!rs.repeat());
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
   * Measures the rendered track-row height — rem-based sizes scale with
   * the root font size, so the hardcoded px stride is only an estimate.
   * Without this the row borders drift out of the content's rhythm.
   */
  #measureTracks() {
    const row = /** @type {HTMLElement | null} */ (
      this.root().querySelector(".da-track-row")
    );
    if (!row?.offsetHeight) return;

    const height = row.offsetHeight;
    if (height === this.#rowStride) return;

    this.#rowStride = height;
    this.#renderedStartIndex = -1;
    this.#renderedEndIndex = -1;
    this.forceRender();
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
        // Negative-cache only when the library is actually loaded — an
        // empty library means the tracks haven't arrived yet, in which
        // case the re-render on their arrival retries this
        if (this.$provider.value?.tracks().length) {
          this.#coverArtCache.set(key, null);
          this.#scheduleArtRender();
        }
        return;
      }
      this.#fetchAlbumArt(key, track);
    });
  }

  // CATALOG VIRTUAL SCROLL

  /**
   * Flattens the letter-grouped catalog sections into a virtual item list
   * (letter headers + one item per row) with cumulative height offsets.
   * @param {{ letter: string; rows: any[] }[]} sections
   */
  #rebuildCatalogItems(sections) {
    /** @type {({ type: "letter"; label: string } | { type: "row"; row: any })[]} */
    const items = [];

    for (const { letter, rows } of sections) {
      if (letter) items.push({ type: "letter", label: letter });
      for (const row of rows) items.push({ type: "row", row });
    }

    this.#catalogItems = items;

    const offsets = new Array(items.length + 1);
    offsets[0] = 0;
    let acc = 0;
    for (let i = 0; i < items.length; i++) {
      acc += items[i].type === "letter"
        ? this.#catalogLetterHeight
        : this.#catalogRowStride;
      offsets[i + 1] = acc;
    }
    this.#catalogOffsets = offsets;
  }

  /**
   * Visible window of catalog items for the current scroll position.
   * Pure math — no DOM reads (the scroll-panel offset is measured per
   * render).
   * @returns {{ startIndex: number; endIndex: number }}
   */
  #computeCatalogRange() {
    const virtualTop = this.#catalogScrollTop - this.#catalogTopOffset;
    const over = CATALOG_OVERSCAN * this.#catalogRowStride;

    const items = this.#catalogItems;
    const offsets = this.#catalogOffsets;
    const top = virtualTop - over;
    const bottom = virtualTop + this.#catalogViewportHeight + over;

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

  #renderIfCatalogWindowChanged() {
    if (!this.#catalogItems.length) return;

    const { startIndex, endIndex } = this.#computeCatalogRange();
    if (
      startIndex === this.#renderedCatalogStart &&
      endIndex === this.#renderedCatalogEnd
    ) return;

    this.forceRender();
  }

  /**
   * Measures the catalog scroll panel: the virtual container's offset
   * within it (search + heading above), its viewport height, and the
   * REAL rendered row/letter heights (rem-based sizes scale with the
   * root font size — the px estimates are only fallbacks).
   */
  #measureCatalog() {
    const virtual = /** @type {HTMLElement | null} */ (
      this.root().querySelector(".da-catalog-virtual")
    );
    const panel = /** @type {HTMLElement | null} */ (
      this.root().querySelector(".da-catalog__scroll")
    );
    if (!virtual || !panel) return;

    this.#catalogTopOffset = virtual.offsetTop - panel.offsetTop;
    this.#catalogViewportHeight = panel.clientHeight;

    let dirty = false;

    const letter = /** @type {HTMLElement | null} */ (
      this.root().querySelector(".da-playlist-section__letter")
    );
    if (letter?.offsetHeight) {
      dirty = this.#catalogLetterHeight !== letter.offsetHeight;
      this.#catalogLetterHeight = letter.offsetHeight;
    }

    const row = /** @type {HTMLElement | null} */ (
      this.root().querySelector(".da-playlist-row")
    );
    if (row?.offsetHeight) {
      dirty = dirty || this.#catalogRowStride !== row.offsetHeight;
      this.#catalogRowStride = row.offsetHeight;
    }

    if (dirty) {
      this.#rebuildCatalogItems(this.#catalogSections ?? []);
      this.forceRender();
    }

    this.#renderIfCatalogWindowChanged();
  }

  /**
   * The playlist thumbnails' representative tracks come from the
   * `$playlistFirstTracks` computed — a single pass over the library.
   */

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
        } else if (panel.classList.contains("da-catalog__scroll")) {
          this.#catalogScrollTop = panel.scrollTop;
          this.#renderIfCatalogWindowChanged();
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
          // React to width changes too: the column count depends on it,
          // and a render is needed even when the visible row range
          // doesn't move, so the grid gets re-measured
          const width = panel.clientWidth;
          const changed = height !== this.#gridViewportHeight ||
            width !== this.#gridPanelWidth;
          this.#gridViewportHeight = height;
          this.#gridPanelWidth = width;

          if (changed) this.forceRender();
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
    requestAnimationFrame(() => {
      this.#syncViewportObserver();
      this.#measureCatalog();
      this.#measureTracks();
    });

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
    if (view.type === "group-tracks") {
      return view.value ?? (view.groupBy === "createdAt" ? "Added on" : "Years");
    }
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
    const railItem = this.#railItemFor(view);

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
              @input="${this.updateRailSearchValue}"
              @change="${this.setSearchTerm}"
            />
            ${(this.#railSearchValue.value ||
              this.$scope.value?.searchTerm() ||
              "").length
              ? html`
                <button
                  class="da-search-clear"
                  @click="${this.clearSearch}"
                  title="Clear search"
                >
                  <i class="ph-fill ph-x-circle"></i>
                </button>
              `
              : nothing}
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
                @click="${() => this.#browse(item.type)}"
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
    const isShuffle = this.$repeatShuffle.value?.shuffle() ?? false;
    const isRepeat = this.$repeatShuffle.value?.repeat() ?? false;
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

    const isLoading = this.#isLoading.value;
    const isError = this.#audioError.value;

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
            ${isLoading
              ? html`
                <div class="da-mini__art-loading" title="Loading ...">
                  <i class="ph-bold ph-spinner-gap"></i>
                </div>
              `
              : nothing}
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
          <button
            @click="${this.toggleShuffle}"
            data-enabled="${isShuffle ? `t` : `f`}"
            title="Toggle shuffle"
          >
            <i class="ph-${isShuffle ? `fill` : `bold`} ph-shuffle"></i>
          </button>
          <button @click="${this.previous}" title="Previous track">
            <i class="ph-bold ph-skip-back"></i>
          </button>
          <button
            class="da-mini__play ${isError ? `da-mini__play--error` : ""}"
            @click="${isError ? this.reload : this.playPause}"
            title="${isError ? `Reload` : isPlaying ? `Pause` : `Play`}">
            <i class="${isError
              ? `ph-fill ph-warning-circle`
              : `ph-bold ${isPlaying ? `ph-pause` : `ph-play`}`}"></i>
          </button>
          <button @click="${this.next}" title="Next track">
            <i class="ph-bold ph-skip-forward"></i>
          </button>
          <button
            @click="${this.toggleRepeat}"
            data-enabled="${isRepeat ? `t` : `f`}"
            title="Toggle repeat"
          >
            <i class="ph-${isRepeat ? `fill` : `bold`} ph-repeat"></i>
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
  /**
   * Which content the catalog column shows for the current view.
   * `null` means the column is hidden (Songs view).
   * @returns {"albums" | "artists" | "years" | "added" | "playlists" | null}
   */
  #catalogMode() {
    const railItem = this.#railItemFor(this.#view.value);
    return railItem === "tracks" ? null : railItem;
  }

  /**
   * Filtered, letter-grouped rows for the catalog column. Memoized so
   * the per-render cost stays constant regardless of library size.
   */
  $catalogData = computed(() => {
    const mode = this.#catalogMode();
    if (!mode) return null;

    const filter = this.#playlistFilter.value.trim().toLowerCase();

    /** @type {{ key: string; label: string; artKey: string | undefined; track: Track | undefined }[]} */
    let rows;
    let label;
    let placeholder;
    let emptyLabel;

    if (mode === "albums") {
      rows = this.$albumRows().map((a) => ({
        key: a.albumKey,
        label: a.albumName,
        artKey: a.albumKey,
        track: a.track,
      }));
      label = "Albums";
      placeholder = "Album name...";
      emptyLabel = "No albums";
    } else if (mode === "artists") {
      rows = this.$artistRows().map((a) => ({
        key: a.artistKey,
        label: a.artistName,
        artKey: a.artistKey,
        track: a.track,
      }));
      label = "Artists";
      placeholder = "Artist name...";
      emptyLabel = "No artists";
    } else if (mode === "years" || mode === "added") {
      const groups = mode === "years" ? this.$yearGroups() : this.$addedGroups();
      rows = groups.map((g) => ({
        key: g.label,
        label: g.label,
        artKey: undefined,
        track: undefined,
      }));
      label = mode === "years" ? "Years" : "Months";
      placeholder = mode === "years" ? "Year..." : "Month...";
      emptyLabel = mode === "years" ? "No years" : "No months";
    } else {
      // Reads the tracks signal too, so the playlist list re-renders
      // (and thumbnails retry) when the library arrives
      const _firstTracks = this.$playlistFirstTracks();
      const col = this.$output.value?.playlistItems.collection();
      rows = col?.state === "loaded"
        ? [...Playlist.gather(col.data).values()]
          .map((p) => p.name)
          .sort(collator.compare)
          .map((name) => ({
            key: name,
            label: name,
            artKey: `playlist:${name}`,
            track: undefined,
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
      flat: mode === "years" || mode === "added",
      sections: mode === "years" || mode === "added"
        ? [{ letter: "", rows }]
        : groupByLetter(rows),
    };
  });

  /**
   * @param {Function} html
   */
  #renderCatalog(html) {
    const data = this.$catalogData();
    const collapsed = this.#catalogCollapsed.value;

    if (!data) return nothing;

    // Rebuild the virtual item list when the sections change
    if (data.sections !== this.#catalogSections) {
      this.#catalogSections = data.sections;
      this.#rebuildCatalogItems(data.sections);
    }

    const { startIndex, endIndex } = this.#computeCatalogRange();
    this.#renderedCatalogStart = startIndex;
    this.#renderedCatalogEnd = endIndex;

    const items = this.#catalogItems;
    const totalHeight = this.#catalogOffsets[items.length] ?? 0;
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
            ${this.#playlistFilter.value
              ? html`
                <button
                  class="da-search-clear"
                  @click="${this.clearCatalogFilter}"
                  title="Clear filter"
                >
                  <i class="ph-fill ph-x-circle"></i>
                </button>
              `
              : nothing}
          </div>

          ${data.sections.length > 0
            ? html`
              <div class="da-catalog__label">${data.label}</div>
              <div
                class="da-catalog-virtual"
                style="height: ${totalHeight}px;"
              >
                ${repeat(
                  items.slice(startIndex, endIndex).map((item, i) => ({
                    item,
                    top: this.#catalogOffsets[startIndex + i],
                  })),
                  (entry) => entry.item.type === "letter"
                    ? `letter-${entry.item.label}`
                    : `row-${entry.item.row.key}`,
                  (entry) => entry.item.type === "letter"
                    ? html`<div
                        class="da-playlist-section__letter"
                        style="top: ${entry.top}px;"
                      >${entry.item.label}</div>`
                    : this.#renderCatalogRow(html, entry.item.row, entry.top),
                )}
              </div>
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
   * @param {{ key: string; label: string; artKey: string; track: Track | undefined }} row
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
    } else if (mode === "years" || mode === "added") {
      const view = /** @type {View & { type: "group-tracks" }} */ (this.#view.value);
      this.#view.value = { ...view, value: row.key };
      this.#renderedTracks = undefined;
      // The scroll-reset effect can't see the panel while the empty
      // state is showing, so reset the track list position here
      this.#scrollTop = 0;
      const panel = this.root().querySelector(".da-tracks-panel");
      panel?.scrollTo(0, 0);
    } else {
      this.setSelectedPlaylist(row.key);
    }
  }

  /**
   * @param {Function} html
   * @param {{ key: string; label: string; artKey: string; track: Track | undefined }} row
   * @param {number} top
   */
  #renderCatalogRow(html, row, top) {
    const artUrl = row.artKey
      ? this.#coverArtCache.get(row.artKey)
      : undefined;

    if (row.track) {
      this.#fetchAlbumArt(row.artKey, row.track);
    } else if (row.artKey) {
      this.#ensureRowArt(row.artKey, () =>
        this.$playlistFirstTracks().get(row.key));
    }

    const currentView = this.#view.value;
    const currentPlaylist = this.$scope.value?.playlist();
    const isActive = currentView.type === "album"
      ? currentView.albumKey === row.key
      : currentView.type === "artist"
      ? currentView.artistKey === row.key
      : currentView.type === "group-tracks"
      ? currentView.value === row.key
      : currentView.type === "playlist-tracks" && currentPlaylist === row.key;

    return html`
      <button
        class="da-playlist-row ${isActive ? `da-playlist-row--active` : ""}"
        style="top: ${top}px;"
        @click="${() => this.#activateCatalogRow(row)}"
        title="${row.label}"
      >
        ${row.artKey
          ? html`
            <div class="da-playlist-thumb">
              ${artUrl
                ? html`<img src="${artUrl}" alt="" loading="lazy" />`
                : html`
                  <div class="da-playlist-thumb__placeholder">
                    <i class="ph-fill ph-music-notes"></i>
                  </div>
                `}
            </div>`
          : nothing}
        <span>${row.label}</span>
      </button>
    `;
  }

  /**
   * @param {Function} html
   */
  #renderMain(html) {
    const view = this.#view.value;

    // Playlists / group views with nothing selected yet — prompt instead
    // of a misleading all-tracks list.
    if (
      (view.type === "playlist-tracks" && !this.$scope.value?.playlist()) ||
      (view.type === "group-tracks" && !view.value)
    ) {
      const what = view.type === "group-tracks"
        ? (view.groupBy === "createdAt" ? "month" : "year")
        : "playlist";
      return html`
        <main class="da-main">
          <div class="da-empty">
            <i class="ph-fill ${view.type === "group-tracks"
              ? (view.groupBy === "createdAt" ? `ph-clock` : `ph-calendar`)
              : `ph-playlist`}"></i>
            <p>Select a ${what}</p>
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
    } else if (view.type === "group-tracks") {
      title = view.value ?? (view.groupBy === "createdAt" ? "Added on" : "Years");
      badge = view.value ? (view.groupBy === "createdAt" ? "Month" : "Year") : "";
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
          <button class="da-pill" @click="${this.playNext}">
            <i class="ph-bold ph-caret-right"></i>
            <span>Play Next${this.#selectedTracks.value.size
              ? ` (${this.#selectedTracks.value.size})`
              : ``}</span>
          </button>
          <button class="da-pill" @click="${this.addViewToQueue}">
            <i class="ph-bold ph-list-plus"></i>
            <span>Add to Queue${this.#selectedTracks.value.size
              ? ` (${this.#selectedTracks.value.size})`
              : ``}</span>
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
    const stride = this.#rowStride;
    const totalSize = count * stride;

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
                top: (startIndex + i) * stride,
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
    const isSelected = this.#selectedTracks.value.has(track.id);

    return html`
      <div
        class="da-track-row ${isSelected ? `da-track-row--selected` : ""} ${isCurrent ? `da-track-row--current` : ""}"
        style="transform: translateY(${top}px);"
        @dblclick="${() => this.playTrack(track)}"
      >
        <div class="da-track__title">
          <button
            class="da-track__select ${isSelected ? `da-track__select--on` : ""}"
            @click="${(/** @type {Event} */ e) => {
              e.stopPropagation();
              this.toggleTrackSelection(track);
            }}"
            title="${isSelected ? `Deselect track` : `Select track`}
            aria-pressed="${isSelected ? `true` : `false`}
          >
            ${artUrl
              ? html`<img src="${artUrl}" alt="" loading="lazy" />`
              : html`
                <div class="da-track-art-placeholder">
                  <i class="ph-fill ph-music-notes"></i>
                </div>
              `}
            <i class="ph-bold ph-check da-track__select-check"></i>
          </button>
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
