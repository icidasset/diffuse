import * as TID from "@atcute/tid";

import {
  defineElement,
  DiffuseElement,
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
 * @import {OutputElement} from "~/components/output/types.d.ts"
 * @import {ArtworkElement} from "~/components/artwork/types.d.ts"
 */

/**
 * @param {Track} track
 */
function trackTitle(track) {
  if (track.tags?.title) return track.tags.title;
  const path = track.uri.split("?")[0];
  const filename = path.split("/").filter(Boolean).at(-1);
  return filename ? decodeURIComponent(filename) : track.uri;
}

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

  $input = signal(
    /** @type {import("~/components/configurator/input/element.js").CLASS | undefined} */ (undefined),
  );

  #cachedUris = signal(/** @type {Set<string>} */ (new Set()));

  // SIGNALS - Pt. 2

  #viewMode = signal(
    /** @type {"list" | "cover"} */ (
      localStorage.getItem("diffuse/browser/view-mode") === "list"
        ? "list"
        : "cover"
    ),
  );

  #coverViewMode = signal(/** @type {"albums" | "artists"} */ ("albums"));

  #openCoverItem = signal(/** @type {OpenCoverItem | null} */ (null));

  #playlistPickerState = signal(
    /** @type {{ mode: "filter" } | { mode: "add"; tracks: Track[] } | { mode: "create"; tracks: Track[] } | null} */ (null),
  );

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

  $favouritesSet = computed(() => {
    const favItems = this.$favourites.value?.playlistItems() ?? [];
    return new Set(
      favItems.map((item) => {
        const a = item.criteria.find((c) => c.field === "tags.artist");
        const t = item.criteria.find((c) => c.field === "tags.title");
        return `${String(a?.value ?? "").toLowerCase()}|${
          String(t?.value ?? "").toLowerCase()
        }`;
      }),
    );
  });

  $sortedArtistGroups = computed(() => {
    const groups = this.$coverGroups.value?.artistGroups() ?? [];
    return this.$scope.value?.sortDirection() === "desc"
      ? groups.map((g) => ({ ...g, groups: [...g.groups].reverse() }))
      : groups;
  });

  $sortedCoverGroups = computed(() => {
    const groups = this.$coverGroups.value?.coverGroups() ?? [];
    return this.$scope.value?.sortDirection() === "desc"
      ? groups.map((g) => ({ ...g, groups: [...g.groups].reverse() }))
      : groups;
  });

  $detailTracks = computed(() => {
    const item = this.#openCoverItem.value;
    if (!item) return /** @type {Track[]} */ ([]);
    if (item.type === "album") {
      return this.$tracksByAlbum().get(item.albumKey) ?? [];
    }
    return this.$tracksByArtist().get(item.artistKey) ?? [];
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

  /** @type {Map<string, Track>} */
  #pendingVisibleCards = new Map();

  /** @type {ReturnType<typeof setTimeout> | undefined} */
  #artFetchDebounce = undefined;

  /** @type {VirtualItem[]} */
  #flatItems = [];

  /** @type {Track[] | undefined} */
  #lastTracks = undefined;

  /** @type {{ label: string; tracks: Track[] }[] | undefined} */
  #lastGroups = undefined;

  // Custom virtual scroll state
  #scrollTop = 0;
  #viewportHeight = 0;
  #renderedStartIndex = -1;
  #renderedEndIndex = -1;
  #itemCount = 0;

  /** @type {ResizeObserver | undefined} */
  #resizeObserver;

  /** @type {AbortController | undefined} */
  #scrollAbort;

  /** @type {IntersectionObserver | undefined} */
  #scrollVisibilityObserver;

  /** @type {number | undefined} */
  #scrollRenderRaf = undefined;

  /** @type {number[]} */
  #offsets = [];

  /** @type {Track[] | undefined} */
  #previousTracks = undefined;

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

    /** @type {import("~/components/configurator/input/element.js").CLASS | null} */
    const input = queryOptional(this, "input-selector");

    whenElementsDefined({ output, provider, queue, scope, favourites }).then(
      () => {
        batch(() => {
          this.$output.value = output;
          this.$provider.value = provider;
          this.$queue.value = queue;
          this.$scope.value = scope;
          this.$favourites.value = favourites;
        });
      },
    );

    if (input) {
      whenElementsDefined({ input }).then(async () => {
        this.$input.value = input;
        const uris = await input.listCached();
        this.#cachedUris.value = new Set(uris);
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

    // Reset scroll when track list changes (skip the initial population)
    this.effect(() => {
      const tracks = this.$provider.value?.tracks();

      untracked(() => {
        if (this.#previousTracks === undefined) {
          this.#previousTracks = tracks;
          return;
        }
        this.#previousTracks = tracks;
        const panel = this.root().querySelector(".scroll-panel");
        if (panel) {
          panel.scrollTo(0, 0);
          this.#scrollTop = 0;
        }
      });
    });

    // Re-observe cover cards when groups change (track filtering, sorting, search)
    this.effect(() => {
      const _coverGroups = this.$sortedCoverGroups();
      const _artistGroups = this.$sortedArtistGroups();

      if (this.#viewMode.value !== "cover") return;
      if (this.#openCoverItem.value) return;

      untracked(() => {
        requestAnimationFrame(() => {
          this.#setupCoverObserver();

          // Direct visibility check: IntersectionObserver is async and may not
          // deliver its initial callback before paint, so fetch visible cards now.
          const panel = this.root().querySelector(".cover-scroll-panel");
          if (!panel) return;
          const panelRect = panel.getBoundingClientRect();
          const margin = 200;
          for (const card of this.root().querySelectorAll(
            ".cover-card[data-album-key]",
          )) {
            const albumKey =
              /** @type {HTMLElement} */ (card).dataset.albumKey;
            if (
              !albumKey || this.#coverArtCache.has(albumKey) ||
              this.#pendingArtFetch.has(albumKey)
            ) continue;
            const cardRect = card.getBoundingClientRect();
            if (
              cardRect.bottom + margin < panelRect.top ||
              cardRect.top - margin > panelRect.bottom
            ) continue;
            const track = this.$albumTrackMap().get(albumKey);
            if (track) this.#fetchAlbumArt(albumKey, track);
          }
        });
      });
    });

    // Set up scroll tracking after first render, when .scroll-panel exists in the DOM.
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
    this.#disconnectCoverObserver();
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

  /**
   * @param {string} playlistName
   * @param {Track[]} tracks
   */
  addTracksToPlaylist = async (playlistName, tracks) => {
    const output = this.$output.value;
    if (!output || !tracks.length) return;

    const col = output.playlistItems.collection();
    const existing = col.state === "loaded" ? col.data : [];

    const existingKeys = new Set(
      existing
        .filter((item) => item.playlist === playlistName)
        .map((item) => {
          const a = item.criteria.find((c) =>
            c.field === "tags.artist"
          )?.value ?? "";
          const t =
            item.criteria.find((c) => c.field === "tags.title")?.value ?? "";
          return `${String(a).toLowerCase()}.${String(t).toLowerCase()}`;
        }),
    );

    const transformations = /** @type {string[]} */ (["toLowerCase"]);
    const now = new Date().toISOString();

    const newItems = tracks
      .filter((track) => {
        const key = `${String(track.tags?.artist ?? "").toLowerCase()}.${
          String(track.tags?.title ?? "").toLowerCase()
        }`;
        return !existingKeys.has(key);
      })
      .map((
        track,
      ) => /** @type {import("~/definitions/types.d.ts").PlaylistItem} */ ({
        $type: "sh.diffuse.output.playlistItem",
        id: TID.now(),
        playlist: playlistName,
        criteria: [
          {
            field: "tags.artist",
            value: /** @type {unknown} */ (track.tags?.artist),
            transformations,
          },
          {
            field: "tags.title",
            value: /** @type {unknown} */ (track.tags?.title),
            transformations,
          },
        ],
        createdAt: now,
        updatedAt: now,
      }));

    if (!newItems.length) return;
    await output.playlistItems.save([...existing, ...newItems]);
  };

  /**
   * @param {string} playlistName
   * @param {Track[]} tracks
   * @param {boolean} ordered
   */
  createPlaylistWithTracks = async (playlistName, tracks, ordered) => {
    const output = this.$output.value;
    if (!output || !tracks.length) return;

    const col = output.playlistItems.collection();
    const existing = col.state === "loaded" ? col.data : [];

    const transformations = /** @type {string[]} */ (["toLowerCase"]);
    const now = new Date().toISOString();

    /** @type {import("~/definitions/types.d.ts").PlaylistItem[]} */
    const newItems = [];
    let prevId = /** @type {string | undefined} */ (undefined);

    for (const track of tracks) {
      const id = TID.now();
      newItems.push(
        /** @type {import("~/definitions/types.d.ts").PlaylistItem} */ ({
          $type: "sh.diffuse.output.playlistItem",
          id,
          playlist: playlistName,
          criteria: [
            {
              field: "tags.artist",
              value: /** @type {unknown} */ (track.tags?.artist),
              transformations,
            },
            {
              field: "tags.title",
              value: /** @type {unknown} */ (track.tags?.title),
              transformations,
            },
          ],
          ...(ordered ? { positionedAfter: prevId } : {}),
          createdAt: now,
          updatedAt: now,
        }),
      );
      if (ordered) prevId = id;
    }

    await output.playlistItems.save([...existing, ...newItems]);
  };

  /**
   * @param {Function} html
   * @param {string} menuId
   * @param {Track[]} tracks
   */
  #renderTrackMenu(html, menuId, tracks) {
    const uris = tracks.map((t) => t.uri);
    const allCached = uris.length > 0 &&
      uris.every((u) => this.#cachedUris.value.has(u));
    return html`
      <div id="${menuId}" class="dropdown" popover @click="${(
        /** @type {MouseEvent} */ e,
      ) => e.stopPropagation()}">
        <button @click="${() => {
          if (!tracks.length) return;
          this.$queue.value?.add({
            inFront: true,
            trackIds: tracks.map((t) => t.id),
          });
          /** @type {HTMLElement | null} */ (this.root().querySelector(
            `#${menuId}`,
          ))?.hidePopover();
        }}">
          <i class="ph-bold ph-clock-counter-clockwise"></i>
          Play next
        </button>
        <button @click="${() => {
          if (!tracks.length) return;
          this.$queue.value?.add({ trackIds: tracks.map((t) => t.id) });
          /** @type {HTMLElement | null} */ (this.root().querySelector(
            `#${menuId}`,
          ))?.hidePopover();
        }}">
          <i class="ph-bold ph-clock-counter-clockwise"></i>
          Add to queue
        </button>
        <button @click="${() => {
          if (!tracks.length) return;
          /** @type {HTMLElement | null} */ (this.root().querySelector(
            `#${menuId}`,
          ))?.hidePopover();
          this.#playlistPickerState.value = { mode: "add", tracks };
        }}">
          <i class="ph-bold ph-playlist"></i>
          Add to playlist
        </button>
        <button @click="${async () => {
          if (!uris.length) return;
          const isCached = uris.every((u) => this.#cachedUris.value.has(u));
          if (isCached) {
            await this.$input.value?.removeFromCache(uris);
          } else {
            await this.$input.value?.cache(uris);
          }
          const updated = await this.$input.value?.listCached();
          if (updated) this.#cachedUris.value = new Set(updated);
          /** @type {HTMLElement | null} */ (this.root().querySelector(
            `#${menuId}`,
          ))?.hidePopover();
        }}">
          <i class="ph-fill ph-lightning"></i>
          ${allCached ? `Remove from cache` : `Store in cache`}
        </button>
      </div>
    `;
  }

  toggleViewMode = () => {
    if (this.#viewMode.value === "cover") {
      this.#disconnectCoverObserver();
      this.#openCoverItem.value = null;
    }
    const next = this.#viewMode.value === "list" ? "cover" : "list";
    localStorage.setItem("diffuse/browser/view-mode", next);
    this.#viewMode.value = next;
    if (next === "list") this.#setupScrollTracking();
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
        this.#coverArtCache.set(albumKey, url);
      } else {
        this.#coverArtCache.set(albumKey, null);
      }
    } catch {
      // don't cache on error — let it be retried
    } finally {
      this.#pendingArtFetch.delete(albumKey);
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
          let hasNew = false;
          for (const entry of entries) {
            if (!entry.isIntersecting) continue;
            hasNew = true;
            const albumKey =
              /** @type {HTMLElement} */ (entry.target).dataset.albumKey;
            if (!albumKey) continue;
            const track = this.$albumTrackMap().get(albumKey);
            if (track) this.#pendingVisibleCards.set(albumKey, track);
            this.#coverObserver?.unobserve(entry.target);
          }
          if (!hasNew) return;
          clearTimeout(this.#artFetchDebounce);
          this.#artFetchDebounce = setTimeout(() => {
            for (const [albumKey, track] of this.#pendingVisibleCards) {
              this.#fetchAlbumArt(albumKey, track);
            }
            this.#pendingVisibleCards.clear();
          }, 150);
        },
        { root, rootMargin: "200px" },
      );
    }

    for (
      const card of this.root().querySelectorAll(".cover-card[data-album-key]")
    ) {
      const albumKey = /** @type {HTMLElement} */ (card).dataset.albumKey;
      if (
        albumKey &&
        (this.#coverArtCache.has(albumKey) ||
          this.#pendingArtFetch.has(albumKey))
      ) continue;
      this.#coverObserver.observe(card);
    }
  }

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
      const panel = this.root().querySelector(".scroll-panel");
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
        // Cap to window.innerHeight to guard against measuring the scroll height
        // before shadow DOM stylesheets finish loading (when the panel has no
        // flex constraints and expands to its full virtual content size).
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

    if (this.#offsets.length === 0) {
      const startIndex = Math.max(
        0,
        Math.floor(scrollTop / TRACK_ROW_HEIGHT) - OVERSCAN,
      );
      const visibleCount = Math.ceil(viewportHeight / TRACK_ROW_HEIGHT) +
        2 * OVERSCAN;
      return {
        startIndex,
        endIndex: Math.min(count, startIndex + visibleCount),
      };
    }

    const offsets = this.#offsets;
    const startIndex = Math.max(0, bisectRight(offsets, scrollTop) - OVERSCAN);
    const endIndex = Math.min(
      count,
      bisectRight(offsets, scrollTop + viewportHeight) + 1 + OVERSCAN,
    );
    return { startIndex, endIndex };
  }

  #buildOffsets() {
    const items = this.#flatItems;
    if (items.length === 0) {
      this.#offsets = [];
      return;
    }
    const offsets = new Array(items.length + 1);
    offsets[0] = 0;
    for (let i = 0; i < items.length; i++) {
      offsets[i + 1] = offsets[i] +
        (items[i].type === "group" ? GROUP_HEADER_HEIGHT : TRACK_ROW_HEIGHT);
    }
    this.#offsets = offsets;
  }

  #disconnectCoverObserver() {
    this.#coverObserver?.disconnect();
    this.#coverObserver = undefined;
    clearTimeout(this.#artFetchDebounce);
    this.#pendingVisibleCards.clear();
  }

  // RENDER

  /**
   * @param {Function} html
   */
  #renderPlaylistPicker(html) {
    const state = this.#playlistPickerState.value;
    if (!state) {
      return html`

      `;
    }

    const isCreate = state.mode === "create";
    const title = state.mode === "filter"
      ? "Filter by playlist"
      : isCreate
      ? "New playlist"
      : "Add to playlist";

    const groups = this.$groupedPlaylists();

    const headerBackBtn = isCreate
      ? html`
        <button
          class="toolbar-icon-btn"
          @click="${() => {
            this.#playlistPickerState.value = {
              mode: "add",
              tracks: /** @type {any} */ (state).tracks,
            };
          }}"
          title="Back"
        >
          <i class="ph-bold ph-arrow-left"></i>
        </button>
      `
      : ``;

    const closeBtn = html`
      <a
        class="toolbar-icon-btn toolbar-icon-btn--link"
        href="l/?path=facets%2Fdata%2Fplaylists%2Findex.html"
        target="_blank"
        title="Open playlist manager"
      >
        <i class="ph-bold ph-gear"></i>
      </a>
      <button
        class="toolbar-icon-btn"
        @click="${() => {
          this.#playlistPickerState.value = null;
        }}"
        title="Close"
      >
        <i class="ph-bold ph-x"></i>
      </button>
    `;

    const body = isCreate
      ? html`
        <form
          class="playlist-picker-create-form"
          @submit="${async (/** @type {SubmitEvent} */ e) => {
            e.preventDefault();
            const form = /** @type {HTMLFormElement} */ (e.currentTarget);
            const name =
              /** @type {HTMLInputElement} */ (form.elements.namedItem(
                `playlist-name`,
              ))?.value.trim();
            const ordered =
              /** @type {HTMLInputElement} */ (form.elements.namedItem(
                `playlist-ordered`,
              ))?.checked ?? false;
            if (!name) return;
            await this.createPlaylistWithTracks(
              name,
              /** @type {any} */ (state).tracks,
              ordered,
            );
            this.#playlistPickerState.value = null;
          }}"
        >
          <label class="playlist-picker-create-label">
            Name
            <input
              class="playlist-picker-create-input"
              name="playlist-name"
              type="text"
              placeholder="My playlist"
              autocomplete="off"
              autofocus
              required
            />
          </label>
          <label
            class="playlist-picker-create-label playlist-picker-create-label--checkbox"
          >
            <input
              class="playlist-picker-create-checkbox"
              name="playlist-ordered"
              type="checkbox"
            />
            Ordered
          </label>
          <button class="playlist-picker-create-submit" type="submit">
            Create playlist
          </button>
        </form>
      `
      : html`
        <div class="playlist-picker-list">
          ${state.mode === "filter"
            ? html`
              <button
                class="playlist-picker-item ${!this.$scope.value?.playlist()
                  ? `playlist-picker-item--active`
                  : ``}"
                @click="${() => {
                  this.setSelectedPlaylist(undefined);
                  this.#playlistPickerState.value = null;
                }}"
              >
                All tracks
              </button>
            `
            : ``} ${state.mode === "add"
            ? html`
              <button
                class="playlist-picker-item playlist-picker-item--create"
                @click="${() => {
                  this.#playlistPickerState.value = {
                    mode: "create",
                    tracks: state.tracks,
                  };
                }}"
              >
                <i class="ph-bold ph-plus"></i>
                Create new playlist
              </button>
              ${groups.length > 0
                ? html`
                  <div class="playlist-picker-divider"></div>
                `
                : ``}
            `
            : ``} ${groups.length === 0
            ? (state.mode === "add" ? `` : html`
              <div class="playlist-picker-empty">No playlists yet</div>
            `)
            : groups.map(({ label, playlists }) =>
              html`
                <div class="playlist-picker-group-label">${label}</div>
                ${playlists.map((p) =>
                  html`
                    <button
                      class="playlist-picker-item ${state.mode === `filter` &&
                          this.$scope.value?.playlist() === p.name
                        ? `playlist-picker-item--active`
                        : ``}"
                      @click="${async () => {
                        if (state.mode === "add") {
                          await this.addTracksToPlaylist(p.name, state.tracks);
                        } else {
                          this.setSelectedPlaylist(p.name);
                        }
                        this.#playlistPickerState.value = null;
                      }}"
                    >
                      ${p.name}
                    </button>
                  `
                )}
              `
            )}
        </div>
      `;

    return html`
      <div
        class="playlist-picker-overlay"
        @click="${(/** @type {MouseEvent} */ e) => {
          if (e.target === e.currentTarget) {
            this.#playlistPickerState.value = null;
          }
        }}"
      >
        <div class="playlist-picker-panel">
          <div class="playlist-picker-header">
            ${headerBackBtn}
            <span class="playlist-picker-title">${title}</span>
            ${closeBtn}
          </div>
          ${body}
        </div>
      </div>
    `;
  }

  /**
   * @param {Function} html
   * @param {boolean} isLoading
   */
  #renderCoverView(html, isLoading) {
    if (this.#openCoverItem.value) return this.#renderCoverDetail(html);

    const coverViewMode = this.#coverViewMode.value;
    const sortDirection = this.$scope.value?.sortDirection() ?? "asc";

    const groups = coverViewMode === "artists"
      ? this.$sortedArtistGroups()
      : this.$sortedCoverGroups();
    const totalCount = groups.reduce((n, g) => n + g.groups.length, 0);

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
              this.#disconnectCoverObserver();
              scope.setSortDirection(
                scope.sortDirection() === `asc` ? `desc` : `asc`,
              );
            }}"
            title="${sortDirection === `desc`
              ? `Sort ascending`
              : `Sort descending`}"
          >
            <i class="ph-bold ph-arrow-${sortDirection === `desc`
              ? `up`
              : `down`}"></i>
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

    const albumTracksMap = this.$tracksByAlbum();
    const artistTracksMap = this.$tracksByArtist();

    if (coverViewMode === "artists") {
      requestAnimationFrame(() => this.#setupCoverObserver());

      return html`
        ${tabs}
        <div class="scroll-panel cover-scroll-panel">
          ${this.$sortedArtistGroups().map(({ label, groups }, groupIndex) =>
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
                  const cardTracks = artistTracksMap.get(artistKey) ?? [];
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
                            <img src="${artUrl}" alt="${artistName}" loading="lazy" @error="${() => {
                              this.#coverArtCache.set(artistKey, null);
                              this.#scheduleArtRender();
                            }}" />
                          `
                          : html`
                            <div class="cover-art-placeholder"><i class="ph-fill ph-vinyl-record"></i></div>
                          `}
                        <button
                          class="cover-menu-btn"
                          popovertarget="artist-card-menu-${track.id}"
                          @click="${(/** @type {Event} */ e) =>
                            e.stopPropagation()}"
                        >
                          <i class="ph-fill ph-dots-three-outline"></i>
                        </button>
                        ${this.#renderTrackMenu(
                          html,
                          `artist-card-menu-${track.id}`,
                          cardTracks,
                        )}
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
    requestAnimationFrame(() => this.#setupCoverObserver());

    return html`
      ${tabs}
      <div class="scroll-panel cover-scroll-panel">
        ${this.$sortedCoverGroups().map(({ label, groups }, groupIndex) =>
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
                const cardTracks = albumTracksMap.get(albumKey) ?? [];
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
                          <img src="${artUrl}" alt="${albumName}" loading="lazy" @error="${() => {
                            this.#coverArtCache.set(albumKey, null);
                            this.#scheduleArtRender();
                          }}" />
                        `
                        : html`
                          <div class="cover-art-placeholder"><i class="ph-fill ph-vinyl-record"></i></div>
                        `}
                      <button
                        class="cover-menu-btn"
                        popovertarget="album-card-menu-${track.id}"
                        @click="${(/** @type {Event} */ e) =>
                          e.stopPropagation()}"
                      >
                        <i class="ph-fill ph-dots-three-outline"></i>
                      </button>
                      ${this.#renderTrackMenu(
                        html,
                        `album-card-menu-${track.id}`,
                        cardTracks,
                      )}
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

    // Rebuild flat items and pixel offsets only when data reference changes
    if (groups !== this.#lastGroups || tracks !== this.#lastTracks) {
      this.#flatItems = groups ? buildFlatList(groups) : [];
      this.#buildOffsets();
      this.#lastGroups = groups;
      this.#lastTracks = tracks;
    }

    const count = groups ? this.#flatItems.length : tracks.length;
    this.#itemCount = count;

    const { startIndex, endIndex } = this.#computeWindow(count);
    this.#renderedStartIndex = startIndex;
    this.#renderedEndIndex = endIndex;

    const totalSize = this.#offsets.length > 0
      ? this.#offsets[this.#flatItems.length]
      : count * TRACK_ROW_HEIGHT;

    const favSet = this.$favouritesSet();

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
            <span class="track-title">${trackTitle(track)}</span>
          </div>
          <div class="col-artist">
            <span>${track.tags?.artist}</span>
          </div>
          <div class="col-album">
            <span>${track.tags?.album}</span>
          </div>
          <div class="col-menu">
            <button
              class="track-menu-btn"
              popovertarget="list-track-menu-${track.id}"
              @click="${(/** @type {Event} */ e) => e.stopPropagation()}"
            >
              <i class="ph-bold ph-dots-three-outline"></i>
            </button>
            ${this.#renderTrackMenu(html, `list-track-menu-${track.id}`, [
              track,
            ])}
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
                ? `ph-caret-up`
                : `ph-caret-down`}"></i>
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
                ? `ph-caret-up`
                : `ph-caret-down`}"></i>
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
                ? `ph-caret-up`
                : `ph-caret-down`}"></i>
            `
            : ``}
        </div>
        <div class="col-menu"></div>
      </div>

      <div class="scroll-panel">
        <div class="virtual-scroll" style="height: ${totalSize}px;">
          ${isLoading
            ? html`
              <div class="loading">Loading ...</div>
            `
            : repeat(
              buildWindowItems(
                groups,
                tracks,
                this.#flatItems,
                this.#offsets,
                startIndex,
                endIndex,
              ),
              (entry) => entry.key,
              (entry) => {
                if (entry.type === "group") {
                  return html`
                    <div
                      class="group-header ${entry.index === 0
                        ? `group-header--top`
                        : ``}"
                      style="transform: translateY(${entry.top}px);"
                    >
                      <i class="ph-fill ph-vinyl-record"></i>
                      <span>${entry.label}</span>
                    </div>
                  `;
                }
                return renderTrackRow(entry.track, entry.top, entry.index);
              },
            )}
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

    const detailTracks = this.$detailTracks();
    const favSet = this.$favouritesSet();

    let key = "",
      name = "",
      subtitle = "";

    if (item.type === "album") {
      key = item.albumKey;
      name = item.albumName;
      subtitle = item.artist;
    } else {
      key = item.artistKey;
      name = item.artistName;
      subtitle = `${item.trackCount} ${
        item.trackCount === 1 ? "track" : "tracks"
      }`;
    }

    const artUrl = this.#coverArtCache.get(key);

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
          ${this.#renderTrackMenu(html, `album-actions-menu`, detailTracks)}
        </div>
        <div class="album-detail-main">
          <div class="album-detail-sidebar">
            <div class="album-detail-art">
              ${artUrl
                ? html`
                  <img src="${artUrl}" alt="${name}" @error="${() => {
                    this.#coverArtCache.set(key, null);
                    this.#scheduleArtRender();
                  }}" />
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
                    <span class="track-title">${trackTitle(t)}</span>
                  </div>
                  <div class="col-artist">
                    <span>${t.tags?.artist}</span>
                  </div>
                  <div class="col-menu">
                    <button
                      class="track-menu-btn"
                      popovertarget="detail-track-menu-${t.id}"
                      @click="${(/** @type {Event} */ e) =>
                        e.stopPropagation()}"
                    >
                      <i class="ph-bold ph-dots-three-outline"></i>
                    </button>
                    ${this.#renderTrackMenu(html, `detail-track-menu-${t.id}`, [
                      t,
                    ])}
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
        @click="${() => {
          this.#playlistPickerState.value = { mode: "filter" };
        }}"
      >
        <i class="ph-fill ph-playlist"></i>
        <span>${playlist ?? `All tracks`}</span>
      </button>
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
      <link rel="stylesheet" href="facets/themes/blur/variables.css" />
      <link rel="stylesheet" href="facets/themes/blur/browser/element.css" />

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
        : this.#renderListView(html, isLoading, sortBy)} ${this
        .#renderPlaylistPicker(html)}
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
 * Largest index i where sortedArr[i] <= target (sorted ascending).
 * Returns 0 when target is below all values.
 * @param {number[]} sortedArr
 * @param {number} target
 * @returns {number}
 */
function bisectRight(sortedArr, target) {
  let lo = 0, hi = sortedArr.length;
  while (lo < hi) {
    const mid = (lo + hi) >>> 1;
    if (sortedArr[mid] <= target) lo = mid + 1;
    else hi = mid;
  }
  return Math.max(0, lo - 1);
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

/**
 * @typedef {{ type: "group"; key: string; label: string; index: number; top: number }} WindowGroupEntry
 * @typedef {{ type: "track"; key: string; track: Track; index: number; top: number }} WindowTrackEntry
 * @typedef {WindowGroupEntry | WindowTrackEntry} WindowEntry
 */

/**
 * @param {{ label: string; tracks: Track[] }[] | undefined} groups
 * @param {Track[]} tracks
 * @param {VirtualItem[]} flatItems
 * @param {number[]} offsets
 * @param {number} startIndex
 * @param {number} endIndex
 * @returns {WindowEntry[]}
 */
function buildWindowItems(
  groups,
  tracks,
  flatItems,
  offsets,
  startIndex,
  endIndex,
) {
  /** @type {WindowEntry[]} */
  const entries = [];
  for (let i = startIndex; i < endIndex; i++) {
    const item = groups
      ? flatItems[i]
      : /** @type {VirtualItem} */ ({ type: "track", track: tracks[i] });
    const top = offsets.length > 0 ? offsets[i] : i * TRACK_ROW_HEIGHT;
    if (!item) continue;
    if (item.type === "group") {
      entries.push({
        type: "group",
        key: `group:${item.label}`,
        label: item.label,
        index: i,
        top,
      });
    } else {
      entries.push({
        type: "track",
        key: `track:${item.track.id}`,
        track: item.track,
        index: i,
        top,
      });
    }
  }
  return entries;
}

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = Browser;
export const NAME = "db-browser";

defineElement(NAME, CLASS);
