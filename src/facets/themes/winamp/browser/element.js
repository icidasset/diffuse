import * as TID from "@atcute/tid";

import {
  defineElement,
  DiffuseElement,
  query,
  queryOptional,
  whenElementsDefined,
} from "~/common/element.js";
import { computed, signal, untracked } from "~/common/signal.js";
import * as Playlist from "~/common/playlist.js";
import { repeat } from "~/vendor/lit-html/directives/repeat.js";

/**
 * @import {RenderArg} from "~/common/element.d.ts"
 * @import {SignalReader} from "~/common/signal.d.ts";
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {OutputElement} from "@specs/components/output/types.d.ts"
 */

const ROW_HEIGHT = 14;
const OVERSCAN = 10;

/** @type {Record<string, string[]>} */
const COLUMN_SORT = {
  title: ["tags.title"],
  artist: ["tags.artist", "tags.album", "tags.disc.no", "tags.track.no"],
  album: ["tags.album", "tags.disc.no", "tags.track.no"],
};

const GROUP_BY_OPTIONS = [
  { value: "firstLetter", label: "First letter" },
  { value: "directory", label: "Path" },
  { value: "createdAt", label: "Processing date" },
  { value: "tags.year", label: "Track year" },
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

  $highlightedTrack = signal(/** @type {string | null} */ (null));
  $highlightedTracks = signal(/** @type {Set<string>} */ (new Set()));
  #anchorTrackId = /** @type {string | null} */ (null);

  $input = signal(
    /** @type {import("~/components/configurator/input/element.js").CLASS | undefined} */ (undefined),
  );

  #cachedUris = signal(/** @type {Set<string>} */ (new Set()));

  #selectedTracks = computed(() => {
    const tracks = this.$provider.value?.tracks() ?? [];
    const highlightedTracks = this.$highlightedTracks.value;
    if (highlightedTracks.size === 0) return [];
    return tracks.filter((t) => highlightedTracks.has(t.id));
  });

  #allCached = computed(() => {
    const selectedTracks = this.#selectedTracks();
    if (selectedTracks.length === 0) return false;
    const cachedUris = this.#cachedUris.value;
    return selectedTracks.every((t) => cachedUris.has(t.uri));
  });

  #playlistPickerState = signal(
    /** @type {{ mode: "add"; tracks: Track[] } | { mode: "create"; tracks: Track[] } | null} */ (null),
  );

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

  /** @type {VirtualItem[]} */
  #flatItems = [];

  /** @type {Track[] | undefined} */
  #lastTracks = undefined;

  /** @type {{ label: string; tracks: Track[] }[] | undefined} */
  #lastGroups = undefined;

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

    /** @type {import("~/components/orchestrator/favourites/element.js").CLASS} */
    const favourites = query(this, "favourites-orchestrator-selector");

    /** @type {import("~/components/configurator/input/element.js").CLASS | null} */
    const input = queryOptional(this, "input-selector");

    // Wait for the above dependencies to be defined, then render again.
    whenElementsDefined({ output, provider, queue, scope, favourites }).then(
      () => {
        this.$output.value = output;
        this.$provider.value = provider;
        this.$queue.value = queue;
        this.$scope.value = scope;
        this.$favourites.value = favourites;
      },
    );

    if (input) {
      whenElementsDefined({ input }).then(async () => {
        this.$input.value = input;
        const uris = await input.listCached();
        this.#cachedUris.value = new Set(uris);
      });
    }

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

    this.effect(() => {
      const groupBy = this.$scope.value?.groupBy();
      const select = this.root().querySelector("#groupby-select");
      if (select) {
        const displayValue = groupBy?.startsWith("firstLetter:")
          ? "firstLetter"
          : (groupBy ?? "");
        /** @type {HTMLSelectElement} */ (select).value = displayValue;
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
this.#renderIfWindowChanged();
        },
        { passive: true },
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

  #renderIfWindowChanged() {
    const { startIndex, endIndex } = this.#computeWindow();

    if (
      startIndex === this.#renderedStartIndex &&
      endIndex === this.#renderedEndIndex
    ) {
      return;
    }

    this.forceRender();
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
   * @param {Event} event
   */
  setGroupBy = (event) => {
    const scope = this.$scope.value;
    if (!scope) return;
    const raw = /** @type {HTMLSelectElement} */ (event.currentTarget).value;
    if (!raw) {
      scope.setGroupBy(undefined);
      return;
    }
    const value = raw === "firstLetter"
      ? `firstLetter:${scope.sortBy()[0] ?? "tags.title"}`
      : raw;
    scope.setGroupBy(value);
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
    const groups = this.$groupedPlaylists();

    return html`
      <div
        class="picker-overlay"
        @click="${(/** @type {MouseEvent} */ e) => {
          if (e.target === e.currentTarget) {
            this.#playlistPickerState.value = null;
          }
        }}"
      >
        <div class="window picker-window">
          <div class="title-bar">
            <div class="title-bar-text">${isCreate
              ? `New playlist`
              : `Add to playlist`}</div>
            <div class="title-bar-controls">
              <button aria-label="Close" @click="${() => {
                this.#playlistPickerState.value = null;
              }}"></button>
            </div>
          </div>
          <div class="window-body picker-body">
            ${isCreate
              ? html`
                <form
                  @submit="${async (/** @type {SubmitEvent} */ e) => {
                    e.preventDefault();
                    const form =
                      /** @type {HTMLFormElement} */ (e.currentTarget);
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
                      state.tracks,
                      ordered,
                    );
                    this.#playlistPickerState.value = null;
                  }}"
                >
                  <div class="field-row">
                    <label for="picker-playlist-name">Name:</label>
                    <input
                      id="picker-playlist-name"
                      name="playlist-name"
                      type="text"
                      autofocus
                      required
                    />
                  </div>
                  <div class="field-row">
                    <input
                      id="picker-playlist-ordered"
                      name="playlist-ordered"
                      type="checkbox"
                    />
                    <label for="picker-playlist-ordered">Ordered</label>
                  </div>
                  <div class="field-row picker-form-actions">
                    <button type="submit">Create</button>
                    <button type="button" @click="${() => {
                      this.#playlistPickerState.value = {
                        mode: `add`,
                        tracks: state.tracks,
                      };
                    }}">Back</button>
                  </div>
                </form>
              `
              : html`
                <div class="picker-list">
                  <button
                    class="picker-item picker-item--create"
                    @click="${() => {
                      this.#playlistPickerState.value = {
                        mode: `create`,
                        tracks: state.tracks,
                      };
                    }}"
                  >
                    + Create new playlist
                  </button>
                  ${groups.length > 0
                    ? html`
                      <hr />
                    `
                    : ``} ${groups.map(({ label, playlists }) =>
                      html`
                        <div class="picker-group-label">${label}</div>
                        ${playlists.map((p) =>
                          html`
                            <button
                              class="picker-item"
                              @click="${async () => {
                                await this.addTracksToPlaylist(
                                  p.name,
                                  state.tracks,
                                );
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
              `}
          </div>
        </div>
      </div>
    `;
  }

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    const highlighted = this.$highlightedTrack.value;
    const isLoading =
      this.$output.value?.tracks?.collection().state !== "loaded";

    const tracks = this.$provider.value?.tracks() ?? [];
    const groups = /** @type {{ label: string; tracks: Track[] }[] | undefined} */ (
      /** @type {any} */ (this.$provider.value)?.groups?.()
    );
    const playlist = this.$scope.value?.playlist();
    const groupBy = this.$scope.value?.groupBy();
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
    if (groups !== this.#lastGroups || tracks !== this.#lastTracks) {
      this.#flatItems = groups ? buildFlatList(groups) : [];
      this.#lastGroups = groups;
      this.#lastTracks = tracks;
    }

    const totalItems = groups ? this.#flatItems.length : tracks.length;
    const { startIndex, endIndex: rawEnd } = this.#computeWindow();
    const endIndex = Math.min(totalItems, rawEnd);

    this.#renderedStartIndex = startIndex;
    this.#renderedEndIndex = endIndex;

    const visibleItems = groups
      ? this.#flatItems.slice(startIndex, endIndex)
      : tracks.slice(startIndex, endIndex).map(
          (t) => /** @type {VirtualItem} */ ({ type: "track", track: t }),
        );
    const totalHeight = totalItems * ROW_HEIGHT;
    const topPad = startIndex * ROW_HEIGHT;

    const highlightedTracks = this.$highlightedTracks.value;
    const selectedTracks = this.#selectedTracks();
    const allCached = this.#allCached();

    return html`
      <link rel="stylesheet" href="vendor/98.css" />
      <link rel="stylesheet" href="facets/themes/winamp/98-scrollbar.css" />
      <link rel="stylesheet" href="facets/themes/winamp/98-vars.css" />
      <link rel="stylesheet" href="facets/themes/winamp/browser/element.css" />

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
        <label for="groupby-select">Group:</label>
        <select id="groupby-select" @change="${this.setGroupBy}">
          <option value="">None</option>
          ${GROUP_BY_OPTIONS.map((opt) =>
            html`
              <option value="${opt.value}">${opt.label}</option>
            `
          )}
        </select>
      </search>

      <div
        class="sunken-panel"
        tabindex="0"
        @keydown="${(/** @type {KeyboardEvent} */ e) => {
          if (e.key !== `ArrowUp` && e.key !== `ArrowDown`) return;
          e.preventDefault();
          const allTracks = this.$provider.value?.tracks() ?? [];
          if (allTracks.length === 0) return;
          const current = this.$highlightedTrack.value;
          const currentIdx = current
            ? allTracks.findIndex((t) => t.id === current)
            : -1;
          const nextIdx = e.key === `ArrowUp`
            ? Math.max(0, currentIdx - 1)
            : Math.min(allTracks.length - 1, currentIdx + 1);
          const next = allTracks[nextIdx];
          this.#anchorTrackId = next.id;
          this.$highlightedTracks.value = new Set([next.id]);
          this.$highlightedTrack.value = next.id;
          const panel = this.root().querySelector(`.sunken-panel`);
          if (panel) {
            const flatIdx = this.#flatItems.length > 0
              ? this.#flatItems.findIndex(
                  (item) => item.type === "track" && item.track.id === next.id,
                )
              : nextIdx;
            const rowTop = (flatIdx < 0 ? nextIdx : flatIdx) * ROW_HEIGHT;
            const rowBottom = rowTop + ROW_HEIGHT;
            if (rowTop < panel.scrollTop) {
              panel.scrollTop = rowTop;
            } else if (rowBottom > panel.scrollTop + panel.clientHeight) {
              panel.scrollTop = rowBottom - panel.clientHeight;
            }
          }
        }}"
      >
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
                : repeat(
                  visibleItems,
                  (item) =>
                    item.type === "group"
                      ? `group:${item.label}`
                      : `track:${item.track.id}`,
                  (item) => {
                  if (item.type === "group") {
                    return html`
                      <tr class="group-header">
                        <td colspan="3">${item.label}</td>
                      </tr>
                    `;
                  }
                  const track = item.track;
                  return html`
                    <tr
                      class="${highlightedTracks.has(track.id) ? `highlighted` : ``}"
                      @click="${(/** @type {MouseEvent} */ e) => {
                        const idx = tracks.findIndex(
                          (t) => t.id === track.id,
                        );
                        if (e.shiftKey && this.#anchorTrackId !== null) {
                          const anchorIdx = tracks.findIndex(
                            (t) => t.id === this.#anchorTrackId,
                          );
                          if (anchorIdx !== -1) {
                            const from = Math.min(anchorIdx, idx);
                            const to = Math.max(anchorIdx, idx);
                            this.$highlightedTracks.value = new Set(
                              tracks.slice(from, to + 1).map((t) => t.id),
                            );
                            this.$highlightedTrack.value = track.id;
                            return;
                          }
                        }
                        this.#anchorTrackId = track.id;
                        this.$highlightedTracks.value = new Set([track.id]);
                        this.$highlightedTrack.value = track.id;
                      }}"
                      @dblclick="${() => this.playTrack(track)}"
                    >
                      <td>${track.tags?.title}</td>
                      <td>${track.tags?.artist}</td>
                      <td>${track.tags?.album}</td>
                    </tr>
                  `;
                })}
            </tbody>
          </table>
        </div>
      </div>

      <div class="field-row actions-row">
        <button
          ?disabled="${selectedTracks.length === 0}"
          @click="${() => {
            if (selectedTracks.length === 0) return;
            this.$queue.value?.add({
              inFront: true,
              trackIds: selectedTracks.map((t) => t.id),
            });
          }}"
        >
          Play next
        </button>
        <button
          ?disabled="${selectedTracks.length === 0}"
          @click="${() => {
            if (selectedTracks.length === 0) return;
            this.$queue.value?.add({ trackIds: selectedTracks.map((t) => t.id) });
          }}"
        >
          Add to queue
        </button>
        <button
          ?disabled="${selectedTracks.length === 0}"
          @click="${() => {
            if (selectedTracks.length === 0) return;
            this.#playlistPickerState.value = {
              mode: `add`,
              tracks: selectedTracks,
            };
          }}"
        >
          Add to playlist
        </button>
        <button
          ?disabled="${selectedTracks.length !== 1}"
          @click="${() => {
            if (selectedTracks.length !== 1) return;
            this.toggleFavourite(selectedTracks[0]);
          }}"
        >
          ${selectedTracks.length === 1 && this.$favourites.value?.isFavourite(selectedTracks[0])
            ? `Unfavourite`
            : `Favourite`}
        </button>
        ${this.$input.value
          ? html`
            <button
              ?disabled="${selectedTracks.length === 0}"
              @click="${async () => {
                if (selectedTracks.length === 0) return;
                const uris = selectedTracks.map((t) => t.uri);
                if (allCached) {
                  await this.$input.value?.removeFromCache(uris);
                } else {
                  await this.$input.value?.cache(uris);
                }
                const updated = await this.$input.value?.listCached();
                if (updated) this.#cachedUris.value = new Set(updated);
              }}"
            >
              ${allCached ? `Remove from cache` : `Store in cache`}
            </button>
          `
          : ``}
      </div>

      ${this.#renderPlaylistPicker(html)}
    `;
  }
}

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

export default Browser;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = Browser;
export const NAME = "dtw-browser";

defineElement(NAME, CLASS);
