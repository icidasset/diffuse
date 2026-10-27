import foundation, { GROUP } from "~/common/foundation.js";
import { effect } from "~/common/signal.js";
import { CommandCore } from "~/vendor/kmenu-core/index.js";
import * as Output from "~/common/output.js";
import * as Playlist from "~/common/playlist.js";
import {
  criteriaFor,
  filterOptions,
  sameFields,
  signatureOf,
  trackLabel,
} from "~/facets/misc/command/common.js";

/**
 * @import {CommandOption} from "kmenu"
 * @import {Facet, PlaylistItem, Track} from "~/definitions/types.d.ts"
 */

foundation.setup({ title: "Command Menu" });

// ---------------------------------------------------------------------------
// Foundation setup
// ---------------------------------------------------------------------------

const [queue, repeatShuffle, output, scope, favourites, audio] = await Promise
  .all([
    foundation.engine.queue(),
    foundation.engine.repeatShuffle(),
    foundation.orchestrator.output(),
    foundation.engine.scope(),
    foundation.orchestrator.favourites(),
    foundation.engine.audio(),
  ]);

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const SEEK_STEP = 10;
const VOLUME_STEP = 0.1;

const FACET_HISTORY_KEY = "diffuse/l/facet-history";
const FALLBACK_BROWSER_FACET = "facets/themes/blur/browser/facet/index.tile";

const SORT_OPTIONS = [
  {
    key: "title",
    label: "Title",
    icon: "ph-text-t",
    fields: ["tags.title"],
  },
  {
    key: "artist",
    label: "Artist",
    icon: "ph-user",
    fields: [
      "tags.artist",
      "tags.album",
      "tags.disc.no",
      "tags.track.no",
    ],
  },
  {
    key: "album",
    label: "Album",
    icon: "ph-vinyl-record",
    fields: ["tags.album", "tags.disc.no", "tags.track.no"],
  },
  {
    key: "year",
    label: "Year",
    icon: "ph-calendar",
    fields: ["tags.year"],
  },
  {
    key: "created",
    label: "Date added",
    icon: "ph-clock",
    fields: ["createdAt"],
  },
];

const GROUP_OPTIONS = [
  { key: "none", label: "None", icon: "ph-x", value: undefined },
  { key: "artist", label: "Artist", icon: "ph-user", value: "tags.artist" },
  {
    key: "album",
    label: "Album",
    icon: "ph-vinyl-record",
    value: "tags.album",
  },
  {
    key: "first-letter",
    label: "First letter",
    icon: "ph-text-aa",
    value: "firstLetter",
  },
  { key: "year", label: "Year", icon: "ph-calendar", value: "tags.year" },
  {
    key: "date",
    label: "Date added",
    icon: "ph-clock",
    value: "createdAt",
  },
  {
    key: "folder",
    label: "Folder",
    icon: "ph-folder",
    value: "directory",
  },
];

// ---------------------------------------------------------------------------
// DOM elements
// ---------------------------------------------------------------------------

const searchInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#search"));
const optionsList =
  /** @type {HTMLUListElement} */ (document.querySelector("#kmenu-listbox"));
const breadcrumbsEl =
  /** @type {HTMLElement} */ (document.querySelector("#breadcrumbs"));
const statusEl = /** @type {HTMLElement} */ (document.querySelector("#status"));

// ---------------------------------------------------------------------------
// Command menu instance
// ---------------------------------------------------------------------------

/** @type {CommandCore} */
const menu = new CommandCore({
  // The core hands us every option (including submenu descendants) at the root
  // level when there is a query. Keep only roots there, but let query terms
  // match a root's descendants so nested commands stay discoverable.
  /**
   * @param {CommandOption[]} options
   * @param {string} query
   * @returns {CommandOption[]}
   */
  filter: (options, query) =>
    filterOptions(options, query, menu.getState().currentLevel === 0),
});

// Register DOM elements with CommandCore (for scroll management and focus)
menu.getListboxProps().ref(optionsList);
menu.getInputProps().ref(searchInput);

// Wire up input events
searchInput.oninput = (/** @type {Event} */ e) =>
  menu.setInput(/** @type {HTMLInputElement} */ (e.target).value);
searchInput.onkeydown = (/** @type {KeyboardEvent} */ e) =>
  menu.getInputProps().onKeyDown(e);

// Escape must navigate back instead of closing the menu, and the menu itself
// always stays open. Handle both before the core's own window-level handlers
// (which are registered on the bubble phase) get a chance to run.
globalThis.addEventListener(
  "keydown",
  (/** @type {KeyboardEvent} */ e) => {
    if (e.key === "Escape") {
      e.preventDefault();
      e.stopImmediatePropagation();

      if (menu.getState().input) {
        menu.setInput("");
        searchInput.value = "";
        return;
      }

      if (menu.getState().breadcrumbs.length > 0) menu.goBack();
      return;
    }

    if ((e.metaKey || e.ctrlKey) && e.key.toLowerCase() === "k") {
      // Keep the palette visible and focused instead of letting the core
      // toggle it closed (which would immediately reopen and lose state).
      e.preventDefault();
      e.stopImmediatePropagation();
      searchInput.focus();
    }
  },
  true,
);

// ---------------------------------------------------------------------------
// Option helpers
// ---------------------------------------------------------------------------

/**
 * Read a non-empty collection, or return null while it is still loading.
 *
 * @template T
 * @param {{ state: "loading" } | { state: "loaded"; data: T } | { state: "error" }} collection
 * @returns {T | null}
 */
function loaded(collection) {
  return collection.state === "loaded" ? collection.data : null;
}

/**
 * @param {string} name
 * @param {Track | null} track
 * @returns {PlaylistItem}
 */
function makePlaylistItem(name, track) {
  const now = new Date().toISOString();

  return {
    $type: "sh.diffuse.output.playlistItem",
    id: crypto.randomUUID(),
    playlist: name,
    criteria: track ? criteriaFor(track) : [],
    createdAt: now,
    updatedAt: now,
  };
}

/** @returns {Track | null} */
function currentTrack() {
  const item = queue.now();
  if (!item) return null;

  const collection = output.tracks.collection();
  if (collection.state !== "loaded") return null;

  return collection.data.find((track) => track.id === item.id) ?? null;
}

/** @param {string[]} trackIds */
function playTracks(trackIds) {
  if (trackIds.length === 0) return;

  // `add` already promotes the first item when the queue is idle, so only
  // advance manually when something is currently playing.
  const wasIdle = !queue.now();
  queue.clear({});
  queue.add({ trackIds });
  if (!wasIdle) queue.shift();
}

/** @param {number} seconds */
function seekBy(seconds) {
  const id = queue.now()?.id;
  if (!id) return;

  const state = audio.state(id);
  if (!state) return;

  const target = state.currentTime() + seconds;
  const duration = state.duration();
  const currentTime = Math.max(
    0,
    Number.isFinite(duration) ? Math.min(target, duration) : target,
  );

  audio.seek({ audioId: id, currentTime });
}

/** @param {number} delta */
function changeVolume(delta) {
  const next = Math.max(0, Math.min(1, audio.volume() + delta));
  audio.adjustVolume({ volume: Math.round(next * 100) / 100 });
}

/** @param {string} id */
function openFacet(id) {
  const url = new URL("l/", document.baseURI);
  url.searchParams.set("id", id);
  if (GROUP !== "facets") url.searchParams.set("group", GROUP);
  globalThis.open(url.toString(), "_blank", "noopener");
}

/** @param {string} path */
function openPage(path) {
  globalThis.open(
    new URL(path, document.baseURI).toString(),
    "_blank",
    "noopener",
  );
}

/**
 * Whether a facet is an interface that can display the track collection.
 *
 * @param {Facet} facet
 * @returns {boolean}
 */
function isBrowsingFacet(facet) {
  const uri = facet.uri ?? "";
  return uri.includes("/themes/") || uri.includes("/browser/") ||
    facet.name.toLowerCase().includes("browser");
}

/**
 * The facet the user was using most recently before this one, restricted to
 * interfaces that can show tracks.
 *
 * @returns {Facet | null}
 */
function lastBrowsingFacet() {
  const collection = output.facets.collection();
  const facets = collection.state === "loaded" ? collection.data : [];
  const byId = new Map(facets.map((facet) => [facet.id, facet]));
  const currentId = new URL(location.href).searchParams.get("id");

  /** @type {string[]} */
  let history = [];

  try {
    const stored = JSON.parse(localStorage.getItem(FACET_HISTORY_KEY) ?? "[]");
    if (Array.isArray(stored)) {
      history = stored.filter((id) => typeof id === "string");
    }
  } catch {
    // Malformed/unavailable storage — fall through to the saved themes.
  }

  for (const id of history) {
    if (id === currentId) continue;

    const facet = byId.get(id);
    if (
      facet &&
      facet.enabled !== false &&
      facet.kind !== "prelude" &&
      isBrowsingFacet(facet)
    ) {
      return facet;
    }
  }

  return facets.find((facet) =>
    facet.id !== currentId &&
    facet.enabled !== false &&
    facet.kind !== "prelude" &&
    isBrowsingFacet(facet)
  ) ?? null;
}

/**
 * Show the collection by opening the last-used browsing facet. When the menu is
 * embedded (split view), sibling panes already follow the shared scope, so
 * replacing this pane would only close the palette.
 */
function revealCollection() {
  if (globalThis.self !== globalThis.top) return;

  const target = lastBrowsingFacet();
  const url = new URL("l/", document.baseURI);

  if (target) url.searchParams.set("id", target.id);
  else url.searchParams.set("path", FALLBACK_BROWSER_FACET);

  if (GROUP !== "facets") url.searchParams.set("group", GROUP);
  globalThis.location.assign(url.toString());
}

/**
 * Apply a collection search and reveal the result.
 *
 * @param {string | undefined} term
 */
function showSearch(term) {
  if (!term) return;

  scope.setPlaylist(undefined);
  scope.setSearchTerm(term);
  scope.setSelection(undefined);
  revealCollection();
}

// ---------------------------------------------------------------------------
// Build the command tree
// ---------------------------------------------------------------------------

/** @returns {CommandOption[]} */
function buildCommands() {
  const state = menu.getState();
  const query = state.input.trim();
  const atRoot = state.currentLevel === 0;

  // State
  const nowItem = queue.now();
  const nowId = nowItem?.id ?? null;
  const future = queue.future();
  const past = queue.past();
  const isRepeat = repeatShuffle.repeat();
  const isShuffle = repeatShuffle.shuffle();

  const isPlaying = audio.isPlaying();
  const nowState = nowId ? audio.state(nowId) : undefined;
  const hasEnded = nowState?.hasEnded() ?? false;
  const volume = audio.volume();
  const volumePercent = Math.round(volume * 100);

  const tracks = loaded(output.tracks.collection()) ?? [];
  const now = nowId ? tracks.find((track) => track.id === nowId) ?? null : null;
  const nowLabel = trackLabel(now);

  const items = loaded(output.playlistItems.collection()) ?? [];
  const playlists = [...Playlist.gather(items).values()].sort((a, b) =>
    a.name.localeCompare(b.name)
  );

  const facets = (loaded(output.facets.collection()) ?? [])
    .filter((facet) => facet.kind !== "prelude" && facet.enabled !== false)
    .sort((a, b) => a.name.localeCompare(b.name));

  const currentPlaylist = scope.playlist();
  const searchTerm = scope.searchTerm();
  const sortBy = scope.sortBy();
  const sortDirection = scope.sortDirection();
  const groupBy = scope.groupBy();

  const isFavourite = now ? favourites.isFavourite(now) : false;

  const activeGroupLabel = groupBy
    ? groupBy.startsWith("firstLetter:")
      ? "First letter"
      : GROUP_OPTIONS.find((option) => option.value === groupBy)?.label ??
        groupBy
    : "None";

  // -------------------------------------------------------------------------
  // Playback
  // -------------------------------------------------------------------------

  /** @type {CommandOption[]} */
  const commands = [
    {
      id: "toggle-playback",
      label: isPlaying && !hasEnded ? "Pause playback" : "Play",
      keywords: ["play", "pause", "resume", "start", "stop"],
      group: "Playback",
      disabled: !nowId,
      data: { icon: isPlaying && !hasEnded ? "ph-pause" : "ph-play" },
      action: () => {
        const id = queue.now()?.id;
        if (!id) return;

        const state = audio.state(id);

        if (state?.isPlaying() && !state.hasEnded()) {
          audio.pause({ audioId: id });
          return;
        }

        // Replaying a finished track must rewind first; `play()` alone leaves
        // it at the end.
        if (state?.hasEnded()) audio.seek({ audioId: id, currentTime: 0 });
        audio.play({ audioId: id });
      },
    },
    {
      id: "next-track",
      label: "Next track",
      keywords: ["next", "skip", "forward", "queue"],
      group: "Playback",
      disabled: future.length === 0,
      data: { icon: "ph-skip-forward" },
      action: () => queue.shift(),
    },
    {
      id: "previous-track",
      label: "Previous track",
      keywords: ["previous", "back", "skip", "queue"],
      group: "Playback",
      disabled: past.length === 0,
      data: { icon: "ph-skip-back" },
      action: () => queue.unshift(),
    },
    {
      id: "seek-back",
      label: `Back ${SEEK_STEP} seconds`,
      keywords: ["seek", "rewind", "back", "skip"],
      group: "Playback",
      disabled: !nowId,
      data: { icon: "ph-rewind" },
      action: () => seekBy(-SEEK_STEP),
    },
    {
      id: "seek-forward",
      label: `Forward ${SEEK_STEP} seconds`,
      keywords: ["seek", "forward", "skip"],
      group: "Playback",
      disabled: !nowId,
      data: { icon: "ph-fast-forward" },
      action: () => seekBy(SEEK_STEP),
    },
    {
      id: "volume-up",
      label: `Volume up (${volumePercent}%)`,
      keywords: ["volume", "louder", "sound", "audio"],
      group: "Playback",
      disabled: volume >= 1,
      data: { icon: "ph-speaker-high" },
      action: () => changeVolume(VOLUME_STEP),
    },
    {
      id: "volume-down",
      label: `Volume down (${volumePercent}%)`,
      keywords: ["volume", "quieter", "sound", "audio"],
      group: "Playback",
      disabled: volume <= 0,
      data: { icon: "ph-speaker-low" },
      action: () => changeVolume(-VOLUME_STEP),
    },
    {
      id: "toggle-mute",
      label: volume === 0 ? "Unmute (75%)" : "Mute",
      keywords: ["mute", "silence", "volume", "sound"],
      group: "Playback",
      data: { icon: volume === 0 ? "ph-speaker-slash" : "ph-speaker-simple-x" },
      action: () => {
        audio.adjustVolume({ volume: audio.volume() === 0 ? 0.75 : 0 });
      },
    },
    {
      id: "toggle-repeat",
      label: "Repeat",
      keywords: ["repeat", "loop", "enable", "disable"],
      group: "Playback",
      data: { icon: "ph-repeat", checked: isRepeat },
      action: () => {
        repeatShuffle.setRepeat(!repeatShuffle.repeat());
      },
    },
    {
      id: "toggle-shuffle",
      label: "Shuffle",
      keywords: ["shuffle", "random", "enable", "disable"],
      group: "Playback",
      data: { icon: "ph-shuffle", checked: isShuffle },
      action: () => {
        repeatShuffle.setShuffle(!repeatShuffle.shuffle());
      },
    },
    {
      id: "clear-queue",
      label: "Clear upcoming tracks",
      keywords: ["clear", "queue", "empty", "remove", "upcoming"],
      group: "Playback",
      disabled: future.length === 0,
      data: { icon: "ph-trash" },
      children: [
        {
          id: "clear-queue-confirm",
          label: `Confirm: clear ${
            future.length === 1 ? "1 track" : `${future.length} tracks`
          }`,
          keywords: ["yes", "confirm", "clear", "queue"],
          action: () => queue.clear({}),
        },
      ],
    },
  ];

  // -------------------------------------------------------------------------
  // Now playing
  // -------------------------------------------------------------------------

  commands.push(
    {
      id: "favourite-toggle",
      label: now
        ? isFavourite
          ? `Remove "${nowLabel}" from favourites`
          : `Add "${nowLabel}" to favourites`
        : "Add now playing to favourites",
      keywords: [
        "favourite",
        "favorite",
        "like",
        "heart",
        "star",
        "now playing",
      ],
      group: "Now playing",
      disabled: !now,
      data: { icon: isFavourite ? "ph-heart-break" : "ph-heart" },
      action: async () => {
        const track = currentTrack();
        if (track) await favourites.toggle(track);
      },
    },
    {
      id: "play-favourites",
      label: "Play favourites",
      keywords: ["play", "favourites", "favorites", "liked"],
      group: "Now playing",
      disabled: tracks.length === 0 || favourites.playlistItems().length === 0,
      data: { icon: "ph-heart" },
      action: () => {
        const collection = output.tracks.collection();
        if (collection.state !== "loaded") return;

        const favouriteTracks = Playlist.filterByPlaylist(
          collection.data,
          favourites.playlistItems(),
        );

        playTracks(favouriteTracks.map((track) => track.id));
      },
    },
  );

  // -------------------------------------------------------------------------
  // Playlists
  // -------------------------------------------------------------------------

  commands.push({
    id: "select-playlist",
    label: currentPlaylist ? `Playlist: ${currentPlaylist}` : "Select playlist",
    keywords: ["playlist", "filter", "browse", "queue", "select"],
    group: "Playlists",
    data: { icon: "ph-music-notes" },
    children: [
      {
        id: "playlist-select-all",
        label: "All tracks",
        keywords: ["all", "everything", "reset", "no playlist"],
        data: { checked: !currentPlaylist, icon: "ph-music-notes" },
        action: () => {
          scope.setPlaylist(undefined);
          revealCollection();
        },
      },
      ...playlists.map((playlist) => ({
        id: `playlist-select-${encodeURIComponent(playlist.name)}`,
        label: playlist.name,
        keywords: [playlist.name, "playlist"],
        data: {
          checked: currentPlaylist === playlist.name,
          icon: "ph-music-notes",
        },
        action: () => {
          scope.setPlaylist(playlist.name);
          revealCollection();
        },
      })),
    ],
  });

  commands.push({
    id: "add-to-playlist",
    label: now
      ? `Add "${nowLabel}" to playlist`
      : "Add current track to playlist",
    keywords: ["add", "playlist", "current", "track", "now playing"],
    group: "Playlists",
    disabled: !now || playlists.length === 0,
    data: { icon: "ph-playlist" },
    children: playlists.map((playlist) => {
      const alreadyAdded = now
        ? playlist.items.some((item) => Playlist.match(now, item))
        : false;

      return {
        id: `playlist-add-${encodeURIComponent(playlist.name)}`,
        label: alreadyAdded
          ? `${playlist.name} (already added)`
          : playlist.name,
        keywords: [playlist.name],
        disabled: alreadyAdded,
        data: {
          checked: alreadyAdded,
          icon: alreadyAdded ? "ph-check" : "ph-plus",
        },
        action: async () => {
          const track = currentTrack();
          if (!track) return;

          const existing = await Output.data(output.playlistItems);
          await output.playlistItems.save([
            ...existing,
            makePlaylistItem(playlist.name, track),
          ]);
        },
      };
    }),
  });

  commands.push({
    id: "create-playlist",
    label: now ? `Create playlist with "${nowLabel}"` : "Create playlist",
    keywords: ["new", "add", "create", "playlist"],
    group: "Playlists",
    data: { icon: "ph-playlist" },
    action: async () => {
      const track = currentTrack();
      const suggestion = menu.getState().input.trim();
      const name = prompt("New playlist name:", suggestion)?.trim();
      if (!name) return;

      const existing = await Output.data(output.playlistItems);
      const match = existing.find((item) =>
        item.playlist.toLowerCase() === name.toLowerCase()
      );

      if (match) {
        scope.setPlaylist(match.playlist);
        revealCollection();
        return;
      }

      await output.playlistItems.save([
        ...existing,
        makePlaylistItem(name, track),
      ]);

      scope.setPlaylist(name);
      revealCollection();
    },
  });

  commands.push({
    id: "rename-playlist",
    label: "Rename playlist",
    keywords: ["rename", "playlist", "name", "edit"],
    group: "Playlists",
    disabled: playlists.length === 0,
    data: { icon: "ph-pencil-simple" },
    children: playlists.map((playlist) => ({
      id: `playlist-rename-${encodeURIComponent(playlist.name)}`,
      label: playlist.name,
      keywords: [playlist.name],
      action: async () => {
        const name = prompt("Rename playlist:", playlist.name)?.trim();
        if (!name || name === playlist.name) return;

        const existing = await Output.data(output.playlistItems);

        if (
          existing.some((item) =>
            item.playlist !== playlist.name &&
            item.playlist.toLowerCase() === name.toLowerCase()
          )
        ) {
          alert(`A playlist named "${name}" already exists.`);
          return;
        }

        const updatedAt = new Date().toISOString();
        await output.playlistItems.save(
          existing.map((item) =>
            item.playlist === playlist.name
              ? { ...item, playlist: name, updatedAt }
              : item
          ),
        );

        if (scope.playlist() === playlist.name) scope.setPlaylist(name);
      },
    })),
  });

  commands.push({
    id: "play-playlist",
    label: "Play playlist",
    keywords: ["play", "playlist", "listen", "queue"],
    group: "Playlists",
    disabled: playlists.length === 0 || tracks.length === 0,
    data: { icon: "ph-play-circle" },
    children: playlists.map((playlist) => ({
      id: `playlist-play-${encodeURIComponent(playlist.name)}`,
      label: playlist.name,
      keywords: [playlist.name, "play"],
      action: async () => {
        const collection = output.tracks.collection();
        if (collection.state !== "loaded") return;

        const existing = await Output.data(output.playlistItems);
        const bundle = Playlist.gather(existing).get(playlist.name);
        if (!bundle) return;

        let playlistTracks = Playlist.filterByPlaylist(
          collection.data,
          bundle.items,
        );

        if (!bundle.unordered) {
          playlistTracks = Playlist.orderByPlaylist(
            playlistTracks,
            bundle.items,
          );
        }

        playTracks(playlistTracks.map((track) => track.id));
      },
    })),
  });

  commands.push({
    id: "remove-playlist",
    label: "Remove playlist",
    keywords: ["delete", "remove", "playlist"],
    group: "Playlists",
    disabled: playlists.length === 0,
    data: { icon: "ph-trash" },
    children: playlists.map((playlist) => ({
      id: `playlist-remove-${encodeURIComponent(playlist.name)}`,
      label: playlist.name,
      keywords: [playlist.name],
      children: [
        {
          id: `playlist-remove-${encodeURIComponent(playlist.name)}-confirm`,
          label: `Confirm: remove "${playlist.name}"`,
          keywords: ["yes", "confirm", "delete", "remove"],
          action: async () => {
            const existing = await Output.data(output.playlistItems);
            await output.playlistItems.save(
              existing.filter((item) => item.playlist !== playlist.name),
            );

            if (scope.playlist() === playlist.name) {
              scope.setPlaylist(undefined);
            }
          },
        },
      ],
    })),
  });

  // -------------------------------------------------------------------------
  // Collection
  // -------------------------------------------------------------------------

  commands.push({
    id: "sort-by",
    label: "Sort by",
    keywords: ["sort", "order", "arrange", "collection"],
    group: "Collection",
    data: { icon: "ph-sort-ascending" },
    children: SORT_OPTIONS.map((option) => ({
      id: `sort-by-${option.key}`,
      label: option.label,
      keywords: [option.label, "sort", "order"],
      data: {
        icon: option.icon,
        checked: sameFields(sortBy, option.fields),
      },
      action: () => {
        if (sameFields(scope.sortBy(), option.fields)) {
          scope.setSortDirection(
            scope.sortDirection() === "desc" ? "asc" : "desc",
          );
        } else {
          scope.setSortBy(option.fields);
          scope.setSortDirection("asc");
        }
      },
    })),
  });

  commands.push({
    id: "toggle-sort-direction",
    label: sortDirection === "desc" ? "Sort ascending" : "Sort descending",
    keywords: [
      "sort",
      "direction",
      "ascending",
      "descending",
      "reverse",
      "order",
    ],
    group: "Collection",
    data: {
      icon: sortDirection === "desc"
        ? "ph-sort-ascending"
        : "ph-sort-descending",
    },
    action: () => {
      scope.setSortDirection(
        scope.sortDirection() === "desc" ? "asc" : "desc",
      );
    },
  });

  commands.push({
    id: "group-by",
    label: `Group by: ${activeGroupLabel}`,
    keywords: ["group", "grouping", "collection", "arrange"],
    group: "Collection",
    data: { icon: "ph-squares-four" },
    children: GROUP_OPTIONS.map((option) => {
      const value = option.key === "first-letter"
        ? `firstLetter:${sortBy[0] ?? "tags.title"}`
        : option.value;

      const checked = option.key === "first-letter"
        ? !!groupBy?.startsWith("firstLetter:")
        : groupBy === value;

      return {
        id: `group-by-${option.key}`,
        label: option.label,
        keywords: [option.label, "group"],
        data: { icon: option.icon, checked },
        action: () => {
          scope.setGroupBy(value);
        },
      };
    }),
  });

  commands.push({
    id: "clear-search",
    label: searchTerm ? `Clear search "${searchTerm}"` : "Clear search",
    keywords: ["clear", "search", "filter", "reset"],
    group: "Collection",
    disabled: !searchTerm,
    data: { icon: "ph-magnifying-glass" },
    action: () => {
      scope.setSearchTerm(undefined);
    },
  });

  commands.push({
    id: "reset-collection",
    label: "Reset collection filters",
    keywords: ["reset", "clear", "filters", "sort", "group", "playlist"],
    group: "Collection",
    disabled: !currentPlaylist && !searchTerm && !groupBy,
    data: { icon: "ph-arrow-counter-clockwise" },
    action: () => {
      scope.setPlaylist(undefined);
      scope.setSearchTerm(undefined);
      scope.setSelection(undefined);
      scope.setGroupBy(undefined);
      scope.revertToDefaultSort();
    },
  });

  // -------------------------------------------------------------------------
  // Navigate
  // -------------------------------------------------------------------------

  commands.push({
    id: "open-facet",
    label: "Open facet",
    keywords: ["open", "facet", "window", "tab", "launch"],
    group: "Navigate",
    disabled: facets.length === 0,
    data: { icon: "ph-squares-four" },
    children: facets.map((facet) => ({
      id: `facet-open-${encodeURIComponent(facet.id)}`,
      label: facet.name,
      keywords: [facet.name, "facet", "open", ...(facet.tags ?? [])],
      action: () => openFacet(facet.id),
    })),
  });

  commands.push({
    id: "open-playlists",
    label: "Manage playlists",
    keywords: ["playlists", "manage", "open", "view"],
    group: "Navigate",
    data: { icon: "ph-playlist" },
    action: () => {
      const facet = facets.find((facet) => facet.name === "Playlists");
      if (facet) {
        openFacet(facet.id);
        return;
      }

      const url = new URL("l/", document.baseURI);
      url.searchParams.set("path", "facets/data/playlists/index.tile");
      if (GROUP !== "facets") url.searchParams.set("group", GROUP);
      globalThis.open(url.toString(), "_blank", "noopener");
    },
  });

  commands.push({
    id: "open-dashboard",
    label: "Open dashboard",
    keywords: ["dashboard", "open", "settings", "facets", "manage"],
    group: "Navigate",
    data: { icon: "ph-gauge" },
    action: () => openPage("dashboard/"),
  });

  // -------------------------------------------------------------------------
  // Search — a query that matches nothing else still becomes a filter action
  // -------------------------------------------------------------------------

  if (atRoot && query) {
    commands.push({
      id: "search-collection",
      label: `Search collection for “${query}”`,
      keywords: [query, "search", "find", "filter", "collection"],
      group: "Search",
      data: { icon: "ph-magnifying-glass" },
      action: () => {
        showSearch(query);
      },
    });

    // Offer to turn an arbitrary typed name into a playlist. Skip it when a
    // playlist with that name already exists (playlist names are matched
    // case-insensitively here, matching the action's behaviour).
    if (
      !playlists.some((playlist) =>
        playlist.name.toLowerCase() === query.toLowerCase()
      )
    ) {
      commands.push({
        id: "create-playlist-query",
        label: now
          ? `Create playlist “${query}” with current track`
          : `Create playlist “${query}”`,
        keywords: [query, "create", "playlist", "new"],
        group: "Create",
        data: { icon: "ph-playlist" },
        action: async () => {
          const existing = await Output.data(output.playlistItems);
          const match = existing.find((item) =>
            item.playlist.toLowerCase() === query.toLowerCase()
          );

          if (match) {
            scope.setPlaylist(match.playlist);
            revealCollection();
            return;
          }

          await output.playlistItems.save([
            ...existing,
            makePlaylistItem(query, currentTrack()),
          ]);

          scope.setPlaylist(query);
          revealCollection();
        },
      });
    }
  }

  return commands;
}

// ---------------------------------------------------------------------------
// Render
// ---------------------------------------------------------------------------

function render() {
  const state = menu.getState();

  // Keep the input in sync with the core. During typing the values already
  // match, so this only fires for programmatic changes (menu close, Escape).
  // It deliberately doesn't depend on focus: close() resets the query while the
  // input is still the active element, and leaving the old text behind would
  // make the next keystroke append to a stale query.
  if (searchInput.value !== state.input) {
    searchInput.value = state.input;
  }

  searchInput.placeholder = state.breadcrumbs.length > 0
    ? "Filter…"
    : "Type a command…";

  // Breadcrumbs
  breadcrumbsEl.innerHTML = "";

  if (state.breadcrumbs.length > 0) {
    const home = document.createElement("button");
    home.type = "button";
    home.className = "crumb-home";
    home.textContent = "Home";
    home.onclick = () => {
      while (menu.getState().breadcrumbs.length > 0) menu.goBack();
    };
    breadcrumbsEl.appendChild(home);

    for (const crumb of state.breadcrumbs) {
      const separator = document.createElement("i");
      separator.className = "ph-bold ph-caret-right crumb-separator";
      separator.setAttribute("aria-hidden", "true");
      breadcrumbsEl.appendChild(separator);

      const span = document.createElement("span");
      span.className = "crumb";
      const option = state.allOptions.find((option) => option.id === crumb.id);
      span.textContent = option?.label ?? crumb.label;
      breadcrumbsEl.appendChild(span);
    }
  }

  // Options list
  optionsList.innerHTML = "";

  if (state.filtered.length === 0) {
    const message = state.input ? "No commands found" : "No commands available";

    const li = document.createElement("li");
    li.className = "empty-message";
    li.textContent = message;
    optionsList.appendChild(li);

    searchInput.removeAttribute("aria-activedescendant");
    statusEl.textContent = message;
    return;
  }

  let currentGroup = null;

  for (const option of state.filtered) {
    // Group header
    if (option.group && option.group !== currentGroup) {
      currentGroup = option.group;
      const groupLi = document.createElement("li");
      groupLi.className = "group-label";
      groupLi.setAttribute("role", "presentation");
      groupLi.textContent = option.group;
      optionsList.appendChild(groupLi);
    }

    const li = document.createElement("li");
    li.className = "option";

    const props = menu.getOptionProps(/** @type {string} */ (option.id));
    props.ref(li); // Register with CommandCore for scroll management

    li.setAttribute("id", `kmenu-option-${option.id}`);
    li.setAttribute("role", "option");
    li.setAttribute(
      "aria-selected",
      state.activeId === option.id ? "true" : "false",
    );
    if (option.disabled) li.setAttribute("aria-disabled", "true");

    const icon = option.data?.icon;
    if (icon) {
      const i = document.createElement("i");
      i.className = `ph-bold ${icon} option-icon`;
      i.setAttribute("aria-hidden", "true");
      li.appendChild(i);
    }

    const label = document.createElement("span");
    label.className = "option-label";
    label.textContent = option.label;
    li.appendChild(label);

    if (option.data?.checked) {
      const check = document.createElement("i");
      check.className = "ph-bold ph-check option-check";
      check.setAttribute("aria-hidden", "true");
      li.appendChild(check);
    }

    const hasChildren = option.children && option.children.length > 0;
    if (hasChildren) {
      const chevron = document.createElement("i");
      chevron.className = "ph-bold ph-caret-right option-chevron";
      chevron.setAttribute("aria-hidden", "true");
      li.appendChild(chevron);
    }

    li.onclick = props.onClick;
    li.onmouseenter = props.onMouseEnter;

    optionsList.appendChild(li);
  }

  if (state.activeId) {
    searchInput.setAttribute(
      "aria-activedescendant",
      `kmenu-option-${state.activeId}`,
    );
  } else {
    searchInput.removeAttribute("aria-activedescendant");
  }

  const active = state.filtered.find((option) => option.id === state.activeId);
  statusEl.textContent = active
    ? `${active.label}. ${state.activeIndex + 1} of ${state.filtered.length}.`
    : "";
}

// ---------------------------------------------------------------------------
// Registration
// ---------------------------------------------------------------------------

let registeredSignature = "";

/**
 * Register the latest command tree. Skipped while a submenu is open so
 * background signal updates don't eject the user back to the root.
 *
 * @param {CommandOption[]} [options]
 */
function commit(options = buildCommands()) {
  if (menu.getState().currentLevel > 0) return;

  const signature = signatureOf(options);
  if (signature === registeredSignature) return;

  registeredSignature = signature;
  menu.registerOptions(options);
  render();

  // Re-registering rebuilds the DOM, so restore the highlight (and scroll it
  // into view) when the active option survived the update.
  const activeId = menu.getState().activeId;
  if (activeId) menu.setActiveById(activeId);
}

// Navigation: update only the active highlight without rebuilding the DOM
menu.on("navigate", (/** @type {any} */ e) => {
  for (const li of optionsList.querySelectorAll("[role=option]")) {
    li.setAttribute(
      "aria-selected",
      li.id === `kmenu-option-${e.activeId}` ? "true" : "false",
    );
  }

  if (e.activeId) {
    searchInput.setAttribute(
      "aria-activedescendant",
      `kmenu-option-${e.activeId}`,
    );
  } else {
    searchInput.removeAttribute("aria-activedescendant");
  }

  const state = menu.getState();
  const active = state.filtered.find((option) => option.id === e.activeId);
  statusEl.textContent = active
    ? `${active.label}. ${state.activeIndex + 1} of ${state.filtered.length}.`
    : "";
});

// Full re-render for structural changes; `commit` keeps the tree in sync with
// the current query (used for the "search collection" fallback).
for (const event of ["open", "change", "submenu", "back"]) {
  menu.on(/** @type {any} */ (event), () => {
    commit();
    render();
  });
}

// Re-open the menu immediately on close to keep it always visible
menu.on("close", () => {
  menu.open();
  commit();
  render();
});

// ---------------------------------------------------------------------------
// Keep commands in sync with state
// ---------------------------------------------------------------------------

effect(() => {
  // Reading every signal here registers them as dependencies; buildCommands
  // also takes care of derived values (labels, checked state, …).
  const commands = buildCommands();
  commit(commands);
});

// ---------------------------------------------------------------------------
// Open and signal ready
// ---------------------------------------------------------------------------

commit();
render();
menu.open();

foundation.ready();
