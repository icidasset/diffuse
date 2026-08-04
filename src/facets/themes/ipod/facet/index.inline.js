import { html, render as litRender } from "lit-html";

import foundation from "~/common/foundation.js";
import { batch, computed, effect, signal } from "~/common/signal.js";
import * as Playlist from "~/common/playlist.js";

/**
 * @import { Track } from "~/definitions/types.d.ts"
 */

// Set doc title
foundation.setup({ title: "iPod | Diffuse" });

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const [queue, repeatShuffle] = await Promise.all([
  foundation.engine.queue(),
  foundation.engine.repeatShuffle(),
]);

await Promise.all([
  customElements.whenDefined(queue.localName),
  customElements.whenDefined(repeatShuffle.localName),
]);

const scopedTracks = await foundation.orchestrator.scopedTracks();
const output = await foundation.orchestrator.output();
const pathCollections = await foundation.orchestrator.pathCollections();
await foundation.orchestrator.sources();
await foundation.orchestrator.processTracks({ disableWhenReady: true });
await foundation.orchestrator.queueAudio();
const controller = await foundation.orchestrator.controller();
await foundation.orchestrator.artwork();
await foundation.orchestrator.mediaSession();
await foundation.orchestrator.favourites();
await foundation.configurator.input();

/** @type {import("~/components/orchestrator/controller/element.js").default} */
const c = controller;

////////////////////////////////////////////
// ELEMENTS
////////////////////////////////////////////

const el = {
  title: /** @type {HTMLElement} */ (document.querySelector("#screen-title")),
  status: /** @type {HTMLElement} */ (document.querySelector("#screen-status")),
  viewport: /** @type {HTMLElement} */ (document.querySelector("#viewport")),
  viewNow: /** @type {HTMLElement} */ (document.querySelector("#view-now")),
  viewList:
    /** @type {HTMLOListElement} */ (document.querySelector("#view-list")),
  nowTitle: /** @type {HTMLElement} */ (document.querySelector("#now-title")),
  nowArtist: /** @type {HTMLElement} */ (document.querySelector("#now-artist")),
  progress: /** @type {HTMLElement} */ (document.querySelector("#progress")),
  progressFill:
    /** @type {HTMLElement} */ (document.querySelector("#progress-fill")),
  progressKnob:
    /** @type {HTMLElement} */ (document.querySelector("#progress-knob")),
  timeElapsed:
    /** @type {HTMLElement} */ (document.querySelector("#time-elapsed")),
  timeRemaining:
    /** @type {HTMLElement} */ (document.querySelector("#time-remaining")),
  wheel: /** @type {HTMLElement} */ (document.querySelector("#wheel")),
  select: /** @type {HTMLButtonElement} */ (document.querySelector("#select")),
  scrollbar: /** @type {HTMLElement} */ (document.querySelector("#scrollbar")),
};

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

const ROW_HEIGHT = 22;

/** "now-playing" or "list" (a menu stack). */
const screen = signal(/** @type {"now-playing" | "list"} */ ("list"));

/**
 * The menu stack. Each frame is a browsable list.
 * @typedef {Object} MenuFrame
 * @property {string} title
 * @property {MenuItem[]} items
 * @property {number} selected
 * @property {number} scroll
 * @property {() => MenuItem[]} [$items]  // computed signal for live frames
 */

/**
 * @typedef {Object} MenuItem
 * @property {string} label
 * @property {string} [value]
 * @property {"action" | "folder" | "track" | "toggle"} kind
 * @property {() => void} [run]        // for "action"
 * @property {boolean} [goNow]      // for "action" — go to Now Playing after run
 * @property {string} [icon]          // for "action" — Phosphor icon class (e.g. "ph-play")
 * @property {boolean} [separator]    // for "action" — render a divider line below
 * @property {() => MenuFrame} [open]   // for "folder"
 * @property {string} [trackId]         // for "track"
 * @property {number} [queueIndex]     // for "track" in the Queue
 * @property {() => boolean} [getOn]    // for "toggle"
 * @property {() => void} [toggle]      // for "toggle"
 */

/** @type {import("~/common/signal.d.ts").Signal<MenuFrame[]>} */
const stack = signal(/** @type {MenuFrame[]} */ ([]));

/** Whether the now-playing screen is in scrubber (seek) mode. */
const scrubbing = signal(false);

/** Current vertical scroll offset (px, <= 0) of the list view. */
const listScroll = signal(0);

let autoFilled = false;

////////////////////////////////////////////
// HELPERS
////////////////////////////////////////////

/**
 * @param {Track | undefined} track
 * @param {string} fallbackId
 */
const trackTitle = (track, fallbackId) => track?.tags?.title ?? fallbackId;

/**
 * @param {Track | undefined} track
 */
const trackArtist = (track) =>
  track?.tags?.artist ?? track?.tags?.albumartist ?? null;

/** @param {number} seconds */
const formatTime = (seconds) => {
  if (!Number.isFinite(seconds) || seconds < 0) seconds = 0;
  const m = Math.floor(seconds / 60);
  const s = Math.floor(seconds % 60);
  return `${m}:${String(s).padStart(2, "0")}`;
};

/** The flat queue list: [past..., now, future...] as display items. */
const queueItems = () => {
  const now = queue.now();
  const past = queue.past();
  const future = queue.future();
  const items = [];
  for (const p of past) items.push({ ...p, state: "past" });
  if (now) items.push({ ...now, state: "now" });
  for (const f of future) items.push({ ...f, state: "future" });
  return items;
};

/** Tracks collection (resolved) */
const tracks = () => {
  const col = output.tracks.collection();
  return col.state === "loaded" ? col.data : [];
};

/** @param {string} id */
const findTrack = (id) => tracks().find((t) => t.id === id);

/** @param {Track} t */
const artistOf = (t) =>
  t.tags?.artist ?? t.tags?.albumartist ?? "Unknown Artist";
/** @param {Track} t */
const albumOf = (t) => t.tags?.album ?? "Unknown Album";

/**
 * @param {Track[]} list
 * @returns {Map<string, Track[]>} keyed by lowercased artist
 */
const groupByArtist = (list) => {
  /** @type {Map<string, Track[]>} */
  const map = new Map();
  for (const t of list) {
    const k = artistOf(t).toLowerCase();
    if (!map.has(k)) map.set(k, []);
    map.get(k)?.push(t);
  }
  return map;
};

/**
 * @param {Track[]} list
 * @returns {Map<string, Track[]>} keyed by lowercased album
 */
const groupByAlbum = (list) => {
  /** @type {Map<string, Track[]>} */
  const map = new Map();
  for (const t of list) {
    const k = albumOf(t).toLowerCase();
    if (!map.has(k)) map.set(k, []);
    map.get(k)?.push(t);
  }
  return map;
};

/** Sort tracks by disc/track no, then title. */
/**
 * @param {Track[]} list
 */
const sortTracks = (list) =>
  [...list].sort((a, b) => {
    const da = a.tags?.disc?.no ?? 0;
    const db = b.tags?.disc?.no ?? 0;
    if (da !== db) return da - db;
    const ta = a.tags?.track?.no ?? 0;
    const tb = b.tags?.track?.no ?? 0;
    if (ta !== tb) return ta - tb;
    return trackTitle(a, "").localeCompare(trackTitle(b, ""));
  });

/**
 * @param {string[]} names
 */
const sortNames = (names) =>
  [...new Set(names)].sort((a, b) => a.localeCompare(b));

////////////////////////////////////////////
// PLAYBACK ACTIONS
////////////////////////////////////////////

const playPause = () => {
  const audioId = c.$queue.value?.now()?.id;
  if (!audioId) return;
  if (c.isPlaying()) c.$audio.value?.pause({ audioId });
  else c.$audio.value?.play({ audioId });
};

const next = () => c.$queue.value?.shift();
const previous = () => c.$queue.value?.unshift();

/**
 * Play a specific track immediately (added in front of the queue, then
 * shifted to — same pattern as the Blur/Winamp browsers).
 * @param {string} trackId
 */
const playTrack = (trackId) => {
  const q = c.$queue.value;
  if (!q) return;
  q.add({ inFront: true, trackIds: [trackId] });
  q.shift();
};

/**
 * Play a list of tracks immediately, replacing the upcoming queue.
 * @param {string[]} trackIds
 */
const playTracksNext = (trackIds) => {
  if (trackIds.length === 0) return;
  const q = c.$queue.value;
  if (!q) return;
  q.add({ inFront: true, trackIds });
  q.shift();
};

/**
 * Append tracks to the end of the manual queue.
 * @param {string[]} trackIds
 */
const addToQueue = (trackIds) => {
  if (trackIds.length === 0) return;
  const q = c.$queue.value;
  if (!q) return;
  q.add({ trackIds });
};

/**
 * Jump the queue to a flat index (past..., now, future...) and play it.
 * Uses shift/unshift so no duplicates are created.
 * @param {number} idx
 */
const playAtQueueIndex = (idx) => {
  const q = c.$queue.value;
  if (!q) return;
  const pastLen = queue.past().length;
  if (idx === pastLen) {
    // already current — restart from the beginning
    const audioId = queue.now()?.id;
    if (audioId) {
      c.$audio.value?.seek({ audioId, currentTime: 0 });
      c.$audio.value?.play({ audioId });
    }
    return;
  }
  if (idx < pastLen) {
    q.unshift({ by: pastLen - idx });
  } else {
    q.shift({ by: idx - pastLen });
  }
};

/** @param {number} percentage 0..1 */
const seekTo = (percentage) => {
  const audioId = c.$queue.value?.now()?.id;
  if (audioId) c.$audio.value?.seek({ audioId, percentage });
};

/** Nudge the scrubber by a number of steps (each ~5% of duration). */
/**
 * @param {number} steps
 */
const scrub = (steps) => {
  const audio = c.audio();
  const duration = audio?.duration() ?? 0;
  if (duration <= 0) return;
  const cur = audio?.currentTime() ?? 0;
  const pct = Math.max(0, Math.min(1, cur / duration + steps * 0.05));
  seekTo(pct);
};

////////////////////////////////////////////
// MENU NAVIGATION
////////////////////////////////////////////

/** @returns {MenuFrame} */
const rootMenu = () => {
  /** @type {MenuItem[]} */
  const items = [
    { label: "Now Playing", kind: "action", run: goNowPlaying },
    { label: "Queue", kind: "action", run: openQueue },
    { label: "Artists", kind: "folder", open: artistsMenu },
    { label: "Albums", kind: "folder", open: albumsMenu },
    { label: "Playlists", kind: "folder", open: playlistsMenu },
    { label: "Songs", kind: "folder", open: songsMenu },
    {
      label: "Shuffle",
      kind: "toggle",
      getOn: () => repeatShuffle.shuffle(),
      toggle: () => repeatShuffle.setShuffle(!repeatShuffle.shuffle()),
    },
    {
      label: "Repeat",
      kind: "toggle",
      getOn: () => repeatShuffle.repeat(),
      toggle: () => repeatShuffle.setRepeat(!repeatShuffle.repeat()),
    },
  ];
  return { title: "Music", items, selected: 0, scroll: 0 };
};

/** @returns {MenuFrame} */
const artistsMenu = () => {
  const byArtist = groupByArtist(tracks());
  const names = sortNames([...byArtist.keys()].map((k) => {
    const t = byArtist.get(k)?.[0];
    return t ? artistOf(t) : k;
  }));
  /** @type {MenuItem[]} */
  const items = names.map((name) => /** @type {MenuItem} */ ({
    label: name,
    kind: "folder",
    open: () => artistMenu(name),
  }));
  return { title: "Artists", items, selected: 0, scroll: 0 };
};

/**
 * @param {string} artist
 * @returns {MenuFrame}
 */
const artistMenu = (artist) => {
  const byArtist = groupByArtist(tracks());
  const artistTracks = byArtist.get(artist.toLowerCase()) ?? [];
  const albums = groupByAlbum(artistTracks);
  const albumNames = sortNames([...albums.keys()].map((k) => {
    const t = albums.get(k)?.[0];
    return t ? albumOf(t) : k;
  }));
  /** @type {MenuItem[]} */
  const items = [
    { label: "All Songs", kind: "folder", open: () => artistSongsMenu(artist) },
    ...albumNames.map((album) => /** @type {MenuItem} */ ({
      label: album,
      kind: "folder",
      open: () => albumMenu(album, artistTracks),
    })),
  ];
  return { title: artist, items, selected: 0, scroll: 0 };
};

/** @returns {MenuFrame} */
const albumsMenu = () => {
  const byAlbum = groupByAlbum(tracks());
  const names = sortNames([...byAlbum.keys()].map((k) => {
    const t = byAlbum.get(k)?.[0];
    return t ? albumOf(t) : k;
  }));
  /** @type {MenuItem[]} */
  const items = names.map((album) => /** @type {MenuItem} */ ({
    label: album,
    kind: "folder",
    open: () => albumMenu(album, tracks()),
  }));
  return { title: "Albums", items, selected: 0, scroll: 0 };
};

/**
 * @param {string} album
 * @param {Track[]} scope  tracks to search within (all, or one artist's)
 * @returns {MenuFrame}
 */
const albumMenu = (album, scope) => {
  const byAlbum = groupByAlbum(scope);
  const list = sortTracks(byAlbum.get(album.toLowerCase()) ?? []);
  const trackIds = list.map((t) => t.id);
  /** @type {MenuItem[]} */
  const items = [
    {
      label: "Play Next",
      kind: "action",
      icon: "ph-play",
      run: () => playTracksNext(trackIds),
      goNow: true,
    },
    {
      label: "Add to Queue",
      kind: "action",
      icon: "ph-plus",
      separator: true,
      run: () => addToQueue(trackIds),
    },
    ...list.map((t) => /** @type {MenuItem} */ ({
      label: trackTitle(t, t.id),
      value: trackArtist(t) ?? "",
      kind: "track",
      trackId: t.id,
    })),
  ];
  return { title: album, items, selected: 0, scroll: 0 };
};

/** @returns {MenuFrame} */
const songsMenu = () => {
  const list = [...tracks()].sort((a, b) =>
    trackTitle(a, "").localeCompare(trackTitle(b, ""))
  );
  const trackIds = list.map((t) => t.id);
  /** @type {MenuItem[]} */
  const items = [
    {
      label: "Play Next",
      kind: "action",
      icon: "ph-play",
      run: () => playTracksNext(trackIds),
      goNow: true,
    },
    {
      label: "Add to Queue",
      kind: "action",
      icon: "ph-plus",
      separator: true,
      run: () => addToQueue(trackIds),
    },
    ...list.map((t) => /** @type {MenuItem} */ ({
      label: trackTitle(t, t.id),
      value: trackArtist(t) ?? "",
      kind: "track",
      trackId: t.id,
    })),
  ];
  return { title: "Songs", items, selected: 0, scroll: 0 };
};

/** @returns {MenuFrame} */
const playlistsMenu = () => {
  const $items = computed(() => {
    const col = pathCollections.playlistItems.collection();
    const items = col.state === "loaded" ? col.data : [];
    const map = Playlist.gather(items);
    const names = sortNames([...map.values()].map((p) => p.name));
    return names.map((name) => /** @type {MenuItem} */ ({
      label: name,
      kind: "folder",
      open: () => playlistMenu(name),
    }));
  });
  return {
    title: "Playlists",
    items: $items(),
    selected: 0,
    scroll: 0,
    $items,
  };
};

/**
 * @param {string} name
 * @returns {MenuFrame}
 */
const playlistMenu = (name) => {
  const $items = computed(() => {
    const col = pathCollections.playlistItems.collection();
    const allItems = col.state === "loaded" ? col.data : [];
    const playlistItems = allItems.filter((item) => item.playlist === name);
    const ordered = Playlist.sort(playlistItems);
    const list = Playlist.filterByPlaylist(tracks(), ordered);
    const trackIds = list.map((t) => t.id);
    const actions = /** @type {MenuItem[]} */ ([
      {
        label: "Play Next",
        kind: "action",
        icon: "ph-play",
        run: () => playTracksNext(trackIds),
        goNow: true,
      },
      {
        label: "Add to Queue",
        kind: "action",
        icon: "ph-plus",
        separator: true,
        run: () => addToQueue(trackIds),
      },
    ]);
    return [
      ...actions,
      ...list.map((t) => /** @type {MenuItem} */ ({
        label: trackTitle(t, t.id),
        value: trackArtist(t) ?? "",
        kind: "track",
        trackId: t.id,
      })),
    ];
  });
  return { title: name, items: $items(), selected: 0, scroll: 0, $items };
};

/**
 * @param {string} artist
 * @returns {MenuFrame}
 */
const artistSongsMenu = (artist) => {
  const byArtist = groupByArtist(tracks());
  const list = sortTracks(byArtist.get(artist.toLowerCase()) ?? []);
  const trackIds = list.map((t) => t.id);
  /** @type {MenuItem[]} */
  const items = [
    {
      label: "Play Next",
      kind: "action",
      icon: "ph-play",
      run: () => playTracksNext(trackIds),
      goNow: true,
    },
    {
      label: "Add to Queue",
      kind: "action",
      icon: "ph-plus",
      separator: true,
      run: () => addToQueue(trackIds),
    },
    ...list.map((t) => /** @type {MenuItem} */ ({
      label: trackTitle(t, t.id),
      value: albumOf(t),
      kind: "track",
      trackId: t.id,
    })),
  ];
  return { title: artist, items, selected: 0, scroll: 0 };
};

/** The Queue screen (now + future queue items). */
const openQueue = () => {
  const items = queueItems();
  /** @type {MenuItem[]} */
  const menuItems = items.map((item, /** @type {number} */ i) => {
    const track = findTrack(item.id);
    return /** @type {MenuItem} */ ({
      label: trackTitle(track, item.id),
      value: trackArtist(track) ?? "",
      kind: "track",
      trackId: item.id,
      queueIndex: i,
    });
  });
  const nowIdx = items.findIndex((i) => i.state === "now");
  const frame = {
    title: "Queue",
    items: menuItems,
    selected: nowIdx >= 0 ? nowIdx : 0,
    scroll: 0,
  };
  pushFrame(frame);
};

const goNowPlaying = () => {
  scrubbing.value = false;
  screen.value = "now-playing";
};

/** @param {MenuFrame} frame */
const pushFrame = (frame) => {
  // Save scroll position on the current top frame before navigating deeper
  const current = stack.value[stack.value.length - 1];
  if (current) current.scroll = listScroll.value;
  batch(() => {
    listScroll.value = frame.scroll;
    stack.value = [...stack.value, frame];
    screen.value = "list";
  });
  el.viewList.style.transform = `translateY(${frame.scroll}px)`;
};

const popFrame = () => {
  if (stack.value.length > 1) {
    const prev = stack.value[stack.value.length - 2];
    batch(() => {
      stack.value = stack.value.slice(0, -1);
      screen.value = "list";
      listScroll.value = prev.scroll;
    });
    el.viewList.style.transform = `translateY(${prev.scroll}px)`;
  } else {
    // top level → go to now playing if something is playing, else stay
    if (queue.now()) goNowPlaying();
  }
};

/** MENU button: go back up. */
const menuButton = () => {
  if (screen.value === "now-playing") {
    // leave scrubber mode first, then go to menu
    if (scrubbing.value) {
      scrubbing.value = false;
      return;
    }
    screen.value = "list";
    return;
  }
  popFrame();
};

/** SELECT (center) button */
const selectButton = () => {
  if (screen.value === "now-playing") {
    // toggle scrubber mode (iPod: center button cycles views)
    scrubbing.value = !scrubbing.value;
    return;
  }
  const frame = stack.value[stack.value.length - 1];
  if (!frame) return;
  const item = frame.items[frame.selected];
  if (!item) return;
  if (item.kind === "action") {
    item.run?.();
    if (item.goNow) goNowPlaying();
  } else if (item.kind === "folder") {
    const next = item.open?.();
    if (next) pushFrame(next);
  } else if (item.kind === "track") {
    if (item.queueIndex !== undefined) {
      playAtQueueIndex(item.queueIndex);
    } else if (item.trackId) {
      playTrack(item.trackId);
    }
    goNowPlaying();
  } else if (item.kind === "toggle") {
    item.toggle?.();
  }
};

/** Scroll within the active list by a number of steps. */
/**
 * @param {number} steps
 */
const scrollList = (steps) => {
  if (screen.value !== "list") return;
  const frame = stack.value[stack.value.length - 1];
  if (!frame) return;
  const count = frame.items.length;
  if (count === 0) return;
  const next = Math.max(0, Math.min(count - 1, frame.selected + steps));
  if (next === frame.selected) return;
  const updated = { ...frame, selected: next };
  stack.value = [...stack.value.slice(0, -1), updated];
};

////////////////////////////////////////////
// RENDER
////////////////////////////////////////////

const PLAY_ICON = /** @type {const} */ ('<i class="ph-fill ph-play"></i>');
const PAUSE_ICON = /** @type {const} */ ('<i class="ph-fill ph-pause"></i>');

// Title bar + view visibility
effect(() => {
  const s = screen.value;
  const playing = c.isPlaying();
  const isScrub = scrubbing.value;

  el.title.textContent = s === "now-playing" ? "Now Playing" : (currentTitle());

  // status glyph (play/pause) on now-playing
  if (s === "now-playing") {
    el.status.innerHTML = isScrub
      ? '<i class="ph-bold ph-arrows-clockwise"></i>'
      : (playing ? PAUSE_ICON : PLAY_ICON);
  } else {
    el.status.innerHTML = "";
  }

  el.viewNow.style.display = s === "now-playing" ? "flex" : "none";
  el.viewList.style.display = s === "now-playing" ? "none" : "block";
});

/** @returns {string} */
function currentTitle() {
  const frame = stack.value[stack.value.length - 1];
  return frame ? frame.title : "Music";
}

// Now-playing content
effect(() => {
  const track = c.currentTrack();
  const audio = c.audio();
  const hasTrack = !!c.$queue.value?.now();
  const isScrub = scrubbing.value;

  el.nowTitle.textContent = track
    ? trackTitle(track, track.id)
    : "Nothing Playing";
  el.nowArtist.textContent = track ? (trackArtist(track) ?? "") : "";

  const currentTime = audio?.currentTime() ?? 0;
  const duration = audio?.duration() ?? 0;
  const progress = duration > 0 ? currentTime / duration : 0;

  el.progressFill.style.width = `${progress * 100}%`;
  el.progressKnob.style.left = `${progress * 100}%`;
  el.timeElapsed.textContent = formatTime(currentTime);
  el.timeRemaining.textContent = `-${
    formatTime(Math.max(0, duration - currentTime))
  }`;

  el.progress.style.opacity = hasTrack ? "1" : "0";
  // enlarge the knob while scrubbing, like the iPod's diamond
  el.progressKnob.classList.toggle("is-scrubbing", isScrub);
});

// List view rendering (menu stack) — virtual list
effect(() => {
  if (screen.value !== "list") return;
  const frame = stack.value[stack.value.length - 1];
  if (!frame) return;

  // re-render when selection changes
  const _sel = frame.selected;
  // also re-render toggles when shuffle/repeat change
  const _shuffle = repeatShuffle.shuffle();
  const _repeat = repeatShuffle.repeat();
  void _shuffle;
  void _repeat;

  // live frames (e.g. Playlists) re-read their source signals so the
  // list updates when the underlying collection changes
  if (frame.$items) {
    frame.items = frame.$items();
    if (frame.selected >= frame.items.length) {
      frame.selected = Math.max(0, frame.items.length - 1);
    }
  }

  if (frame.items.length === 0) {
    litRender(
      html`
        <li
          class="ipod__list-item"
          style="position:absolute;top:0"><span class="ipod__list-item__label" style="opacity:0.7">Empty</span></li>
      `,
      el.viewList,
    );
    updateScrollbar(0, 0, el.viewport.clientHeight);
    return;
  }

  const total = frame.items.length;
  const viewportH = el.viewport.clientHeight;

  // Adjust scroll so the selected row stays visible
  const sel = frame.selected;
  const itemTop = sel * ROW_HEIGHT;
  let target = listScroll.value;
  if (itemTop + ROW_HEIGHT > -listScroll.value + viewportH) {
    target = -(itemTop + ROW_HEIGHT - viewportH);
  } else if (itemTop < -listScroll.value) {
    target = -itemTop;
  }
  if (target > 0) target = 0;
  listScroll.value = target;
  if (frame) frame.scroll = target;
  el.viewList.style.transform = `translateY(${target}px)`;
  updateScrollbar(total, target, viewportH);

  const scrollPos = -target; // positive px scrolled from top

  // Calculate visible range with a buffer
  const BUFFER = 2;
  const startIdx = Math.max(0, Math.floor(scrollPos / ROW_HEIGHT) - BUFFER);
  const endIdx = Math.min(
    total,
    Math.ceil((scrollPos + viewportH) / ROW_HEIGHT) + BUFFER,
  );

  litRender(
    html`
      ${frame.items.slice(startIdx, endIdx).map(
        (/** @type {MenuItem} */ item, /** @type {number} */ offset) => {
          const i = startIdx + offset;
          const selected = i === _sel;
          const marker = item.kind === "track" && isCurrentTrack(item.trackId);
          const actionIcon = item.kind === "action" && item.icon
            ? html`<span class="ipod__list-item__marker"><i class="ph-fill ${item.icon}"></i></span>`
            : null;
          const cls = "ipod__list-item" +
            (selected ? " ipod__list-item--selected" : "") +
            (item.separator ? " ipod__list-item--separator" : "");
          return html`
            <li
              class="${cls}"
              style="top:${i * ROW_HEIGHT}px"
            >
              ${marker
                ? html`<span class="ipod__list-item__marker"><i class="ph-fill ph-play"></i></span>`
                : actionIcon}
              <span class="ipod__list-item__label">${item.label}</span>
              ${item.kind === "toggle"
                ? html`<span class="ipod__list-item__value">${
                  item.getOn?.() ? "On" : "Off"
                }</span>`
                : item.value
                ? html`<span class="ipod__list-item__value">${item.value}</span>`
                : null}
            </li>
          `;
        },
      )}
    `,
    el.viewList,
  );
});

/**
 * @param {string | undefined} trackId
 */
function isCurrentTrack(trackId) {
  const now = queue.now();
  return !!now && now.id === trackId;
}

/**
 * Update the iPod-style scrollbar thumb.
 * @param {number} total
 * @param {number} target
 * @param {number} viewportH
 */
function updateScrollbar(total, target, viewportH) {
  if (total <= 0) {
    el.scrollbar.hidden = true;
    return;
  }
  const contentH = total * ROW_HEIGHT;
  if (contentH <= viewportH) {
    el.scrollbar.hidden = true;
    return;
  }
  el.scrollbar.hidden = false;
  const thumbH = Math.max(20, (viewportH / contentH) * viewportH);
  const maxScroll = contentH - viewportH;
  const top = (Math.abs(target) / maxScroll) * (viewportH - thumbH);
  el.scrollbar.style.height = `${thumbH}px`;
  el.scrollbar.style.transform = `translateY(${top}px)`;
}

////////////////////////////////////////////
// CLICK WHEEL INTERACTION
////////////////////////////////////////////

/**
 * Angle (degrees) of a point relative to the wheel center.
 * 0° = right, 90° = down (screen coords), measured clockwise.
 * @param {number} x
 * @param {number} y
 * @param {DOMRect} rect
 */
const angleDeg = (x, y, rect) => {
  const cx = rect.left + rect.width / 2;
  const cy = rect.top + rect.height / 2;
  return (Math.atan2(y - cy, x - cx) * 180) / Math.PI;
};

/**
 * Distance from center for a point, vs wheel rect.
 * @param {number} x
 * @param {number} y
 * @param {DOMRect} rect
 */
const distFromCenter = (x, y, rect) => {
  const cx = rect.left + rect.width / 2;
  const cy = rect.top + rect.height / 2;
  return Math.hypot(x - cx, y - cy);
};

/** Wheel drag → scroll (lists) or seek (now-playing scrubber). */
const WHEEL_STEP_DEG = 14; // degrees of rotation per step

let drag =
  /** @type {null | { last: number; accum: number; moved: boolean }} */ (null);

el.wheel.addEventListener("pointerdown", (event) => {
  const rect = el.wheel.getBoundingClientRect();
  const centerR = (rect.width * (78 / 224)) / 2; // center button radius
  const dist = distFromCenter(event.clientX, event.clientY, rect);

  // Center button handles its own click; ignore starts inside it.
  if (dist <= centerR) return;

  el.wheel.setPointerCapture(event.pointerId);
  el.wheel.classList.add("is-pressing");
  drag = {
    last: angleDeg(event.clientX, event.clientY, rect),
    accum: 0,
    moved: false,
  };
});

el.wheel.addEventListener("pointermove", (event) => {
  if (!drag) return;
  const rect = el.wheel.getBoundingClientRect();
  const a = angleDeg(event.clientX, event.clientY, rect);

  // smallest signed delta
  let delta = a - drag.last;
  if (delta > 180) delta -= 360;
  else if (delta < -180) delta += 360;

  drag.accum += delta;
  drag.last = a;

  if (Math.abs(drag.accum) > 6) drag.moved = true;

  while (Math.abs(drag.accum) >= WHEEL_STEP_DEG) {
    const dir = drag.accum > 0 ? 1 : -1;
    if (screen.value === "now-playing" && scrubbing.value) {
      scrub(dir);
    } else {
      scrollList(dir);
    }
    drag.accum -= dir * WHEEL_STEP_DEG;
  }
});

el.wheel.addEventListener("pointerup", (event) => {
  el.wheel.classList.remove("is-pressing");
  if (!drag) return;
  const wasMoved = drag.moved;
  drag = null;

  if (wasMoved) return; // it was a scroll/seek gesture, not a tap

  // Tap on the ring → quadrant button
  const rect = el.wheel.getBoundingClientRect();
  const centerR = (rect.width * (78 / 224)) / 2;
  const dist = distFromCenter(event.clientX, event.clientY, rect);
  if (dist <= centerR) return; // center handled separately

  const a = angleDeg(event.clientX, event.clientY, rect);
  // top (≈ -90) = MENU, right (≈ 0) = next, bottom (≈ 90) = play, left (≈ 180) = prev
  if (a >= -45 && a < 45) next();
  else if (a >= 45 && a < 135) playPause();
  else if (a >= 135 || a < -135) previous();
  else menuButton();
});

el.wheel.addEventListener("pointercancel", () => {
  el.wheel.classList.remove("is-pressing");
  drag = null;
});

// Center select button
el.select.addEventListener("click", selectButton);

// Seek by tapping/clicking the progress bar
el.progress.addEventListener("click", (event) => {
  const rect = el.progress.getBoundingClientRect();
  const percentage = (event.clientX - rect.left) / rect.width;
  seekTo(Math.max(0, Math.min(1, percentage)));
});

////////////////////////////////////////////
// KEYBOARD SHORTCUTS
////////////////////////////////////////////

document.addEventListener("keydown", (event) => {
  if (event.target instanceof HTMLInputElement) return;
  switch (event.key) {
    case " ":
      event.preventDefault();
      playPause();
      break;
    case "ArrowRight":
      next();
      break;
    case "ArrowLeft":
      previous();
      break;
    case "ArrowUp":
      if (screen.value === "now-playing" && scrubbing.value) scrub(-1);
      else scrollList(-1);
      break;
    case "ArrowDown":
      if (screen.value === "now-playing" && scrubbing.value) scrub(1);
      else scrollList(1);
      break;
    case "Enter":
      selectButton();
      break;
    case "Escape":
    case "Backspace":
      menuButton();
      break;
  }
});

////////////////////////////////////////////
// BOOT + AUTO-FILL QUEUE WHEN EMPTY
////////////////////////////////////////////

// Initialise the menu stack with the root.
stack.value = [rootMenu()];

effect(() => {
  const fingerprint = queue.supplyFingerprint();
  const now = queue.now();
  const future = queue.future();

  // Wait until tracks have been processed, and the queue is empty.
  if (fingerprint === undefined) return;
  if (now !== null || future.length > 0) return;
  if (autoFilled) return;

  const all = scopedTracks.tracks();
  if (all.length === 0) return;

  autoFilled = true;
  queue.supply({ trackIds: all.map((t) => t.id) });
  queue.fill({ augment: true, amount: 50, shuffled: true });
});

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
