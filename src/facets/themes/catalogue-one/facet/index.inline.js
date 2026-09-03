import { html, render as litRender } from "lit-html";

import foundation from "~/common/foundation.js";
import { computed, effect, signal } from "~/common/signal.js";
import * as Playlist from "~/common/playlist.js";

/**
 * @import { Track } from "~/definitions/types.d.ts"
 * @import { Item as QueueItem } from "@specs/components/engine/queue/types.d.ts"
 */

// Set doc title
foundation.setup({ title: "Catalogue One | Diffuse" });

////////////////////////////////////////////
// SETUP — engines & orchestrators
////////////////////////////////////////////

const [queue, repeatShuffle] = await Promise.all([
  foundation.engine.queue(),
  foundation.engine.repeatShuffle(),
]);

await Promise.all([
  customElements.whenDefined(queue.localName),
  customElements.whenDefined(repeatShuffle.localName),
]);

const scope = await foundation.engine.scope();
const scopedTracks = await foundation.orchestrator.scopedTracks();
const output = await foundation.orchestrator.output();
await foundation.orchestrator.sources();
await foundation.orchestrator.processTracks({ disableWhenReady: true });
await foundation.orchestrator.queueAudio();
const controller = await foundation.orchestrator.controller();
const artwork = await foundation.orchestrator.artwork();
const coverGroups = await foundation.orchestrator.coverGroups();
const favourites = await foundation.orchestrator.favourites();
await foundation.orchestrator.mediaSession();
await foundation.configurator.input();

/** @type {import("~/components/orchestrator/controller/element.js").default} */
const c = controller;

await Promise.all([
  customElements.whenDefined(scopedTracks.localName),
  customElements.whenDefined(output.localName),
  customElements.whenDefined(controller.localName),
  customElements.whenDefined(artwork.localName),
  customElements.whenDefined(coverGroups.localName),
  customElements.whenDefined(favourites.localName),
]);

////////////////////////////////////////////
// DOM ELEMENTS
////////////////////////////////////////////

const el = {
  content: /** @type {HTMLElement} */ (document.querySelector("#content")),
  queuePanel:
    /** @type {HTMLElement} */ (document.querySelector("#queue-panel")),
  queueToggle: /** @type {HTMLButtonElement} */ (
    document.querySelector("#queue-toggle")
  ),
  playerBar: /** @type {HTMLElement} */ (document.querySelector("#player-bar")),
  search: /** @type {HTMLInputElement} */ (document.querySelector("#search")),
  playlistSelect: /** @type {HTMLSelectElement} */ (
    document.querySelector("#playlist-select")
  ),
  tabs: /** @type {NodeListOf<HTMLButtonElement>} */ (
    document.querySelectorAll(".cat-tab")
  ),
};

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

const viewMode = signal(
  /** @type {"albums" | "artists" | "songs"} */ ("albums"),
);

const detailItem = signal(
  /** @type {{ type: "album"; key: string } | { type: "artist"; key: string } | null} */ (
    null
  ),
);

const queueOpen = signal(false);

////////////////////////////////////////////
// HELPERS
////////////////////////////////////////////

/** @param {Track} t */
const artistOf = (t) =>
  t.tags?.artist ?? t.tags?.albumartist ?? "Unknown Artist";
/** @param {Track} t */
const albumOf = (t) => t.tags?.album ?? "Unknown Album";
/** @param {Track} t */
const titleOf = (t) => t.tags?.title ?? t.id;

/** @param {number} seconds */
const formatTime = (seconds) => {
  if (!Number.isFinite(seconds) || seconds < 0) seconds = 0;
  const m = Math.floor(seconds / 60);
  const s = Math.floor(seconds % 60);
  return `${m}:${String(s).padStart(2, "0")}`;
};

/** @param {Track} t */
const trackDuration = (t) => {
  const ms = t.stats?.duration;
  if (!ms) return "";
  return formatTime(ms / 1000);
};

/** @param {Uint8Array} bytes */
function detectMime(bytes) {
  if (bytes[0] === 0xFF && bytes[1] === 0xD8) return "image/jpeg";
  if (bytes[0] === 0x89 && bytes[1] === 0x50) return "image/png";
  if (bytes[0] === 0x47 && bytes[1] === 0x49) return "image/gif";
  if (bytes[0] === 0x52 && bytes[1] === 0x49) return "image/webp";
  return "image/jpeg";
}

////////////////////////////////////////////
// COMPUTED — tracks, groups, playlists
////////////////////////////////////////////

/** All tracks from the output collection. */
const allTracks = computed(() => {
  const col = output.tracks.collection();
  return col.state === "loaded" ? col.data : [];
});

/** Scoped + filtered tracks. */
const tracks = computed(() => scopedTracks.tracks() ?? []);

/** Playlists grouped by name. */
const playlists = computed(() => {
  const col = output.playlistItems.collection();
  if (col.state !== "loaded") return [];
  const map = Playlist.gather(col.data);
  return [...map.values()].sort((a, b) => a.name.localeCompare(b.name));
});

/** Flat album list deduplicated from scoped tracks. */
const albums = computed(() => {
  const list = tracks();
  /** @type {Map<string, { key: string; name: string; artist: string; track: Track; count: number }>} */
  const map = new Map();
  for (const t of list) {
    const key = albumOf(t).toLowerCase();
    const existing = map.get(key);
    if (existing) {
      existing.count++;
    } else {
      map.set(key, {
        key,
        name: albumOf(t),
        artist: artistOf(t),
        track: t,
        count: 1,
      });
    }
  }
  return [...map.values()].sort((a, b) => a.name.localeCompare(b.name));
});

/** Flat artist list deduplicated from scoped tracks. */
const artists = computed(() => {
  const list = tracks();
  /** @type {Map<string, { key: string; name: string; track: Track; count: number }>} */
  const map = new Map();
  for (const t of list) {
    const key = artistOf(t).toLowerCase();
    const existing = map.get(key);
    if (existing) {
      existing.count++;
    } else {
      map.set(key, {
        key,
        name: artistOf(t),
        track: t,
        count: 1,
      });
    }
  }
  return [...map.values()].sort((a, b) => a.name.localeCompare(b.name));
});

/** Tracks for the currently-open detail view. */
const detailTracks = computed(() => {
  const item = detailItem.value;
  if (!item) return [];

  const list = tracks();
  if (item.type === "album") {
    return list
      .filter((t) => albumOf(t).toLowerCase() === item.key)
      .sort((a, b) => {
        const da = a.tags?.disc?.no ?? 0;
        const db = b.tags?.disc?.no ?? 0;
        if (da !== db) return da - db;
        const ta = a.tags?.track?.no ?? 0;
        const tb = b.tags?.track?.no ?? 0;
        if (ta !== tb) return ta - tb;
        return titleOf(a).localeCompare(titleOf(b));
      });
  } else {
    return list
      .filter((t) => artistOf(t).toLowerCase() === item.key)
      .sort((a, b) => {
        const aa = albumOf(a).toLowerCase();
        const ab = albumOf(b).toLowerCase();
        if (aa !== ab) return aa.localeCompare(ab);
        const da = a.tags?.disc?.no ?? 0;
        const db = b.tags?.disc?.no ?? 0;
        if (da !== db) return da - db;
        const ta = a.tags?.track?.no ?? 0;
        const tb = b.tags?.track?.no ?? 0;
        if (ta !== tb) return ta - tb;
        return titleOf(a).localeCompare(titleOf(b));
      });
  }
});

/** Queue items: [past..., now, future...] */
const queueItems = computed(() => {
  const past = queue.past();
  const now = queue.now();
  const future = queue.future();
  /** @type {(QueueItem & { state: "past" | "now" | "future" })[]} */
  const items = [];
  for (const p of past) items.push({ ...p, state: "past" });
  if (now) items.push({ ...now, state: "now" });
  for (const f of future) items.push({ ...f, state: "future" });
  return items;
});

/** Total queue count for the badge. */
const queueCount = computed(() => {
  return queue.future().length + (queue.now() ? 1 : 0);
});

////////////////////////////////////////////
// ARTWORK CACHE
////////////////////////////////////////////

/** @type {Map<string, string | null>} */
const artCache = new Map();
/** @type {Set<string>} */
const pendingArt = new Set();
/** @type {{ key: string; track: Track }[]} */
const artQueue = [];
let artActive = 0;
const MAX_ART_CONCURRENT = 4;

/**
 * Grid cards and queue items carry a `data-art-key` container whose cover is
 * filled lazily; they do NOT render their own content inside the template so
 * lit-html never tracks their internals. Detail header and player-bar art are
 * rendered eagerly by their own templates instead, so they're excluded here.
 */
const ART_CONTAINER_SELECTOR =
  ".cat-card__art[data-art-key], .cat-queue__item-art[data-art-key]";

/**
 * Store an artwork-cache entry for `key`. The cache is unbounded: covers are
 * small and their blob data is released by GC once the referencing `<img>`
 * leaves the DOM (matching the `blur` theme, which keeps every cover URL with
 * no eviction or `revokeObjectURL`).
 * @param {string} key
 * @param {string | null} value
 */
function cacheArt(key, value) {
  if (artCache.has(key)) artCache.delete(key); // refresh LRU position
  artCache.set(key, value);
}

/**
 * Fetch artwork for a given key (album or artist key) using a representative track.
 * Results are cached as blob URLs (or null if no art found).
 * @param {string} key
 * @param {Track} track
 */
function fetchArt(key, track) {
  if (artCache.has(key)) return;
  if (pendingArt.has(key)) return;
  pendingArt.add(key);
  artQueue.push({ key, track });
  drainArtQueue();
}

function drainArtQueue() {
  while (artActive < MAX_ART_CONCURRENT && artQueue.length > 0) {
    const job = artQueue.shift();
    if (!job) break;
    artActive++;
    doFetchArt(job.key, job.track);
  }
}

/**
 * @param {string} key
 * @param {Track} track
 */
async function doFetchArt(key, track) {
  try {
    // Bounded upstream (orchestrator drops a cover after 60s and clears its
    // in-flight entry), so a hanging download can't wedge the queue: this
    // promise always settles, and each slot is always released.
    const bytes = await artwork.get(track);
    if (bytes) {
      const mime = detectMime(bytes);
      const url = URL.createObjectURL(new Blob([/** @type {BlobPart} */ (bytes)], { type: mime }));
      cacheArt(key, url);
    } else {
      cacheArt(key, null);
    }
  } catch {
    // don't cache on error — allow retry
  } finally {
    pendingArt.delete(key);
    artActive--;
    drainArtQueue();
  }
  if (artCache.get(key)) renderArtForKey(key);
}

/**
 * Swap a freshly fetched cover into every rendered grid card / queue item for
 * `key` so the image appears in place without re-rendering the whole list
 * (which would rebuild the DOM and could reset the scroll position). Detail
 * header and player-bar art are rendered eagerly by their own templates.
 * @param {string} key
 */
function renderArtForKey(key) {
  const url = artCache.get(key);
  if (!url) return;
  // Match by attribute value, not by interpolating `key` into a CSS selector.
  // Album/artist names contain arbitrary chars (' " \ ...) that would make
  // `[data-art-key="${key}"]` an invalid selector and throw, silently
  // breaking the cover swap for that item.
  for (const node of document.querySelectorAll(ART_CONTAINER_SELECTOR)) {
    if ((/** @type {HTMLElement} */ (node)).dataset.artKey !== key) continue;
    renderArtForContainer(/** @type {HTMLElement} */ (node));
  }
}

/**
 * Observe artwork containers that are (near) visible so we only fetch artwork
 * for items that are actually on screen. Elements are tagged with
 * `data-art-key` and `data-art-track-id` at render time (without fetching);
 * when one scrolls into view we fetch just that item's artwork. Rooted at the
 * document viewport so both the scrolled `.cat-content` panel and the queue
 * panel are covered (their own `overflow` clipping still correctly reports
 * non-intersecting items). Only intersecting items enter the fetch queue, so
 * we never load artwork for cards that aren't (about to be) visible.
 */
/** @type {IntersectionObserver | undefined} */
let artObserver = undefined;

/** Cards that have scrolled into view, pending a debounced batch fetch. */
/** @type {Map<string, Track>} */
const pendingVisibleArt = new Map();
/** @type {ReturnType<typeof setTimeout> | undefined} */
let artFetchDebounce = undefined;

function armArtObserver() {
  artObserver?.disconnect();
  artObserver = undefined;
  // Drop any pending batch from the previous view so we don't fetch artwork
  // for cards that are no longer shown.
  clearTimeout(artFetchDebounce);
  artFetchDebounce = undefined;
  pendingVisibleArt.clear();

  artObserver = new IntersectionObserver(
    (entries) => {
      let hasNew = false;
      for (const entry of entries) {
        const target = /** @type {HTMLElement} */ (entry.target);
        if (!entry.isIntersecting) continue;
        const key = target.dataset.artKey;
        if (!key || artCache.has(key) || pendingVisibleArt.has(key)) continue;
        const trackId = target.dataset.artTrackId;
        const track = trackId ? findTrack(trackId) : undefined;
        // Do NOT unobserve here. If the fetch for this card later fails or is
        // dropped (timeout, transient error, or a re-arm clearing the pending
        // batch), leaving it observed lets the next scroll re-entry retry it.
        // Otherwise a failed cover would be unobserved and never attempted
        // again, which is why covers could stop appearing after a while.
        if (key && track) {
          pendingVisibleArt.set(key, track);
          hasNew = true;
        }
      }
      if (!hasNew) return;

      // Batch fetches so a rapid scroll doesn't dispatch one fetch per card.
      clearTimeout(artFetchDebounce);
      artFetchDebounce = setTimeout(() => {
        for (const [key, track] of pendingVisibleArt) {
          fetchArt(key, track);
        }
        pendingVisibleArt.clear();
      }, 150);
    },
    { rootMargin: "200px" },
  );

  for (const target of document.querySelectorAll(ART_CONTAINER_SELECTOR)) {
    const elTarget = /** @type {HTMLElement} */ (target);
    const key = elTarget.dataset.artKey;
    if (!key) continue;
    if (artCache.has(key) || pendingArt.has(key)) continue;
    artObserver.observe(elTarget);
  }
}

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

/** Play a track immediately. @param {string} trackId */
const playTrack = (trackId) => {
  const q = c.$queue.value;
  if (!q) return;
  q.add({ inFront: true, trackIds: [trackId] });
  q.shift();
};

/** Play a list of tracks, replacing the upcoming queue. @param {string[]} trackIds */
const playTracks = (trackIds) => {
  if (trackIds.length === 0) return;
  const q = c.$queue.value;
  if (!q) return;
  q.add({ inFront: true, trackIds });
  q.shift();
};

/** Append tracks to the end of the queue. @param {string[]} trackIds */
const addToQueue = (trackIds) => {
  if (trackIds.length === 0) return;
  const q = c.$queue.value;
  if (!q) return;
  q.add({ trackIds });
};

/** Jump to a flat queue index. @param {number} idx */
const playAtQueueIndex = (idx) => {
  const q = c.$queue.value;
  if (!q) return;
  const pastLen = queue.past().length;
  if (idx === pastLen) {
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

/** Toggle favourite on a track. @param {Track} track */
const toggleFavourite = (track) => {
  favourites.toggle(track);
};

/** Find a track by ID from the full collection. @param {string} id */
const findTrack = (id) => allTracks().find((t) => t.id === id);

/** Is this track currently playing? @param {string} trackId */
const isCurrentTrack = (trackId) => {
  const now = queue.now();
  return !!now && now.id === trackId;
};

////////////////////////////////////////////
// CONTEXT MENU
////////////////////////////////////////////

/** @type {{ x: number; y: number; track: Track } | null} */
let contextMenuState = null;

/**
 * @param {number} x
 * @param {number} y
 * @param {Track} track
 */
function openContextMenu(x, y, track) {
  contextMenuState = { x, y, track };
  renderContextMenu();
}

function closeContextMenu() {
  contextMenuState = null;
  renderContextMenu();
}

let contextMenuEl = /** @type {HTMLDivElement | null} */ (null);

function renderContextMenu() {
  if (contextMenuEl) {
    contextMenuEl.remove();
    contextMenuEl = null;
  }
  if (!contextMenuState) return;

  const { x, y, track } = contextMenuState;
  const isFav = favourites.isFavourite(track);

  contextMenuEl = document.createElement("div");
  contextMenuEl.className = "cat-menu";
  contextMenuEl.style.left = `${Math.min(x, globalThis.innerWidth - 220)}px`;
  contextMenuEl.style.top = `${Math.min(y, globalThis.innerHeight - 200)}px`;

  litRender(
    html`
      <button
        class="cat-menu__item"
        @click=${() => {
          playTrack(track.id);
          closeContextMenu();
        }}
      >
        <i class="ph-fill ph-play"></i>
        <span>Play now</span>
      </button>
      <button
        class="cat-menu__item"
        @click=${() => {
          addToQueue([track.id]);
          closeContextMenu();
        }}
      >
        <i class="ph-bold ph-plus"></i>
        <span>Add to queue</span>
      </button>
      <button
        class="cat-menu__item"
        @click=${() => {
          toggleFavourite(track);
          closeContextMenu();
        }}
      >
        <i class="ph-fill ph-heart" style="${isFav
          ? "color: var(--cat-accent)"
          : ""}"></i>
        <span>${isFav ? "Remove from favourites" : "Add to favourites"}</span>
      </button>
    `,
    contextMenuEl,
  );

  document.body.appendChild(contextMenuEl);
}

// Close context menu on outside click
document.addEventListener("click", (e) => {
  if (
    contextMenuEl && !contextMenuEl.contains(/** @type {Node} */ (e.target))
  ) {
    closeContextMenu();
  }
});

document.addEventListener("keydown", (e) => {
  if (e.key === "Escape") closeContextMenu();
});

////////////////////////////////////////////
// RENDER — content area
////////////////////////////////////////////

const artPlaceholder = html`
  <div class="cat-card__art-placeholder">
    <i class="ph-fill ph-music-notes"></i>
  </div>
`;

const queueArtPlaceholder = html`
  <div class="cat-queue__item-art-placeholder">
    <i class="ph-fill ph-music-notes"></i>
  </div>
`;

/**
 * @param {string} key
 * @param {Track} track
 */
function artBlock(key, track) {
  // Artwork is not fetched here — the card is tagged so `armArtObserver` can
  // request artwork lazily once it scrolls into view. The container is rendered
  // empty (no lit-html content part) so its children can be inserted/replaced
  // freely by `renderArtForContainer` without corrupting lit-html's committed
  // part nodes.
  return html`
    <div
      class="cat-card__art"
      data-art-key="${key}"
      data-art-track-id="${track.id}"
    ></div>
  `;
}

/**
 * Render (or refresh) the art content of a single `data-art-key` container:
 * the cached cover image if available, otherwise the placeholder. Rendered
 * through lit-html so updates stay consistent with its internal bookkeeping,
 * and the placeholder variant is chosen to match the container type (grid card
 * vs queue item).
 * @param {HTMLElement} container
 */
function renderArtForContainer(container) {
  const key = container.dataset.artKey;
  if (!key) return;
  const url = artCache.get(key);
  if (url) {
    litRender(html`<img src="${url}" alt="" />`, container);
    return;
  }
  const placeholder = container.classList.contains("cat-queue__item-art")
    ? queueArtPlaceholder
    : artPlaceholder;
  litRender(placeholder, container);
}

/**
 * Ensure every rendered art container shows a placeholder when its cover isn't
 * cached yet. Run after each render so fresh cards show the placeholder icon
 * immediately.
 */
function renderArtPlaceholders() {
  for (const node of document.querySelectorAll(ART_CONTAINER_SELECTOR)) {
    renderArtForContainer(/** @type {HTMLElement} */ (node));
  }
}

/**
 * @param {string} key
 * @param {Track} track
 */
function detailArtBlock(key, track) {
  fetchArt(key, track);
  const url = artCache.get(key);
  if (url) {
    return html`<img src="${url}" alt="" />`;
  }
  return html`
    <div class="cat-detail__art-placeholder">
      <i class="ph-fill ph-music-notes"></i>
    </div>
  `;
}

function renderAlbumGrid() {
  const list = albums();
  if (list.length === 0) return renderEmpty();

  return html`
    <div class="cat-grid">
      ${list.map(
        (album) =>
          html`
            <button
              class="cat-card"
              @click=${() => {
                detailItem.value = { type: "album", key: album.key };
              }}
            >
              <div class="cat-card__art">
                ${artBlock(album.key, album.track)}
              </div>
              <div class="cat-card__info">
                <p class="cat-card__title">${album.name}</p>
                <p class="cat-card__subtitle">${album.artist}</p>
              </div>
            </button>
          `,
      )}
    </div>
  `;
}

function renderArtistGrid() {
  const list = artists();
  if (list.length === 0) return renderEmpty();

  return html`
    <div class="cat-grid">
      ${list.map(
        (artist) =>
          html`
            <button
              class="cat-card"
              @click=${() => {
                detailItem.value = { type: "artist", key: artist.key };
              }}
            >
              <div class="cat-card__art">
                ${artBlock(artist.key, artist.track)}
              </div>
              <div class="cat-card__info">
                <p class="cat-card__title">${artist.name}</p>
                <p class="cat-card__subtitle">${artist
                  .count} track${artist.count !== 1 ? "s" : ""}</p>
              </div>
            </button>
          `,
      )}
    </div>
  `;
}

function renderSongList() {
  const list = tracks();
  if (list.length === 0) return renderEmpty();

  return html`
    <ul class="cat-songs">
      ${list.map(
        (track, i) =>
          html`
            <li
              class="cat-track ${isCurrentTrack(track.id)
                ? "cat-track--current"
                : ""}"
              @click=${() => playTrack(track.id)}
              @contextmenu=${/** @param {MouseEvent} e */ (e) => {
                e.preventDefault();
                openContextMenu(e.clientX, e.clientY, track);
              }}
            >
              <span class="cat-track__index">${i + 1}</span>
              <div class="cat-track__info">
                <div class="cat-track__title">${titleOf(track)}</div>
                <div class="cat-track__artist">${artistOf(track)}</div>
              </div>
              <span class="cat-track__duration">${trackDuration(track)}</span>
              <button
                class="cat-track__menu"
                @click=${/** @param {Event} e */ (e) => {
                  e.stopPropagation();
                  const target = /** @type {HTMLElement} */ (e.currentTarget);
                  const rect = target.getBoundingClientRect();
                  openContextMenu(rect.right, rect.bottom, track);
                }}
              >
                <i class="ph-bold ph-dots-three-vertical"></i>
              </button>
            </li>
          `,
      )}
    </ul>
  `;
}

function renderDetailView() {
  const item = detailItem.value;
  if (!item) return null;

  const list = detailTracks();
  if (list.length === 0) {
    detailItem.value = null;
    return null;
  }

  const isAlbum = item.type === "album";
  const title = isAlbum ? albumOf(list[0]) : artistOf(list[0]);
  const subtitle = isAlbum
    ? artistOf(list[0])
    : `${list.length} track${list.length !== 1 ? "s" : ""}`;

  const artKey = item.key;
  const firstTrack = list[0];
  const trackIds = list.map((t) => t.id);

  return html`
    <div class="cat-detail">
      <button class="cat-back" @click=${() => {
        detailItem.value = null;
      }}>
        <i class="ph-bold ph-arrow-left"></i>
        <span>Back</span>
      </button>

      <div class="cat-detail__header">
        <div class="cat-detail__art" data-art-key="${artKey}">
          ${detailArtBlock(artKey, firstTrack)}
        </div>
        <div class="cat-detail__meta">
          <h2 class="cat-detail__title">${title}</h2>
          <p class="cat-detail__subtitle">${subtitle}</p>
          <div class="cat-detail__actions">
            <button class="cat-btn" @click=${() => playTracks(trackIds)}>
              <i class="ph-fill ph-play"></i>
              <span>Play</span>
            </button>
            <button
              class="cat-btn cat-btn--secondary"
              @click=${() => addToQueue(trackIds)}
            >
              <i class="ph-bold ph-plus"></i>
              <span>Add to queue</span>
            </button>
          </div>
        </div>
      </div>

      <ul class="cat-track-list">
        ${list.map(
          (track, i) =>
            html`
              <li
                class="cat-track ${isCurrentTrack(track.id)
                  ? "cat-track--current"
                  : ""}"
                @click=${() => playTrack(track.id)}
                @contextmenu=${/** @param {MouseEvent} e */ (e) => {
                  e.preventDefault();
                  openContextMenu(e.clientX, e.clientY, track);
                }}
              >
                <span class="cat-track__index">${i + 1}</span>
                <div class="cat-track__info">
                  <div class="cat-track__title">${titleOf(track)}</div>
                  ${!isAlbum
                    ? html`<div class="cat-track__artist">${
                      albumOf(track)
                    }</div>`
                    : null}
                </div>
                <span class="cat-track__duration">${trackDuration(track)}</span>
                <button
                  class="cat-track__menu"
                  @click=${/** @param {Event} e */ (e) => {
                    e.stopPropagation();
                    const target = /** @type {HTMLElement} */ (e.currentTarget);
                    const rect = target.getBoundingClientRect();
                    openContextMenu(rect.right, rect.bottom, track);
                  }}
                >
                  <i class="ph-bold ph-dots-three-vertical"></i>
                </button>
              </li>
            `,
        )}
      </ul>
    </div>
  `;
}

function renderEmpty() {
  const term = scope.searchTerm();
  const playlist = scope.playlist();
  let msg = "No tracks found.\nAdd an audio source to get started.";
  if (term) msg = `No tracks match "${term}".`;
  else if (playlist) msg = "This playlist is empty.";

  return html`
    <div class="cat-empty">
      <i class="ph-bold ph-record"></i>
      <p>${msg}</p>
    </div>
  `;
}

function renderContent() {
  const detail = detailItem.value;
  const mode = viewMode.value;

  // Read scoped tracks to register signal dependency
  const _tracks = tracks();
  void _tracks;

  if (detail) {
    litRender(renderDetailView(), el.content);
    armArtObserver();
    return;
  }

  if (mode === "albums") litRender(renderAlbumGrid(), el.content);
  else if (mode === "artists") litRender(renderArtistGrid(), el.content);
  else litRender(renderSongList(), el.content);
  renderArtPlaceholders();
  armArtObserver();
}

////////////////////////////////////////////
// RENDER — queue panel
////////////////////////////////////////////

function renderQueue() {
  const items = queueItems();
  const past = items.filter((i) => i.state === "past");
  const now = items.filter((i) => i.state === "now");
  const future = items.filter((i) => i.state === "future");

  /**
   * @param {QueueItem & { state: "past" | "now" | "future" }} item
   */
  const renderItem = (item) => {
    const track = findTrack(item.id);
    const title = track ? titleOf(track) : item.id;
    const artist = track ? artistOf(track) : "";
    const flatIndex = items.indexOf(item);

    return html`
      <div
        class="cat-queue__item ${item.state === "now"
          ? "cat-queue__item--current"
          : ""}"
        @click=${() => playAtQueueIndex(flatIndex)}
      >
        <div
          class="cat-queue__item-art"
          data-art-key="${track ? albumOf(track).toLowerCase() : ""}"
          data-art-track-id="${track ? track.id : ""}"
        ></div>
        <div class="cat-queue__item-info">
          <div class="cat-queue__item-title">${title}</div>
          <div class="cat-queue__item-artist">${artist}</div>
        </div>
        ${item.state === "now"
          ? html`<i class="ph-fill ph-speaker-high cat-queue__item-playing"></i>`
          : null}
      </div>
    `;
  };

  return html`
    <div class="cat-queue__header">
      <span>Queue</span>
      <div class="cat-queue__header-actions">
        ${future.length > 0
          ? html`
            <button class="cat-queue__clear" @click=${() => {
              const q = c.$queue.value;
              if (!q) return;
              q.clear({});
            }}>
              <i class="ph-bold ph-x"></i>
              Clear
            </button>
          `
          : null}
        <button class="cat-queue__close" @click=${() => {
          queueOpen.value = false;
        }}>
          <i class="ph-bold ph-x"></i>
        </button>
      </div>
    </div>
    <div class="cat-queue__list">
      ${now.length > 0
        ? html`
          <div class="cat-queue__section">
            <p class="cat-queue__section-title">Now Playing</p>
            ${now.map(renderItem)}
          </div>
        `
        : null}
      ${future.length > 0
        ? html`
          <div class="cat-queue__section">
            <p class="cat-queue__section-title">Up Next</p>
            ${future.map(renderItem)}
          </div>
        `
        : null}
      ${past.length > 0
        ? html`
          <div class="cat-queue__section">
            <p class="cat-queue__section-title">History</p>
            ${past.map(renderItem)}
          </div>
        `
        : null}
      ${items.length === 0
        ? html`
          <div class="cat-empty" style="padding: var(--space-lg)">
            <p>Queue is empty</p>
          </div>
        `
        : null}
    </div>
  `;
}

function renderQueuePanel() {
  const isOpen = queueOpen.value;
  el.queuePanel.hidden = !isOpen;
  el.queueToggle.setAttribute("data-active", isOpen ? "t" : "f");
  if (!isOpen) return;
  litRender(renderQueue(), el.queuePanel);
  renderArtPlaceholders();
  armArtObserver();
}

////////////////////////////////////////////
// RENDER — player bar
////////////////////////////////////////////

function renderPlayer() {
  const track = c.currentTrack();
  const audio = c.audio();
  const playing = c.isPlaying();
  const hasTrack = !!c.$queue.value?.now();

  const currentTime = audio?.currentTime() ?? 0;
  const duration = track?.stats?.duration
    ? track.stats.duration / 1000
    : (audio?.duration() ?? 0);
  const progress = duration > 0 ? currentTime / duration : 0;

  const repeat = repeatShuffle.repeat();
  const shuffle = repeatShuffle.shuffle();

  const artKey = track ? albumOf(track).toLowerCase() : "";
  const artUrl = track ? artCache.get(artKey) : undefined;

  litRender(
    html`
      <div class="cat-player__art" data-art-key="${track ? artKey : ""}">
        ${hasTrack && track
          ? (artUrl ? html`<img src="${artUrl}" alt="" />` : html`
            <div
              class="cat-player__art-placeholder"><i class="ph-fill ph-music-notes"></i></div>
          `)
          : html`
            <div
              class="cat-player__art-placeholder"><i class="ph-fill ph-music-notes"></i></div>
          `}
      </div>

      <div class="cat-player__info">
        <div class="cat-player__title">${hasTrack && track
          ? titleOf(track)
          : "Nothing playing"}</div>
        <div class="cat-player__artist">${hasTrack && track
          ? artistOf(track)
          : ""}</div>
      </div>

      <div class="cat-player__controls">
        <button
          class="cat-player__btn ${shuffle ? "cat-player__btn--active" : ""}"
          title="Shuffle"
          @click=${() => repeatShuffle.setShuffle(!shuffle)}
        >
          <i class="ph-fill ph-shuffle"></i>
        </button>
        <button class="cat-player__btn" title="Previous" @click=${previous}
          ?disabled=${!hasTrack}>
          <i class="ph-fill ph-skip-back"></i>
        </button>
        <button
          class="cat-player__btn cat-player__btn--play"
          title="Play/Pause"
          @click=${playPause}
          ?disabled=${!hasTrack}
        >
          <i class="ph-fill ${playing ? "ph-pause" : "ph-play"}"></i>
        </button>
        <button class="cat-player__btn" title="Next" @click=${next}
          ?disabled=${!hasTrack}>
          <i class="ph-fill ph-skip-forward"></i>
        </button>
        <button
          class="cat-player__btn ${repeat ? "cat-player__btn--active" : ""}"
          title="Repeat"
          @click=${() => repeatShuffle.setRepeat(!repeat)}
        >
          <i class="ph-fill ph-repeat"></i>
        </button>
      </div>

      <div class="cat-player__progress">
        <span class="cat-player__time">${formatTime(currentTime)}</span>
        <input
          type="range"
          class="cat-player__seek"
          min="0"
          max="100"
          step="0.1"
          value="${progress * 100}"
          ?disabled=${!hasTrack || duration <= 0}
          @input=${/** @param {InputEvent} e */ (e) => {
            const val = parseFloat(
              /** @type {HTMLInputElement} */ (e.target).value,
            );
            seekTo(val / 100);
          }}
        />
        <span class="cat-player__time cat-player__time--remaining">
          ${duration > 0 ? formatTime(duration) : "0:00"}
        </span>
      </div>
    `,
    el.playerBar,
  );
}

////////////////////////////////////////////
// RENDER — playlist select
////////////////////////////////////////////

function renderPlaylistSelect() {
  const list = playlists();
  const current = scope.playlist();

  litRender(
    html`
      <option value="">All music</option>
      ${list.map(
        (p) =>
          html`<option value="${p.name}" ?selected=${p.name === current}>
          ${p.name}
        </option>`,
      )}
    `,
    el.playlistSelect,
  );
}

////////////////////////////////////////////
// TABS
////////////////////////////////////////////

function updateTabs() {
  const mode = viewMode.value;
  el.tabs.forEach((tab) => {
    tab.setAttribute("data-active", tab.dataset.view === mode ? "t" : "f");
  });
}

el.tabs.forEach((tab) => {
  tab.addEventListener("click", () => {
    viewMode.value = /** @type {"albums" | "artists" | "songs"} */ (
      tab.dataset.view
    );
    detailItem.value = null;
  });
});

////////////////////////////////////////////
// SEARCH
////////////////////////////////////////////

/** @type {ReturnType<typeof setTimeout>} */
let searchDebounce;
el.search.addEventListener("input", () => {
  clearTimeout(searchDebounce);
  const value = el.search.value;
  searchDebounce = setTimeout(() => {
    scope.setSearchTerm(value || undefined);
  }, 250);
});

// Restore saved search term
const savedSearch = scope.searchTerm();
if (savedSearch) el.search.value = savedSearch;

////////////////////////////////////////////
// PLAYLIST SELECT
////////////////////////////////////////////

el.playlistSelect.addEventListener("change", () => {
  const value = el.playlistSelect.value;
  scope.setPlaylist(value || undefined);
  detailItem.value = null;
});

////////////////////////////////////////////
// QUEUE TOGGLE
////////////////////////////////////////////

el.queueToggle.addEventListener("click", () => {
  queueOpen.value = !queueOpen.value;
});

////////////////////////////////////////////
// KEYBOARD SHORTCUTS
////////////////////////////////////////////

document.addEventListener("keydown", (e) => {
  // Ignore when typing in inputs
  const tag = (e.target instanceof HTMLElement) ? e.target.tagName : "";
  if (tag === "INPUT" || tag === "SELECT" || tag === "TEXTAREA") return;

  if (e.key === " ") {
    e.preventDefault();
    playPause();
  } else if (e.key === "ArrowRight" && e.shiftKey) {
    next();
  } else if (e.key === "ArrowLeft" && e.shiftKey) {
    previous();
  }
});

////////////////////////////////////////////
// EFFECTS — re-render on signal changes
////////////////////////////////////////////

// Content area
effect(() => {
  // Register dependencies
  viewMode.value;
  detailItem.value;
  const _tracks = tracks();
  const _albums = albums();
  const _artists = artists();
  void _tracks;
  void _albums;
  void _artists;
  renderContent();
});

// Tabs
effect(() => {
  viewMode.value;
  updateTabs();
});

// Queue panel
effect(() => {
  queueOpen.value;
  const _items = queueItems();
  void _items;
  renderQueuePanel();
});

// Queue toggle badge
effect(() => {
  const count = queueCount();
  const existing = el.queueToggle.querySelector(".cat-queue-toggle__badge");
  if (count > 0) {
    if (!existing) {
      const badge = document.createElement("span");
      badge.className = "cat-queue-toggle__badge";
      el.queueToggle.append(badge);
    }
    const badge = el.queueToggle.querySelector(".cat-queue-toggle__badge");
    if (badge) badge.textContent = String(count);
  } else {
    if (existing) existing.remove();
  }
});

// Player bar
effect(() => {
  const _track = c.currentTrack();
  const _playing = c.isPlaying();
  const _audio = c.audio();
  const _repeat = repeatShuffle.repeat();
  const _shuffle = repeatShuffle.shuffle();
  void _track;
  void _playing;
  void _audio;
  void _repeat;
  void _shuffle;
  renderPlayer();
});

// Playlist select
effect(() => {
  const _playlists = playlists();
  void _playlists;
  renderPlaylistSelect();
});

// Auto-fetch artwork for currently playing track
effect(() => {
  const track = c.currentTrack();
  if (track) {
    const key = albumOf(track).toLowerCase();
    fetchArt(key, track);
  }
});

////////////////////////////////////////////
// BOOT — auto-fill queue when empty
////////////////////////////////////////////

let autoFilled = false;

effect(() => {
  const fingerprint = queue.supplyFingerprint();
  const now = queue.now();
  const future = queue.future();

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
