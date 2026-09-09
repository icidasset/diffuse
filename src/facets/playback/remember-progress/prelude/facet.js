import * as Output from "~/common/output.js";
import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

////////////////////////////////////////////
// CONFIG
////////////////////////////////////////////

/** Tracks at least this long (seconds) get their position remembered. */
const LONG_S = 30 * 60;

/** Smallest remembered position (seconds); anything earlier is treated as "start". */
const RESUME_MARGIN_S = 5;

/** Once a track is this far through, its position is forgotten (treat as finished). */
const CLEAR_NEAR_END = 0.98;

/**
 * How often (ms) the position map is written to `localStorage`. This keeps the
 * device-local copy precise (a reload only loses a few seconds), independent of
 * how often we sync to the settings collection.
 */
const LOCAL_SAVE_INTERVAL_MS = 5 * 1000;

/**
 * Minimum interval (ms) between periodic syncs of the position map to the
 * `settings` collection. This is what keeps us well clear of PDS rate limits.
 */
const SYNC_MIN_INTERVAL_MS = 5 * 60 * 1000;

/** Debounce (ms) for event-driven syncs (pause / end) so rapid toggling can't burst. */
const SYNC_EVENT_DEBOUNCE_MS = 1000;

/** Local precise store: `{ [trackId]: seconds }`. */
const LOCAL_KEY = "facets/playback/remember-progress/position";

/** Synced settings key holding the position map `{ [trackId]: seconds }`. */
const SYNC_KEY = "sh.diffuse.playback.remember-progress";

////////////////////////////////////////////
// INSTALL
////////////////////////////////////////////

const INSTALLED = new WeakMap();

/**
 * This feature facet gives Diffuse the "long track resume" behaviour without
 * touching any component. For every track that's at least 30 minutes long it
 * keeps a running position map, persisted two ways:
 *
 *  - locally to `localStorage` for precision (every few seconds), so a reload
 *    loses only a moment of playback;
 *  - to the `settings` collection for sync, written periodically at a 5-minute
 *    minimum interval and immediately on pause / track end / page close, so the
 *    synced position matches where you actually stopped.
 *
 * Restored positions are applied purely via `audio.seek()`; the `tracks`
 * collection is never modified.
 */
effect(() => {
  const audio = foundation.signals.engine.audio();
  const outputEl = foundation.signals.orchestrator.output();
  if (!audio || !outputEl || INSTALLED.get(audio)) return;
  INSTALLED.set(audio, true);

  output = outputEl;
  install(audio);
});

/**
 * @type {import("~/components/orchestrator/output/element.js").CLASS | null}
 */
let output = null;

/** @type {Record<string, number>} */
let map = {};

/**
 * @param {import("~/components/engine/audio/element.js").CLASS} audio
 */
async function install(audio) {
  // Authoritative in-memory map: the precise local copy wins per track, with
  // the synced map from settings as the cross-device fallback.
  map = Object.assign(await readSynced().catch(() => ({})), readLocal());

  // Tracks that are no longer supplied stop being tracked, so a re-supplied
  // track can restore again next time it's activated.
  const restored = new Set();
  const wasPlaying = new Set();

  effect(() => {
    const ids = new Set(audio.items().map((i) => i.id));
    for (const id of [...restored]) {
      if (!ids.has(id)) restored.delete(id);
    }
  });

  effect(() => {
    for (const item of audio.items()) {
      const st = audio.state(item.id);
      if (!st) continue;

      const duration = st.duration();
      // Not long enough (or duration unknown yet) — never remembered.
      if (duration < LONG_S) continue;

      const currentTime = st.currentTime();
      const isPlaying = st.isPlaying();

      // Never act on background / preloaded tracks.
      if (st.isPreload()) continue;

      // Essentially finished: forget the position so it restarts next time,
      // and drop it from both stores.
      if (currentTime >= CLEAR_NEAR_END * duration) {
        if (map[item.id] != null) {
          delete map[item.id];
          writeLocalNow();
          flushSyncedDebounced();
        }
        wasPlaying.delete(item.id);
        continue;
      }

      const sec = Math.floor(currentTime);
      if (sec > RESUME_MARGIN_S) {
        if (sec !== map[item.id]) {
          map[item.id] = sec;
          scheduleLocalWrite();
          scheduleSyncedTotal();
        }
      }

      if (isPlaying) {
        wasPlaying.add(item.id);
      } else if (wasPlaying.delete(item.id) && map[item.id] != null) {
        // Just stopped (paused or otherwise) — sync the precise stop point.
        writeLocalNow();
        flushSyncedDebounced();
      }

      // Restore once, when the media is ready. Gated on the active (non-preload)
      // track's readiness rather than on `isPlaying`, so it fires reliably the
      // moment the long track is loaded — even before the user hits play. The
      // `restored` set makes this one-shot per activation; it's cleared above
      // when the item leaves the engine.
      const loading = st.loadingState();
      if (loading === "loaded" && !restored.has(item.id)) {
        restored.add(item.id);
        const saved = map[item.id];
        if (saved != null && saved > RESUME_MARGIN_S) {
          audio.seek({ audioId: item.id, currentTime: saved });
        }
      }
    }
  });

  // Capture the precise final position when the page is abandoned.
  globalThis.addEventListener("pagehide", onPageHide);
}

function onPageHide() {
  writeLocalNow();
  flushSyncedNow();
}

////////////////////////////////////////////
// LOCAL (PRECISION)
////////////////////////////////////////////

let localTimer = /** @type {ReturnType<typeof setTimeout> | null} */ (null);

function scheduleLocalWrite() {
  if (localTimer) return;
  localTimer = setTimeout(() => {
    localTimer = null;
    writeLocalNow();
  }, LOCAL_SAVE_INTERVAL_MS);
}

function writeLocalNow() {
  try {
    localStorage.setItem(LOCAL_KEY, JSON.stringify(map));
  } catch {
    // Best-effort; storage can be full or unavailable.
  }
}

function readLocal() {
  try {
    const raw = localStorage.getItem(LOCAL_KEY);
    const parsed = raw ? JSON.parse(raw) : {};
    return parsed && typeof parsed === "object" ? parsed : {};
  } catch {
    return {};
  }
}

////////////////////////////////////////////
// SYNCED (SETTINGS)
////////////////////////////////////////////

/**
 * Read the synced position map from the settings collection.
 *
 * @returns {Promise<Record<string, number>>}
 */
async function readSynced() {
  if (!output) return {};
  const settings = await Output.data(output.settings);
  const setting = settings.find((s) => s.key === SYNC_KEY);
  if (!setting) return {};
  const parsed = JSON.parse(setting.value);
  return parsed && typeof parsed === "object" ? parsed : {};
}

let lastSyncedWrite = 0;
let syncedTimer = /** @type {ReturnType<typeof setTimeout> | null} */ (null);
let syncedEventTimer = /** @type {ReturnType<typeof setTimeout> | null} */ (null);

/**
 * Schedule a periodic sync honoring the 5-minute minimum interval. The write
 * reads the map at fire time, so it always captures the latest positions.
 */
function scheduleSyncedTotal() {
  if (syncedTimer) return;
  const delay = Math.max(0, lastSyncedWrite + SYNC_MIN_INTERVAL_MS - Date.now());
  syncedTimer = setTimeout(() => {
    syncedTimer = null;
    flushSynced();
  }, delay);
}

/** Event-driven precise sync, debounced against rapid stop/start toggling. */
function flushSyncedDebounced() {
  if (syncedEventTimer) return;
  syncedEventTimer = setTimeout(() => {
    syncedEventTimer = null;
    flushSynced();
  }, SYNC_EVENT_DEBOUNCE_MS);
}

/** Immediate sync (page close / unload). */
function flushSyncedNow() {
  if (syncedEventTimer) {
    clearTimeout(syncedEventTimer);
    syncedEventTimer = null;
  }
  flushSynced();
}

/** Write the whole position map as a single `settings` entry. */
async function flushSynced() {
  if (!output) return;
  lastSyncedWrite = Date.now();
  try {
    const settings = await Output.data(output.settings);
    const value = JSON.stringify(map);
    const existing = settings.find((s) => s.key === SYNC_KEY);
    const updated = existing
      ? settings.map((s) =>
        s.key === SYNC_KEY ? { ...s, value } : s
      )
      : [
        ...settings,
        {
          $type: /** @type {"sh.diffuse.output.setting"} */ (
            "sh.diffuse.output.setting"
          ),
          id: crypto.randomUUID(),
          key: SYNC_KEY,
          value,
        },
      ];
    await output.settings.save(updated);
  } catch {
    // A failed write is non-fatal; the next scheduled flush will retry.
  }
}
