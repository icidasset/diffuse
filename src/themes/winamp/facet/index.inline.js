import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

import WindowManager from "~/themes/winamp/window-manager/element.js";
import WebampElement from "~/themes/winamp/webamp/element.js";
import { setAudioEngine, setCurrentTrackIdResolver } from "~/themes/winamp/webamp/media.js";

// Set doc title
document.title = "Winamp | Diffuse";

// Doc loader
const main = /** @type {HTMLElement} */ (document.querySelector("main"));
main.classList.add("has-loaded");

/**
 * @import {OutputElement} from "~/components/output/types.d.ts"
 * @import {Track} from "~/definitions/types.d.ts"
 */

const input = await foundation.configurator.input();
const audio = await foundation.engine.audio();
const queue = await foundation.engine.queue();
const repeatShuffle = await foundation.engine.repeatShuffle();
const scopedTracks = await foundation.orchestrator.scopedTracks();
const search = await foundation.processor.search();

await foundation.orchestrator.sources();
await foundation.orchestrator.processTracks({ disableWhenReady: true });
await foundation.orchestrator.queueAudio();

await import("~/themes/winamp/browser/element.js");
await import("~/themes/winamp/window/element.js");

/** @type {OutputElement | null} */
const output = document.querySelector("#output");
if (!output) throw new Error("Missing output element");

globalThis.queue = queue;
globalThis.output = output;

// Provide refs to the media class
setAudioEngine(audio);
setCurrentTrackIdResolver(() => {
  const idx = amp.store.getState().playlist?.currentTrack;
  return idx != null ? webampIndexToTrackId.get(idx) : undefined;
});

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

const ampElement = document.querySelector("dtw-webamp");
if (ampElement instanceof WebampElement === false) {
  throw new Error("Missing webamp element");
}

const amp = ampElement.amp;

////////////////////////////////////////////
// 📡
////////////////////////////////////////////

// Sync audio engine volume → webamp
effect(() => {
  amp.store.dispatch({ type: "SET_VOLUME", volume: Math.round(audio.volume() * 100) });
});

// Sync Diffuse repeat → webamp
effect(() => {
  const repeat = repeatShuffle.repeat();
  if (amp.store.getState().media.repeat !== repeat) {
    amp.store.dispatch({ type: "TOGGLE_REPEAT" });
  }
});

/** Maps webamp playlist index → Diffuse track ID */
/** @type {Map<number, string>} */
const webampIndexToTrackId = new Map();

/**
 * Set when we are pushing changes to webamp so the store subscriber
 * doesn't try to sync the queue back in response.
 */
let isWebampUpdate = false;

/**
 * Rebuild the webamp playlist to exactly match the Diffuse queue.
 */
effect(() => {
  const past = queue.past();
  const now = queue.now();
  const future = queue.future();
  const list = [...past, ...(now ? [now] : []), ...future];

  const tracksCol = output.tracks.collection();
  if (tracksCol.state !== "loaded") return;
  const tracksList = tracksCol.data;

  const existing = amp.getPlaylistTracks();

  isWebampUpdate = true;

  amp.store.dispatch(
    /**
     * @param {any} dispatch
     */
    (dispatch) => {
      // Remove all current tracks
      if (existing.length > 0) {
        dispatch({ type: "REMOVE_TRACKS", ids: existing.map((t) => t.id) });
      }

      webampIndexToTrackId.clear();

      // Re-add every queue item in order
      list.forEach((item, idx) => {
        const track = tracksList.find((t) => t.id === item.id);
        if (!track) return;

        dispatch({
          type: "ADD_TRACK_FROM_URL",
          url: track.uri,
          duration: track.stats?.duration != null
            ? track.stats.duration / 1000
            : undefined,
          defaultName: undefined,
          id: idx,
          atIndex: idx,
        });

        dispatch({
          type: "SET_MEDIA_DURATION",
          duration: track.stats?.duration != null
            ? track.stats.duration / 1000
            : undefined,
          id: idx,
        });

        dispatch({
          type: "SET_MEDIA_TAGS",
          artist: track.tags?.artist,
          title: track.tags?.title,
          album: track.tags?.album,
          sampleRate: track.stats?.sampleRate ?? 44000,
          bitrate: track.stats?.bitrate ?? 192000,
          numberOfChannels: 2,
          id: idx,
        });

        webampIndexToTrackId.set(idx, item.id);
      });

      // Point webamp at the current queue item
      const nowIdx = now ? list.findIndex((item) => item.id === now.id) : -1;
      if (nowIdx !== -1) {
        dispatch({ type: "BUFFER_TRACK", id: nowIdx });
      }
    },
  );

  isWebampUpdate = false;
});

/**
 * Keep note of when search is ready.
 */
const tracksPromise = Promise.withResolvers();

effect(() => {
  const col = output.tracks.collection();
  if (col.state !== "loaded") return;

  const fingerprintSearch = search.supplyFingerprint();
  if (fingerprintSearch === undefined) return;

  const fingerprintQueue = queue.supplyFingerprint();
  if (fingerprintQueue === undefined) return;

  tracksPromise.resolve("loaded");
});

/**
 * Keep Diffuse's queue in sync when webamp's active track changes.
 */
let prevWebampTrack = /** @type {number | null} */ (null);
let prevWebampRepeat = amp.store.getState().media.repeat;
let milkdropRandomized = false;

amp.store.subscribe(() => {
  // Pick a random milkdrop preset the first time presets load
  if (!milkdropRandomized) {
    const presets = amp.store.getState().milkdrop?.presets;
    if (presets?.length > 0) {
      milkdropRandomized = true;
      amp.store.dispatch({
        type: "SELECT_PRESET_AT_INDEX",
        index: Math.floor(Math.random() * presets.length),
        transitionType: 0,
      });
    }
  }

  if (isWebampUpdate) return;

  // Sync webamp repeat → Diffuse
  const currentRepeat = amp.store.getState().media.repeat;
  if (currentRepeat !== prevWebampRepeat) {
    prevWebampRepeat = currentRepeat;
    repeatShuffle.setRepeat(currentRepeat);
  }

  const currentTrack = amp.store.getState().playlist?.currentTrack;
  if (currentTrack === prevWebampTrack) return;
  prevWebampTrack = currentTrack;
  if (currentTrack == null) return;

  const targetId = webampIndexToTrackId.get(currentTrack);
  if (!targetId || queue.now()?.id === targetId) return;

  const past = queue.past();
  const future = queue.future();

  const pastIdx = past.findLastIndex((item) => item.id === targetId);
  if (pastIdx !== -1) {
    const steps = past.length - pastIdx;
    for (let i = 0; i < steps; i++) queue.unshift();
    return;
  }

  const futureIdx = future.findIndex((item) => item.id === targetId);
  if (futureIdx !== -1) {
    for (let i = 0; i <= futureIdx; i++) queue.shift();
    // return;
  }

  // Track not in queue navigation — insert at front and make it current
  // queue.add({ trackIds: [targetId], inFront: true });
  // queue.shift();
});

////////////////////////////////////////////
// DESKTOP
////////////////////////////////////////////

// Open associated window when click desktop items
document.body.querySelectorAll(".desktop__item").forEach((element) => {
  if (element instanceof HTMLElement) {
    element.addEventListener("dblclick", () => {
      const f = element.querySelector("label")?.getAttribute("for");
      if (f) return windowManager()?.toggleWindow(f);
    });
  }
});

// Add batch
document.body.querySelector("#desktop-batch")?.addEventListener(
  "dblclick",
  () => {
    tracksPromise.promise.then(() => {
      addBatch();
    });
  },
);

// Toggle Winamp if click that desktop item
let winampIsShown = true;

document.body.querySelector("#desktop-winamp")?.addEventListener(
  "dblclick",
  () => {
    if (winampIsShown) {
      amp.close();
    } else {
      amp.reopen();
      winampIsShown = true;
    }
  },
);

amp.onClose(() => winampIsShown = false);

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

async function addBatch() {
  await queue.fill({ augment: true, amount: 50, shuffled: true });
}

function windowManager() {
  const w = document.body.querySelector("dtw-window-manager");
  if (w instanceof WindowManager) return w;
  return null;
}
