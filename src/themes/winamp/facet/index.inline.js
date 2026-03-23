import foundation from "~/common/foundation.js";
import { effect, untracked } from "~/common/signal.js";

import WindowManager from "~/themes/winamp/window-manager/element.js";
import WebampElement from "~/themes/winamp/webamp/element.js";

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
const queue = await foundation.engine.queue();
const scopedTracks = await foundation.orchestrator.scopedTracks();
const search = await foundation.processor.search();

await foundation.orchestrator.sources();
await foundation.orchestrator.processTracks({ disableWhenReady: true });

await import("~/themes/winamp/browser/element.js");
await import("~/themes/winamp/window/element.js");

/** @type {OutputElement | null} */
const output = document.querySelector("#output");
if (!output) throw new Error("Missing output element");

globalThis.queue = queue;
globalThis.output = output;

////////////////////////////////////////////
// 📡
////////////////////////////////////////////

/** @type {Record<string, number>} */
const index = {};

/** @type {boolean} */
let initiatedPlaylist = false;

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

const ampElement = document.querySelector("dtw-webamp");
if (ampElement instanceof WebampElement === false) {
  throw new Error("Missing webamp element");
}

const amp = ampElement.amp;

// Override track loader
const loadFromUrl = amp.media.loadFromUrl.bind(amp.media);

/**
 * @param {string} uri
 * @param {boolean} autoPlay
 */
async function loadOverride(uri, autoPlay) {
  if (uri.startsWith("blob:")) {
    return await loadFromUrl(uri, autoPlay);
  }

  const resp = await input.resolve({ method: "GET", uri });
  if (!resp) throw new Error("Failed to resolve URI");
  if (resp && "stream" in resp) {
    throw new Error("Webamp does not support playing streams.");
  }

  return await loadFromUrl(resp.url, autoPlay);
}

amp.media.loadFromUrl = loadOverride.bind(amp.media);

////////////////////////////////////////////
// 📡
////////////////////////////////////////////

/**
 * Whenever the queue changes update the playlist.
 */
effect(() => {
  const past = untracked(() => queue.past());
  const now = untracked(() => queue.now());
  const future = queue.future();
  const list = [...past, ...(now ? [now] : []), ...future];

  /** @type {Record<string, number>} */
  const newIdx = {};

  list.forEach((item) => {
    newIdx[item.id] = (newIdx[item.id] ?? 0) + 1;
  });

  /** @type {Track[]} */
  const tracksToAdd = [];

  const tracksCol = output.tracks.collection();
  const tracksList = tracksCol.state === "loaded" ? tracksCol.data : [];

  Object.entries(newIdx).forEach(([id, n]) => {
    const x = index[id] ?? 0;
    if (n > x) {
      const track = tracksList.find((t) => t.id === id);
      if (track) tracksToAdd.push(track);
      index[id] = x + 1;
    }
  });

  tracksToAdd.forEach((t) => ampElement.addTrack(t));

  if (!initiatedPlaylist && tracksToAdd.length) {
    initiatedPlaylist = true;
    amp.store.dispatch({ type: "BUFFER_TRACK", id: 0 });
  }
});

/**
 * Fill queue supply with available tracks.
 */
effect(() => {
  const tracks = scopedTracks.tracks();
  queue.supply({ trackIds: tracks.map((t) => t.id) });
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
