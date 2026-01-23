import "@components/input/opensubsonic/element.js";
import "@components/input/s3/element.js";
import "@components/orchestrator/input/element.js";
import "@components/orchestrator/output/element.js";
import "@components/orchestrator/process-tracks/element.js";
import "@components/orchestrator/queue-tracks/element.js";
import "@components/orchestrator/search-tracks/element.js";
import "@components/orchestrator/sources/element.js";
import "@components/processor/metadata/element.js";

import * as Input from "@components/configurator/input/element.js";
import * as Queue from "@components/engine/queue/element.js";
import * as Search from "@components/processor/search/element.js";

import { component } from "@common/element.js";
import { effect, signal, untracked } from "@common/signal.js";

import "./browser/element.js";
import "./configurators/input/element.js";
import "./configurators/output.js";
import "./window/element.js";

import WindowManager from "./window-manager/element.js";
import WebampElement from "./webamp/element.js";

/**
 * @import {OutputElement} from "@components/output/types.d.ts"
 * @import {Track} from "@definitions/types.d.ts"
 */

const input = component(Input);
const queue = component(Queue);
const search = component(Search);

/** @type {OutputElement<Track[]> | null} */
const output = document.querySelector("#output");
if (!output) throw new Error("Missing output element");

globalThis.queue = queue;
globalThis.output = output;

////////////////////////////////////////////
// 📡
////////////////////////////////////////////

const $playlist = signal(/** @type {Array<Track>} */ ([]), {
  eager: true,
});

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
  const now = untracked(queue.now);
  const past = untracked(queue.past);
  const future = queue.future();

  const playlist = [
    ...past,
    ...(now ? [now] : []),
    ...future,
  ];

  const lengthLastPlaylist = untracked($playlist.get).length;
  const tracksToAdd = playlist.slice(lengthLastPlaylist);

  $playlist.value = playlist;

  tracksToAdd.forEach((t) => ampElement.addTrack(t));

  if (lengthLastPlaylist === 0 && playlist.length) {
    amp.store.dispatch({ type: "BUFFER_TRACK", id: 0 });
  }
});

/**
 * Keep note of when search is ready.
 */
const tracksPromise = Promise.withResolvers();

effect(() => {
  const state = output.tracks.state();
  if (state !== "loaded") return;

  const cacheId = search.cacheId();
  if (cacheId === undefined) return;

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

  // Automatically insert track if there isn't any
  if (!queue.now()) await queue.shift();
}

function windowManager() {
  const w = document.body.querySelector("dtw-window-manager");
  if (w instanceof WindowManager) return w;
  return null;
}
