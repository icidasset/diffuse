import "@components/input/opensubsonic/element.js";
import "@components/input/s3/element.js";
import "@components/orchestrator/input/element.js";
import "@components/orchestrator/output/element.js";
// import "@components/orchestrator/process-tracks/element.js";
import "@components/orchestrator/queue-tracks/element.js";
import "@components/orchestrator/search-tracks/element.js";
import "@components/processor/metadata/element.js";

import * as Input from "@components/configurator/input/element.js";
import * as Queue from "@components/engine/queue/element.js";
import * as Search from "@components/processor/search/element.js";

import { component } from "@common/element.js";
import { effect, signal, untracked } from "@common/signal.js";

import "./browser/element.js";
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

const currBase = 0;

const $currTrack = signal(/** @type {null | number} */ (null));
const $playlist = signal(/** @type {Set<string>} */ (new Set()), {
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

/**
 * Observe changes in Webamp's internal store.
 */
amp.store.subscribe(() => {
  const state = amp.store.getState();

  if (
    state.playlist.currentTrack !== null &&
    state.playlist.currentTrack - currBase > 0
  ) {
    $currTrack.value = state.playlist.currentTrack - currBase;
  }
});

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

  const oldSet = untracked($playlist.get);
  const newSet = new Set(playlist.map((i) => i.id));

  const addedItems = newSet.difference(oldSet);

  // TODO: Can't do removals yet without resetting the webamp instance.
  // const removedItems = oldSet.difference(newSet);

  if (addedItems.size === 0) return;

  playlist.forEach((item, idx) => {
    if (addedItems.has(item.id) === false) return;

    // TODO
    // if (item.stats?.duration == undefined) return;

    // TODO: Inserting at a specific index doesn't work
    ampElement.addTrack(item);
  });

  if (untracked($currTrack.get) === null) {
    amp.setCurrentTrack(past.length);
  }

  $playlist.value = newSet;
});

/**
 * Whenever Webamp's queue changes,
 * reflect the change in our queue too.
 */
effect(() => {
  if (($currTrack.value ?? 0) > untracked(queue.past).length) {
    queue.shift();
  }
});

/** */
const tracksPromise = Promise.withResolvers();

effect(() => {
  const state = output.tracks.state();
  if (state !== "loaded") return;

  const cacheId = search.cacheId();
  if (cacheId === "") return;

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

// TODO:
// amp.onMinimize(() => amp.close());

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

function addBatch() {
  queue.fill({ augment: true, amount: 50, shuffled: true });

  // Automatically insert track if there isn't any
  if (!queue.now()) queue.shift();
}

function windowManager() {
  const w = document.body.querySelector("dtw-window-manager");
  if (w instanceof WindowManager) return w;
  return null;
}
