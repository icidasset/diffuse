import "@components/orchestrator/process-tracks/element.js";
import "@components/orchestrator/queue-tracks/element.js";
import "@components/output/polymorphic/indexed-db/element.js";
import "@components/processor/metadata/element.js";

import * as Input from "@components/input/opensubsonic/element.js";
import * as Queue from "@components/engine/queue/element.js";

import { component } from "@common/element.js";
import { effect, signal, untracked } from "@common/signal.js";

import "./browser/element.js";
import "./window/element.js";
import "./window-manager/element.js";
import WebampElement from "./webamp/element.js";

const input = component(Input);
const queue = component(Queue);

globalThis.queue = queue;

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
    if (item.stats?.duration == undefined) return;

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

////////////////////////////////////////////
// DESKTOP
////////////////////////////////////////////

// Open associated window when click desktop items
document.body.querySelectorAll(".desktop__item").forEach((element) => {
  if (element instanceof HTMLElement) {
    element.addEventListener("dblclick", () => {
      const f = element.querySelector("label")?.getAttribute("for");
      if (f) {
        document.body.querySelector(`dtw-window#${f}`)?.toggleAttribute("open");
      }
    });
  }
});

// Toggle Winamp if click that desktop item
let winampIsShown = true;

document.body.querySelector("#desktop-winamp")?.addEventListener(
  "dblclick",
  () => {
    if (winampIsShown) amp.close();
    else {
      amp.reopen();
      winampIsShown = true;
    }
  },
);

amp.onClose(() => winampIsShown = false);

// TODO:
// amp.onMinimize(() => amp.close());
