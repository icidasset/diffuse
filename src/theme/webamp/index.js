import deepDiff from "@fry69/deep-diff";

// import "@component/orchestrator/process-tracks/element.js";
import "@component/orchestrator/queue-tracks/element.js";
import "@component/output/indexed-db/element.js";
import "@component/processor/metadata/element.js";

import * as Input from "@component/input/opensubsonic/element.js";
import * as Queue from "@component/engine/queue/element.js";

import { component } from "@common/element.js";
import { effect, signal, untracked } from "@common/signal.js";

import "./browser/element.js";
import "./window/element.js";
import "./window-manager/element.js";
import WebampElement from "./webamp.js";
import { xxh32 } from "xxh32";

/**
 * @import {URLTrack} from "webamp"
 *
 * @import {Item} from "@component/engine/queue/types.d.ts"
 */

const input = component(Input);
const queue = component(Queue);

globalThis.queue = queue;

////////////////////////////////////////////
// 📡
////////////////////////////////////////////

let currBase = 0;

const $currTrack = signal(/** @type {null | number} */ (null));
const $playlist = signal(/** @type {Item[]} */ ([]));

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
  if (state.playlist.currentTrack !== null) {
    $currTrack.value = state.playlist.currentTrack;
  }
});

/**
 * Whenever the queue changes update the playlist.
 */
effect(() => {
  const now = queue.now();
  const past = untracked(queue.past);
  const future = queue.future();

  const playlist = [
    ...past,
    ...(now ? [now] : []),
    ...future,
  ];

  const hashNew = xxh32(JSON.stringify(playlist.map((i) => i.id)));
  const hashOld = xxh32(
    JSON.stringify(untracked($playlist.get).map((i) => i.id)),
  );

  console.log(hashNew, hashOld);
  if (hashNew === hashOld) return;

  const webampTracks = playlist.map((item) => {
    /** @type {URLTrack} */
    const urlTrack = {
      url: item.uri,
      metaData: {
        title: item.tags?.title || "",
        artist: item.tags?.artist || "",
        album: item.tags?.album,
      },
      duration: item.stats?.duration,
    };

    return urlTrack;
  });

  currBase = untracked($playlist.get).length;

  amp.setTracksToPlay([]);
  amp.appendTracks(webampTracks);

  console.log("SET CURR", currBase + past.length);
  amp.setCurrentTrack(currBase + past.length);

  $playlist.value = playlist;
});

/**
 * Whenever Webamp's queue changes,
 * reflect the change in our queue too.
 */
effect(() => {
  console.log("CURR", $currTrack.value);

  // if (($currTrack.value ?? 0) > untracked(queue.past).length) {
  //   queue.shift();
  // }
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
