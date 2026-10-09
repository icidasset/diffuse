import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

import WindowManager from "~/facets/themes/winamp/window-manager/element.js";

// Set doc title
foundation.setup({ title: "Winamp | Diffuse" });

/**
 * @import {OutputElement} from "@specs/components/output/types.d.ts"
 */

const queue = await foundation.engine.queue();
const repeatShuffle = await foundation.engine.repeatShuffle();
const scopedTracks = await foundation.orchestrator.scopedTracks();

await foundation.orchestrator.sources();
await foundation.orchestrator.processTracks({ disableWhenReady: true });
await foundation.orchestrator.queueAudio();
await foundation.orchestrator.controller();
await foundation.orchestrator.artwork();
await foundation.orchestrator.mediaSession();
await foundation.orchestrator.favourites();
await foundation.configurator.input();

await import("~/facets/themes/winamp/browser/element.js");
await import("~/facets/themes/winamp/window/element.js");
await import("~/facets/themes/winamp/artwork/element.js");
await import("~/facets/themes/winamp/track-details/element.js");

const { default: WinampElement } = await import(
  "~/facets/themes/winamp/winamp/element.js"
);

/** @type {OutputElement | null} */
const output = document.querySelector("do-output");
if (!output) throw new Error("Missing output element");

globalThis.queue = queue;
globalThis.output = output;

////////////////////////////////////////////
// DESKTOP
////////////////////////////////////////////

// Open associated window when click desktop items
document.body.querySelectorAll(".desktop__item").forEach((element) => {
  if (element instanceof HTMLElement) {
    element.addEventListener("dblclick", () => {
      if (element.id === "desktop-winamp") {
        const w = document.body.querySelector("dtw-winamp");
        if (w instanceof WinampElement) w.open();
        return;
      }
      const f = element.querySelector("label")?.getAttribute("for");
      if (f) return windowManager()?.toggleWindow(f);
    });
  }
});

/**
 * Keep note of when search is ready.
 */
const tracksPromise = Promise.withResolvers();

effect(() => {
  const col = output.tracks.collection();
  if (col.state !== "loaded") return;

  const fingerprintSearch = scopedTracks.supplyFingerprint();
  if (fingerprintSearch === undefined) return;

  const fingerprintQueue = queue.supplyFingerprint();
  if (fingerprintQueue === undefined) return;

  tracksPromise.resolve("loaded");
});

// Add batch
document.body.querySelector("#desktop-batch")?.addEventListener(
  "dblclick",
  () => {
    if (!queue.supplyFingerprint()) {
      queue.supply({ trackIds: scopedTracks.tracks().map((t) => t.id) });
    }

    tracksPromise.promise.then(() => {
      addBatch();
    });
  },
);

// Open a new window with a fresh instance group, i.e. an independent player
// with its own queue and playback state (shared collection and sources).
// Live instances reveal their group through the locks held by their
// components (same trick and naming as the Blur themes).
document.body.querySelector("#desktop-new-instance")?.addEventListener(
  "dblclick",
  async () => {
    const state = await navigator.locks.query();
    const held = (state.held ?? []).flatMap((l) => l.name ? [l.name] : []);

    let nextGroup;

    if (!held.some((n) => n.includes("/Deck B"))) {
      nextGroup = "Deck B";
    } else if (!held.some((n) => n.includes("/Deck C"))) {
      nextGroup = "Deck C";
    } else {
      return;
    }

    const url = new URL(document.location.href);
    url.searchParams.set("group", nextGroup);
    window.open(url.toString(), "_blank");
  },
);

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

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
