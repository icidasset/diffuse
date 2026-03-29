import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

import WindowManager from "~/themes/winamp/window-manager/element.js";

// Set doc title
foundation.setup({ title: "Winamp | Diffuse" });

// Doc loader
const main = /** @type {HTMLElement} */ (document.querySelector("main"));
main.classList.add("has-loaded");

/**
 * @import {OutputElement} from "~/components/output/types.d.ts"
 */

const queue = await foundation.engine.queue();
const repeatShuffle = await foundation.engine.repeatShuffle();
const scopedTracks = await foundation.orchestrator.scopedTracks();

await foundation.orchestrator.sources();
await foundation.orchestrator.processTracks({ disableWhenReady: true });
await foundation.orchestrator.queueAudio();
await foundation.orchestrator.controller();

await import("~/themes/winamp/browser/element.js");
await import("~/themes/winamp/window/element.js");

const { default: WinampElement } = await import(
  "~/themes/winamp/winamp/element.js"
);

/** @type {OutputElement | null} */
const output = document.querySelector("#output");
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
