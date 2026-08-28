import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

/**
 * @import ArtworkController from "~/facets/themes/blur/artwork-controller/element.js"
 */

// Set doc title
foundation.setup({ title: "Blur Pocket | Diffuse" });

////////////////////////////////////////////
// 🚀 Foundation
////////////////////////////////////////////

await foundation.engine.queue();
await foundation.engine.repeatShuffle();
await foundation.engine.scope();
await foundation.orchestrator.scopedTracks();

await foundation.orchestrator.sources();
await foundation.orchestrator.processTracks({ disableWhenReady: true });
await foundation.orchestrator.queueAudio();
await foundation.orchestrator.controller();
await foundation.orchestrator.mediaSession();
await foundation.orchestrator.artwork();
await foundation.orchestrator.coverGroups();
await foundation.orchestrator.favourites();
await foundation.configurator.input();

await import("~/facets/themes/blur/artwork-controller/element.js");
await import("~/facets/themes/blur/browser/element.js");

const groupLabel = foundation.GROUP === "facets" ? "Deck A" : foundation.GROUP;

const artworkController = document.querySelector("db-artwork-controller");
const browser = document.querySelector("db-browser");

artworkController?.setAttribute("group", foundation.GROUP);
artworkController?.setAttribute("group-label", groupLabel);
browser?.setAttribute("group", foundation.GROUP);

////////////////////////////////////////////
// Now Playing sheet
////////////////////////////////////////////

const sheet = /** @type {HTMLElement | null} */ (
  document.querySelector("#now-playing")
);
const miniPlayer = /** @type {HTMLElement | null} */ (
  document.querySelector("#mini-player")
);
const miniExpand = document.querySelector("#mini-expand");
const npGrab = /** @type {HTMLElement | null} */ (
  document.querySelector("#np-grab")
);

function openSheet() {
  sheet?.classList.add("is-open");
}

function closeSheet() {
  sheet?.classList.remove("is-open");
}

miniExpand?.addEventListener("click", openSheet);

// Drag the grab handle down to dismiss
{
  let startY = 0;
  let currentY = 0;
  let dragging = false;

  npGrab?.addEventListener("pointerdown", (e) => {
    if (!sheet || !(e instanceof PointerEvent)) return;
    dragging = true;
    startY = e.clientY;
    currentY = 0;
    sheet.style.transition = "none";
    npGrab.setPointerCapture(e.pointerId);
  });

  npGrab?.addEventListener("pointermove", (e) => {
    if (!dragging || !sheet || !(e instanceof PointerEvent)) return;
    currentY = Math.max(0, e.clientY - startY);
    sheet.style.transform = `translateY(${currentY}px)`;
  });

  const endDrag = () => {
    if (!dragging || !sheet) return;
    dragging = false;
    sheet.style.transition = "";
    sheet.style.transform = "";
    if (currentY > 120) closeSheet();
    currentY = 0;
  };

  npGrab?.addEventListener("pointerup", endDrag);
  npGrab?.addEventListener("pointercancel", endDrag);
}

// Escape closes the sheet
document.addEventListener("keydown", (e) => {
  if (e.key === "Escape" && sheet?.classList.contains("is-open")) {
    closeSheet();
  }
});

////////////////////////////////////////////
// Mini-player
////////////////////////////////////////////

const miniTitle = document.querySelector("#mini-title");
const miniArtist = document.querySelector("#mini-artist");
const miniArt = document.querySelector("#mini-art");
const miniPlayIcon = document.querySelector("#mini-play-icon");
const miniPlayPause = document.querySelector("#mini-play-pause");
const miniNext = document.querySelector("#mini-next");

let miniArtUrl = "";
let miniArtObjectUrl = "";
// Monotonic generation token. Incremented on every effect run so that an async
// `art.get()` resolution can tell whether it is still the latest run. Stale
// resolutions (superseded by a newer run before the artwork bytes arrived) bail
// out without creating or revoking object URLs, preventing blob URLs from
// accumulating when the mini-player updates rapidly (e.g. per progress tick or
// across track switches).
let miniArtGen = 0;

// Split the mini-player into focused effects, each reacting only to the signals
// it reads. Nothing here needs per-tick progress, so — unlike a single blanket
// effect — these do not re-run on every playback tick.

// Visibility + track metadata (title / artist). Runs only when the current
// track changes, not on playback progress.
effect(() => {
  const controller = foundation.signals.orchestrator.controller();
  const track = controller?.currentTrack();

  if (track) {
    miniPlayer?.removeAttribute("hidden");
  } else {
    miniPlayer?.setAttribute("hidden", "");
  }

  if (miniTitle) {
    miniTitle.textContent = track?.tags?.title ?? "Diffuse";
  }
  if (miniArtist) {
    miniArtist.textContent = track?.tags?.artist ??
      (track ? "" : "Waiting on queue …");
  }
});

// Play / pause icon. Reacts only to the play state signal.
effect(() => {
  const controller = foundation.signals.orchestrator.controller();
  const playing = controller?.isPlaying() ?? false;

  if (miniPlayIcon) {
    miniPlayIcon.className = playing
      ? "ph-fill ph-pause"
      : "ph-fill ph-play";
  }
});

// Artwork thumbnail — pull from the artwork orchestrator. Bump the generation
// before firing an async fetch; if a newer run supersedes this one before the
// bytes resolve, the stale callback will be ignored entirely.
effect(() => {
  const controller = foundation.signals.orchestrator.controller();
  const track = controller?.currentTrack();
  const gen = ++miniArtGen;

  if (!track) {
    showNoArtwork();
    return;
  }

  const art = foundation.signals.orchestrator.artwork();
  art?.get(track).then((bytes) => {
    // A newer effect run has taken over — drop this result without creating
    // or revoking any object URL.
    if (gen !== miniArtGen) return;

    // No cover for this track — fall back to the placeholder icon and hide
    // any <img> that was shown for the previous one.
    if (!bytes) {
      showNoArtwork();
      return;
    }
    // Avoid recreating the same blob URL on every reactive run.
    const key = `${track.id}:${bytes.byteLength}`;
    if (key === miniArtUrl) return;
    miniArtUrl = key;

    const mime = detectMime(bytes);
    const url = URL.createObjectURL(
      new Blob([/** @type {ArrayBuffer} */ (bytes.buffer)], { type: mime }),
    );

    // Revoke the previous artwork blob URL before replacing it so its bytes
    // are released. Otherwise each stale resolution leaks one blob URL,
    // growing memory until iOS Safari crashes after playing tracks for a
    // while.
    if (miniArtObjectUrl) {
      URL.revokeObjectURL(miniArtObjectUrl);
    }
    miniArtObjectUrl = url;

    if (miniArt) {
      miniArt.innerHTML = "";
      const img = document.createElement("img");
      // If the image fails to load, drop it and show the placeholder icon.
      img.addEventListener("error", showNoArtwork);
      img.alt = "";
      img.src = url;
      miniArt.append(img);
    }
  }).catch(() => {
    if (gen === miniArtGen) showNoArtwork();
  });
});

/** @type {ArtworkController | null} */
const artworkControllerEl = /** @type {ArtworkController | null} */ (
  artworkController
);

// Wire mini-player buttons to the artwork-controller's public methods.
miniPlayPause?.addEventListener("click", (e) => {
  e.stopPropagation();
  artworkControllerEl?.playPause();
});

miniNext?.addEventListener("click", (e) => {
  e.stopPropagation();
  artworkControllerEl?.next();
});

////////////////////////////////////////////
// Shortcuts
////////////////////////////////////////////

document.querySelector("#btn-new-deck")?.addEventListener("click", async () => {
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
  globalThis.open(url.toString(), "_blank");
});

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();

////////////////////////////////////////////
// 🛠️ HELPERS
////////////////////////////////////////////

/**
 * Reset the minimized now-playing artwork back to the placeholder icon,
 * hiding any <img> that may be (or failed to be) displayed.
 */
function showNoArtwork() {
  miniArtUrl = "";
  if (miniArtObjectUrl) {
    URL.revokeObjectURL(miniArtObjectUrl);
    miniArtObjectUrl = "";
  }
  if (miniArt) {
    miniArt.innerHTML = `<i class="ph-fill ph-music-notes"></i>`;
  }
}

/**
 * @param {Uint8Array} bytes
 * @returns {string}
 */
function detectMime(bytes) {
  if (bytes[0] === 0xFF && bytes[1] === 0xD8) return "image/jpeg";
  if (bytes[0] === 0x89 && bytes[1] === 0x50) return "image/png";
  if (bytes[0] === 0x47 && bytes[1] === 0x49) return "image/gif";
  if (bytes[0] === 0x52 && bytes[1] === 0x49) return "image/webp";
  return "image/jpeg";
}
