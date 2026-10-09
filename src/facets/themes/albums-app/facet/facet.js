import foundation from "~/common/foundation.js";

// Set doc title
foundation.setup({ title: "Albums.app" });

////////////////////////////////////////////
// 🚀
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

await import("~/facets/themes/albums-app/browser/element.js");

const groupLabel = foundation.GROUP === "facets" ? "Deck A" : foundation.GROUP;
const browser = document.querySelector("da-browser");

browser?.setAttribute("group", foundation.GROUP);
browser?.setAttribute("group-label", groupLabel);

////////////////////////////////////////////
// SHORTCUTS
////////////////////////////////////////////

// The titlebar's "new deck" button in the browser element bubbles this up
document.addEventListener("da-new-deck", async () => {
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
});

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
