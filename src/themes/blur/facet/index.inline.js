import foundation from "~/common/foundation.js";

// Set doc title
foundation.setup({ title: "Blur | Diffuse" });

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
await foundation.orchestrator.artwork();
await foundation.orchestrator.favourites();

await import("~/themes/blur/artwork-controller/element.js");
await import("~/themes/blur/browser/element.js");

const group = foundation.GROUP === "facets" ? "Deck A" : foundation.GROUP;
document.querySelector("db-artwork-controller")?.setAttribute("group", group);
document.querySelector("db-browser")?.setAttribute("group", group);

////////////////////////////////////////////
// SHORTCUTS
////////////////////////////////////////////

document.querySelector("#btn-new-deck")?.addEventListener("click", async () => {
  const state = await navigator.locks.query();
  const held = (state.held ?? []).map((l) => l.name);

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

foundation.ready();
