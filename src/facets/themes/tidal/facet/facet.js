import foundation from "~/common/foundation.js";

foundation.setup({ title: "Tidal | Diffuse" });

const [
  out,
  que,
  scp,
  art,
  cov,
  fav,
  trc,
  rs,
  audio,
  ctrl,
] = await Promise.all([
  foundation.orchestrator.output(),
  foundation.engine.queue(),
  foundation.engine.scope(),
  foundation.orchestrator.artwork(),
  foundation.orchestrator.coverGroups(),
  foundation.orchestrator.favourites(),
  foundation.orchestrator.scopedTracks(),
  foundation.engine.repeatShuffle(),
  foundation.engine.audio(),
  foundation.orchestrator.controller(),
]);

await foundation.configurator.input();
await foundation.orchestrator.sources();
await foundation.orchestrator.processTracks({ disableWhenReady: true });

await foundation.orchestrator.queueAudio();
await foundation.orchestrator.mediaSession();

await import("~/facets/themes/tidal/browser/element.js");
await import("~/facets/themes/tidal/player/element.js");

// Browser
const browser = document.querySelector("db-tidal-browser");
if (browser) {
  browser.setAttribute("artwork-selector", art.selector);
  browser.setAttribute("cover-groups-orchestrator-selector", cov.selector);
  browser.setAttribute("favourites-orchestrator-selector", fav.selector);
  browser.setAttribute("output-selector", out.selector);
  browser.setAttribute("queue-engine-selector", que.selector);
  browser.setAttribute("scope-engine-selector", scp.selector);
  browser.setAttribute("tracks-selector", trc.selector);
}

// Player
const player = document.querySelector("db-tidal-player");
if (player) {
  player.setAttribute("artwork-selector", art.selector);
  player.setAttribute("controller-orchestrator-selector", ctrl.selector);
  player.setAttribute("favourites-orchestrator-selector", fav.selector);
  player.setAttribute("queue-engine-selector", que.selector);
  player.setAttribute("repeat-shuffle-engine-selector", rs.selector);
  player.setAttribute("output-selector", out.selector);
}

foundation.ready();
