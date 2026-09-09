import foundation from "~/common/foundation.js";
import PlayerElement from "~/facets/themes/tidal/player/element.js";

foundation.setup({ title: "Player | Tidal | Diffuse" });

// Setup the prerequisite elements
await foundation.orchestrator.queueAudio();
await foundation.orchestrator.mediaSession();

const [out, que, rs, art, fav, ctrl] = await Promise.all([
  foundation.orchestrator.output(),
  foundation.engine.queue(),
  foundation.engine.repeatShuffle(),
  foundation.orchestrator.artwork(),
  foundation.orchestrator.favourites(),
  foundation.orchestrator.controller(),
]);

const el = new PlayerElement();
el.setAttribute("artwork-selector", art.selector);
el.setAttribute("controller-orchestrator-selector", ctrl.selector);
el.setAttribute("favourites-orchestrator-selector", fav.selector);
el.setAttribute("queue-engine-selector", que.selector);
el.setAttribute("repeat-shuffle-engine-selector", rs.selector);
el.setAttribute("output-selector", out.selector);

(document.querySelector("#container") ?? document.body).append(el);

foundation.ready();
