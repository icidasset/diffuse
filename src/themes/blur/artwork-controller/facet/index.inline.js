import foundation from "~/common/foundation.js";
import ArtworkController from "~/themes/blur/artwork-controller/element.js";

// Set doc title
foundation.setup({ title: "Artwork controller | Blur | Diffuse" });

// Setup the prerequisite elements
await foundation.orchestrator.queueAudio();
await foundation.orchestrator.mediaSession();

const [art, ctl, fav, inp] = await Promise.all([
  foundation.orchestrator.artwork(),
  foundation.orchestrator.controller(),
  foundation.orchestrator.favourites(),
  foundation.configurator.input(),
]);

// Controller
const dac = new ArtworkController();
dac.setAttribute("artwork-selector", art.selector);
dac.setAttribute("controller-orchestrator-selector", ctl.selector);
dac.setAttribute("input-selector", inp.selector);
dac.setAttribute("favourites-orchestrator-selector", fav.selector);

// Add to DOM
(document.querySelector("#container") ?? document.body).append(dac);

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
