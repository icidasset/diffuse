import foundation from "~/common/foundation.js";
import ArtworkController from "~/themes/blur/artwork-controller/element.js";

// Set doc title
foundation.setup({ title: "Artwork controller | Blur | Diffuse" });

// Setup the prerequisite elements
await foundation.orchestrator.queueAudio();
await foundation.orchestrator.mediaSession();

const [aud, art, fav, inp, out, que] = await Promise.all([
  foundation.engine.audio(),
  foundation.orchestrator.artwork(),
  foundation.orchestrator.favourites(),
  foundation.configurator.input(),
  foundation.orchestrator.output(),
  foundation.engine.queue(),
]);

// Controller
const dac = new ArtworkController();
dac.setAttribute("artwork-selector", art.selector);
dac.setAttribute("audio-engine-selector", aud.selector);
dac.setAttribute("input-selector", inp.selector);
dac.setAttribute("output-selector", out.selector);
dac.setAttribute("queue-engine-selector", que.selector);
dac.setAttribute("favourites-orchestrator-selector", fav.selector);

// Add to DOM
(document.querySelector("#container") ?? document.body).append(dac);
