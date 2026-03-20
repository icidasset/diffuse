import foundation from "~/common/foundation.js";
import InputConfigElement from "~/themes/winamp/configurators/input/element.js";

// Set doc title
document.title = "Input | Winamp | Diffuse";

const [inp, out, pro, sou] = await Promise.all([
  foundation.configurator.input(),
  foundation.orchestrator.output(),
  foundation.orchestrator.processTracks({ disableWhenReady: true }),
  foundation.orchestrator.sources(),
]);

const el = new InputConfigElement();
el.setAttribute("input-selector", inp.selector);
el.setAttribute("output-selector", out.selector);
el.setAttribute("sources-orchestrator-selector", sou.selector);
el.setAttribute("process-tracks-orchestrator-selector", pro.selector);

document.querySelector("#placeholder")?.replaceWith(el);
