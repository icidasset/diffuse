import foundation from "~/common/facets/foundation.js";
import InputConfigElement from "~/themes/webamp/configurators/input/element.js";

const [inp, out, pro, sou] = await Promise.all([
  foundation.orchestrator.input(),
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
