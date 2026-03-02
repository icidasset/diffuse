import foundation from "@diffuse/foundation";
import InputConfigElement from "@diffuse/themes/webamp/configurators/input/element.js";

const inp = foundation.orchestrator.input();
const out = foundation.orchestrator.output();
const pro = foundation.orchestrator.processTracks({ disableWhenReady: true });
const sou = foundation.orchestrator.sources();

const el = new InputConfigElement();
el.setAttribute("input-selector", inp.selector);
el.setAttribute("output-selector", out.selector);
el.setAttribute("sources-orchestrator-selector", sou.selector);
el.setAttribute("process-tracks-orchestrator-selector", pro.selector);

document.querySelector("#placeholder")?.replaceWith(el);
