import foundation from "@common/constituents/foundation.js";
import InputConfigElement from "@themes/webamp/configurators/input/element.js";

const inp = foundation.orchestrator.input();
const out = foundation.orchestrator.output();
const sou = foundation.orchestrator.sources();

const el = new InputConfigElement();
el.setAttribute("input-selector", inp.selector);
el.setAttribute("output-selector", out.selector);
el.setAttribute("sources-orchestrator-selector", sou.selector);

document.querySelector("#placeholder")?.replaceWith(el);
