import foundation from "~/common/facets/foundation.js";
import OutputConfigElement from "~/themes/webamp/configurators/output/element.js";

const out = foundation.orchestrator.output();

const el = new OutputConfigElement();
el.setAttribute("output-selector", out.selector);

document.querySelector("#placeholder")?.replaceWith(el);
