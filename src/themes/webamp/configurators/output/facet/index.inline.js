import foundation from "~/common/foundation.js";
import OutputConfigElement from "~/themes/webamp/configurators/output/element.js";

const out = await foundation.orchestrator.output();

const el = new OutputConfigElement();
el.setAttribute("output-selector", out.selector);

document.querySelector("#placeholder")?.replaceWith(el);
