import defaults from "@common/constituents/default/config.js";
import InputConfigElement from "@themes/webamp/configurators/input/element.js";

const sources = defaults.lazy.orchestrator.sources();

const el = new InputConfigElement();
el.setAttribute("input-selector", defaults.orchestrator.input.selector);
el.setAttribute("output-selector", defaults.orchestrator.output.selector);
el.setAttribute("sources-orchestrator-selector", sources.selector);

document.querySelector("#placeholder")?.replaceWith(el);
