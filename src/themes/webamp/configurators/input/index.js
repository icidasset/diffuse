import defaults from "@common/constituents/default/config.js";
import InputConfigElement from "@themes/webamp/configurators/input/element.js";

const el = new InputConfigElement();
el.setAttribute("input-selector", defaults.orchestrator.input.selector);
el.setAttribute("output-selector", defaults.orchestrator.output.selector);
el.setAttribute(
  "sources-orchestrator-selector",
  defaults.orchestrator.sources.selector,
);

document.querySelector("#placeholder")?.replaceWith(el);
