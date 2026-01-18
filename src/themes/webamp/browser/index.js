import defaults from "@common/constituents/default/config.js";
import BrowserElement from "@themes/webamp/browser/element.js";

const el = new BrowserElement();
el.setAttribute("input-selector", defaults.orchestrator.input.selector);
el.setAttribute("output-selector", defaults.orchestrator.output.selector);
el.setAttribute("queue-engine-selector", defaults.engine.queue.selector);
el.setAttribute(
  "search-processor-selector",
  defaults.processor.search.selector,
);

document.querySelector("#placeholder")?.replaceWith(el);
