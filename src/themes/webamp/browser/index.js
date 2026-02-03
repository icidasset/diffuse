import defaults from "@common/constituents/default/config.js";
import BrowserElement from "@themes/webamp/browser/element.js";

const search = defaults.lazy.processor.search();
defaults.lazy.orchestrator.searchTracks();

const el = new BrowserElement();
el.setAttribute(
  "input-selector",
  defaults.instantiated.orchestrator.input.selector,
);
el.setAttribute(
  "output-selector",
  defaults.instantiated.orchestrator.output.selector,
);
el.setAttribute(
  "queue-engine-selector",
  defaults.instantiated.engine.queue.selector,
);
el.setAttribute("search-processor-selector", search.selector);

document.querySelector("#placeholder")?.replaceWith(el);
