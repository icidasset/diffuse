import defaults from "@common/constituents/default/config.js";
import BrowserElement from "@themes/webamp/browser/element.js";

const queue = defaults.lazy.engine.queue();
const search = defaults.lazy.processor.search();

defaults.lazy.orchestrator.queueTracks();
defaults.lazy.orchestrator.searchTracks();

const el = new BrowserElement();
el.setAttribute("input-selector", defaults.orchestrator.input.selector);
el.setAttribute("output-selector", defaults.orchestrator.output.selector);
el.setAttribute("queue-engine-selector", queue.selector);
el.setAttribute("search-processor-selector", search.selector);

document.querySelector("#placeholder")?.replaceWith(el);
