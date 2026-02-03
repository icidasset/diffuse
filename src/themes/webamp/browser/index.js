import foundation from "@common/constituents/foundation.js";
import BrowserElement from "@themes/webamp/browser/element.js";

const que = foundation.assemblage.queueManagement();
const sea = foundation.assemblage.searchThroughCollection();

const el = new BrowserElement();
el.setAttribute("input-selector", que.orchestrator.input.selector);
el.setAttribute("output-selector", que.orchestrator.output.selector);
el.setAttribute("queue-engine-selector", que.engine.queue.selector);
el.setAttribute("search-processor-selector", sea.processor.search.selector);

document.querySelector("#placeholder")?.replaceWith(el);
