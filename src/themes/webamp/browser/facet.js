import foundation from "@common/facets/foundation.js";
import BrowserElement from "@themes/webamp/browser/element.js";

foundation.features.fillQueueAutomatically();
foundation.features.processInputs();
foundation.features.searchThroughCollection();

const inp = foundation.orchestrator.input();
const out = foundation.orchestrator.output();
const que = foundation.engine.queue();
const sea = foundation.processor.search();

const el = new BrowserElement();
el.setAttribute("input-selector", inp.selector);
el.setAttribute("output-selector", out.selector);
el.setAttribute("queue-engine-selector", que.selector);
el.setAttribute("search-processor-selector", sea.selector);

document.querySelector("#placeholder")?.replaceWith(el);
