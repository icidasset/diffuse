import foundation from "~/common/facets/foundation.js";
import BrowserElement from "~/themes/webamp/browser/element.js";

foundation.features.processInputs();
foundation.features.searchThroughCollection();

const out = foundation.orchestrator.output();
const que = foundation.engine.queue();
const scp = foundation.engine.scope();
const trc = foundation.orchestrator.scopedTracks();

const el = new BrowserElement();
el.setAttribute("output-selector", out.selector);
el.setAttribute("queue-engine-selector", que.selector);
el.setAttribute("scope-engine-selector", scp.selector);
el.setAttribute("tracks-selector", trc.selector);

document.querySelector("#placeholder")?.replaceWith(el);
