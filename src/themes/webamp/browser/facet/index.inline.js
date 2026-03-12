import foundation from "~/common/facets/foundation.js";
import BrowserElement from "~/themes/webamp/browser/element.js";

await foundation.features.processInputs();
await foundation.features.searchThroughCollection();

const [out, que, scp, trc] = await Promise.all([
  foundation.orchestrator.output(),
  foundation.engine.queue(),
  foundation.engine.scope(),
  foundation.orchestrator.scopedTracks(),
]);

const el = new BrowserElement();
el.setAttribute("output-selector", out.selector);
el.setAttribute("queue-engine-selector", que.selector);
el.setAttribute("scope-engine-selector", scp.selector);
el.setAttribute("tracks-selector", trc.selector);

document.querySelector("#placeholder")?.replaceWith(el);
