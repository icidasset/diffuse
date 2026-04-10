import foundation from "~/common/foundation.js";
import BrowserElement from "~/themes/blur/browser/element.js";

// Set doc title
foundation.setup({ title: "Browser | Blur | Diffuse" });

const [out, que, scp, trc, fav] = await Promise.all([
  foundation.orchestrator.output(),
  foundation.engine.queue(),
  foundation.engine.scope(),
  foundation.orchestrator.scopedTracks(),
  foundation.orchestrator.favourites(),
]);

// Default to grouping by date added
// TODO: Remove
if (!scp.groupBy()) scp.setGroupBy("createdAt");

const el = new BrowserElement();
el.setAttribute("output-selector", out.selector);
el.setAttribute("queue-engine-selector", que.selector);
el.setAttribute("scope-engine-selector", scp.selector);
el.setAttribute("tracks-selector", trc.selector);
el.setAttribute("favourites-orchestrator-selector", fav.selector);

(document.querySelector("#container") ?? document.body).append(el);

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
