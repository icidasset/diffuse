import foundation from "~/common/foundation.js";
import BrowserElement from "~/themes/blur/browser/element.js";

// Set doc title
foundation.setup({ title: "Browser | Blur | Diffuse" });

const [out, que, scp, art, cov, fav, trc] = await Promise.all([
  foundation.orchestrator.output(),
  foundation.engine.queue(),
  foundation.engine.scope(),
  foundation.orchestrator.artwork(),
  foundation.orchestrator.coverGroups(),
  foundation.orchestrator.favourites(),
  foundation.orchestrator.scopedTracks(),
]);

// Default to grouping by date added
// TODO: Remove
if (!scp.groupBy()) scp.setGroupBy("createdAt");

const el = new BrowserElement();
el.setAttribute("artwork-selector", art.selector);
el.setAttribute("cover-groups-orchestrator-selector", cov.selector);
el.setAttribute("favourites-orchestrator-selector", fav.selector);
el.setAttribute("output-selector", out.selector);
el.setAttribute("queue-engine-selector", que.selector);
el.setAttribute("scope-engine-selector", scp.selector);
el.setAttribute("tracks-selector", trc.selector);

(document.querySelector("#container") ?? document.body).append(el);

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
