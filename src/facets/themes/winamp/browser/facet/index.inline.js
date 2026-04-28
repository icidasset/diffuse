import foundation from "~/common/foundation.js";
import BrowserElement from "~/facets/themes/winamp/browser/element.js";

// Set doc title
foundation.setup({ title: "Browser | Winamp | Diffuse" });

const [out, que, scp, trc, inp] = await Promise.all([
  foundation.orchestrator.output(),
  foundation.engine.queue(),
  foundation.engine.scope(),
  foundation.orchestrator.scopedTracks(),
  foundation.configurator.input(),
]);

const el = new BrowserElement();
el.setAttribute("output-selector", out.selector);
el.setAttribute("queue-engine-selector", que.selector);
el.setAttribute("scope-engine-selector", scp.selector);
el.setAttribute("tracks-selector", trc.selector);
el.setAttribute("input-selector", inp.selector);

document.querySelector("#placeholder")?.replaceWith(el);

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
