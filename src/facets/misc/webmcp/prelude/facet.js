import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";
import { WEBMCP_FACET_URI, registerDiffuseTools } from "~/common/webmcp.js";

/**
 * Registers the Diffuse tools with the browser's agent via WebMCP. The prelude
 * runs on every page that inserts preludes — facet pages (`l/`), and the
 * dashboard, featured and editor pages (via `common/pages/output.js`).
 */
const controller = new AbortController();
await registerDiffuseTools({ signal: controller.signal });

/**
 * Unregister the tools when this facet is disabled or removed from the
 * collection. The spec's `registerTool` supports unregistration through the
 * signal; browsers that don't support it yet need a reload. Facet pages reload
 * on prelude changes by themselves; other pages (dashboard, featured, editor)
 * rely on this effect.
 */
const out = await foundation.orchestrator.output();

effect(() => {
  const col = out.facets.collection();
  if (col.state !== "loaded") return;

  const facet = col.data.find((f) => f.uri === WEBMCP_FACET_URI);
  if (!facet || facet.enabled === false) {
    controller.abort();
  }
});
