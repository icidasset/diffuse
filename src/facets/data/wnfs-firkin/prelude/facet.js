import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

/**
 * @import OutputOrchestrator from "~/components/orchestrator/output/element.js"
 */

// Tag name is stable; importing the element module here would pull the
// WNFS/WASM chunk on every interface load. Import it when the feature is
// actually wired up instead.
const WNFS_OUTPUT_NAME = "dob-wnfs-firkin";

const OUTPUT_ID = "do-output__dc-output__wnfs-firkin";
const CONNECT_URL = "facets/connect/firkin/index.tile";

/** Namespace of the WNFS userdata output. */
const OUTPUT_NAMESPACE = "wnfs-firkin";

/**
 * Adds the Firkin + WNFS userdata output (which also owns the passkey
 * sign-in).
 *
 * The Firkin input and uploader live in the default input and upload bundles:
 * they read the shared Firkin session from IndexedDB and operate on the
 * account's file registry directly, so they do not depend on this feature or
 * on the WNFS output.
 *
 * This is a feature (prelude) facet, so it only runs when the user enables
 * it.
 */
effect(() => {
  const output = foundation.signals.orchestrator.output();
  if (!output) return;

  wnfsFirkin(output);
  output.loadSelected();
});

/**
 * @param {OutputOrchestrator} output
 */
function wnfsFirkin(output) {
  if (output.root().querySelector(`#${OUTPUT_ID}`)) return;

  // Load (and define) the element before adding it. If the import fails — for
  // example because a stale service-worker cache is serving an old WASM chunk —
  // an undefined `dob-wnfs-firkin` element would otherwise make the output
  // configurator wait on `customElements.whenDefined()` forever.
  import("~/components/output/bytes/wnfs/element.js")
    .then(() => {
      if (output.root().querySelector(`#${OUTPUT_ID}`)) return;

      const storage = document.createElement(WNFS_OUTPUT_NAME);
      storage.setAttribute("group", foundation.GROUP);
      storage.setAttribute("id", OUTPUT_ID);
      storage.setAttribute("label", "Firkin + WNFS");
      storage.setAttribute("namespace", OUTPUT_NAMESPACE);
      storage.setAttribute("connect", CONNECT_URL);

      output.outputConfigurator.append(storage);
      output.loadSelected();
    })
    .catch((err) => {
      console.error("Failed to load the Firkin + WNFS output:", err);
    });
}
