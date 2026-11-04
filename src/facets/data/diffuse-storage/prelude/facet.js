import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

/**
 * @import OutputOrchestrator from "~/components/orchestrator/output/element.js"
 */

// Tag name is stable; importing the element module here would pull the
// WNFS/WASM chunk on every interface load. Import it when the feature is
// actually wired up instead.
const WNFS_OUTPUT_NAME = "dob-diffuse-storage";

const OUTPUT_ID = "do-output__dc-output__diffuse-storage";
const CONNECT_URL = "facets/connect/diffuse-storage/index.tile";

/**
 * Adds the "Diffuse storage" output to the output configurator.
 *
 * This is a feature (prelude) facet, so it only runs when the user enables
 * it. The WNFS output is the storage itself: each collection is stored as a
 * self-describing file in the private WNFS.
 */
effect(() => {
  const output = foundation.signals.orchestrator.output();
  if (!output) return;

  diffuseStorage(output);
  output.loadSelected();
});

/**
 * @param {OutputOrchestrator} output
 */
function diffuseStorage(output) {
  if (output.root().querySelector(`#${OUTPUT_ID}`)) return;

  // Load (and define) the element before adding it. If the import fails — for
  // example because a stale service-worker cache is serving an old WASM chunk —
  // an undefined `dob-diffuse-storage` element would otherwise make the output
  // configurator wait on `customElements.whenDefined()` forever.
  import("~/components/output/bytes/wnfs/element.js")
    .then(() => {
      if (output.root().querySelector(`#${OUTPUT_ID}`)) return;

      const storage = document.createElement(WNFS_OUTPUT_NAME);
      storage.setAttribute("group", foundation.GROUP);
      storage.setAttribute("id", OUTPUT_ID);
      storage.setAttribute("label", "Diffuse storage");
      storage.setAttribute("namespace", "diffuse-storage");
      storage.setAttribute("connect", CONNECT_URL);

      output.outputConfigurator.append(storage);
      output.loadSelected();
    })
    .catch((err) => {
      console.error("Failed to load the Diffuse storage output:", err);
    });
}
