import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

/**
 * @import OutputOrchestrator from "~/components/orchestrator/output/element.js"
 */

// Tag names are stable; importing the element modules here would pull the
// Firkin SDK and the local-first sync transformer on every interface load.
// Import them when the feature is actually wired up instead.
const OUTPUT_NAME = "dob-firkin";
const SYNC_NAME = "dtob-dasl-sync";

const OUTPUT_ID = "do-output__dob-firkin";
const SYNC_ID = "do-output__dc-output__firkin";
const CONNECT_URL = "facets/connect/firkin/index.tile";

/** Namespace of the raw Firkin userdata output. */
const OUTPUT_NAMESPACE = "firkin";

/**
 * Adds the Firkin userdata output: raw content-addressed blobs in a Firkin
 * account, with a registry pointer per collection.
 *
 * The generic Firkin output is wrapped in the DASL sync transformer so the five
 * collections stay in a local IndexedDB cache and merge with what's on the
 * server, the same way the S3 and Dropbox outputs work.
 *
 * The Firkin input and uploader live in the default input and upload bundles:
 * they read the shared Firkin session from IndexedDB, so they do not depend on
 * this feature.
 *
 * This is a feature (prelude) facet, so it only runs when the user enables it.
 */
effect(() => {
  const output = foundation.signals.orchestrator.output();
  if (!output) return;

  firkin(output);
  output.loadSelected();
});

/**
 * @param {OutputOrchestrator} output
 */
function firkin(output) {
  if (output.outputConfigurator.querySelector(`#${SYNC_ID}`)) return;

  // Load (and define) the elements before adding them, so the output
  // configurator doesn't wait on `customElements.whenDefined()` for an element
  // whose module failed to import.
  Promise.all([
    import("~/components/output/bytes/firkin/element.js"),
    import("~/components/transformer/output/bytes/dasl-sync/element.js"),
  ])
    .then(() => {
      if (output.outputConfigurator.querySelector(`#${SYNC_ID}`)) return;

      const storage = document.createElement(OUTPUT_NAME);
      storage.setAttribute("group", foundation.GROUP);
      storage.setAttribute("id", OUTPUT_ID);
      storage.setAttribute("namespace", OUTPUT_NAMESPACE);

      const sync = document.createElement(SYNC_NAME);
      sync.setAttribute("group", foundation.GROUP);
      sync.setAttribute("id", SYNC_ID);
      sync.setAttribute("label", "Firkin");
      sync.setAttribute("namespace", OUTPUT_NAMESPACE);
      sync.setAttribute("connect", CONNECT_URL);
      sync.setAttribute("output-selector", `#${OUTPUT_ID}`);

      output.append(storage);
      output.outputConfigurator.append(sync);
      output.loadSelected();
    })
    .catch((err) => {
      console.error("Failed to load the Firkin output:", err);
    });
}
