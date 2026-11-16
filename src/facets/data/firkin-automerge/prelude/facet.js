import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

/**
 * @import OutputOrchestrator from "~/components/orchestrator/output/element.js"
 */

// Tag names are stable; importing the element modules here would pull the
// Firkin SDK and the Automerge WASM on every interface load. Import them when
// the feature is actually wired up instead.
const OUTPUT_NAME = "dob-firkin";
const TRANSFORMER_NAME = "dtob-automerge";

const OUTPUT_ID = "do-output__dob-firkin";
const TRANSFORMER_ID = "do-output__dc-output__firkin-automerge";
const CONNECT_URL = "facets/connect/firkin/index.tile";

/** Namespace of the logs and for the local cache. */
const NAMESPACE = "firkin-automerge";

/**
 * Entries kept per device. Must exceed the transformer's snapshot cadence, so a
 * window always contains a whole document to replay from.
 */
const KEEP = 64;

/**
 * Adds the Firkin + Automerge userdata output.
 *
 * The Firkin element is a generic append-only log: every save appends only the
 * changes since the last push, so edits sync as a few hundred bytes instead of
 * a whole collection. The Automerge transformer above it turns that log into
 * the five collections and merges concurrent edits per record.
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

  firkinAutomerge(output);
  output.loadSelected();
});

/**
 * @param {OutputOrchestrator} output
 */
function firkinAutomerge(output) {
  if (output.outputConfigurator.querySelector(`#${TRANSFORMER_ID}`)) return;

  // Load (and define) the elements before adding them, so the output
  // configurator doesn't wait on `customElements.whenDefined()` for an element
  // whose module failed to import.
  Promise.all([
    import("~/components/output/bytes/firkin/element.js"),
    import("~/components/transformer/output/bytes/automerge/element.js"),
  ])
    .then(() => {
      if (output.outputConfigurator.querySelector(`#${TRANSFORMER_ID}`)) return;

      const storage = document.createElement(OUTPUT_NAME);
      storage.setAttribute("group", foundation.GROUP);
      storage.setAttribute("id", OUTPUT_ID);
      storage.setAttribute("namespace", NAMESPACE);
      // The transformer appends deltas and replays the whole log, so the
      // element has to hand back every entry (not just the newest).
      storage.setAttribute("mode", "log");
      storage.setAttribute("keep", String(KEEP));

      const transformer = document.createElement(TRANSFORMER_NAME);
      transformer.setAttribute("group", foundation.GROUP);
      transformer.setAttribute("id", TRANSFORMER_ID);
      transformer.setAttribute("label", "Firkin (Automerge)");
      transformer.setAttribute("namespace", NAMESPACE);
      transformer.setAttribute("connect", CONNECT_URL);
      transformer.setAttribute("output-selector", `#${OUTPUT_ID}`);

      output.append(storage);
      output.outputConfigurator.append(transformer);
      output.loadSelected();
    })
    .catch((err) => {
      console.error("Failed to load the Firkin + Automerge output:", err);
    });
}
