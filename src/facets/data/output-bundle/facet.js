import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

const DASL_SYNC_NAME = "dtob-dasl-sync";
const DROPBOX_OUTPUT_NAME = "dob-dropbox";
const S3_OUTPUT_NAME = "dob-s3";

/**
 * @import OutputOrchestrator from "~/components/orchestrator/output/element.js"
 */

/** @type {Set<string>} */
let previouslyActivated = new Set();

/**
 * Setup DOM elements when needed.
 */
effect(() => {
  const output = foundation.signals.orchestrator.output();
  if (!output) return;

  dropbox(output);
  s3(output);

  output.loadSelected();
});

/**
 * Dynamically import custom elements when needed.
 */
effect(() => {
  const output = foundation.signals.orchestrator.output();
  if (!output) return;

  const set = output.activated();
  const newlyActicated = set.difference(previouslyActivated);

  previouslyActivated = set;

  Array.from(newlyActicated).map((id) => {
    switch (id) {
      case "do-output__dc-output__s3":
        return s3CustomElements();
      case "do-output__dc-output__dropbox":
        return dropboxCustomElements();
    }
  });
});

////////////////////////////////////////////
// DROPBOX
////////////////////////////////////////////

/**
 * Wraps user data in a custom CBOR encode data structure
 * which tracks what identifiers are removed.
 * The transformer used here allows for merging of data.
 *
 * @param {OutputOrchestrator} output
 */
export function dropbox(output) {
  const out = document.createElement(DROPBOX_OUTPUT_NAME);
  out.setAttribute("group", foundation.GROUP);
  out.setAttribute("id", "do-output__dob-dropbox");

  const sync = document.createElement(DASL_SYNC_NAME);
  sync.setAttribute("group", foundation.GROUP);
  sync.setAttribute("id", "do-output__dc-output__dropbox");
  sync.setAttribute("label", "Dropbox");
  sync.setAttribute("namespace", "dropbox");
  sync.setAttribute(
    "connect",
    "facets/connect/dropbox/index.tile",
  );
  sync.setAttribute("output-selector", "#do-output__dob-dropbox");

  // Add elements to DOM, make sure the transformer
  // is added to the configurator so the user can select it.
  output.append(out);
  output.outputConfigurator.append(sync);
}

export function dropboxCustomElements() {
  import("~/components/output/bytes/dropbox/element.js");
  import(
    "~/components/transformer/output/bytes/dasl-sync/element.js"
  );
}

////////////////////////////////////////////
// S3
////////////////////////////////////////////

/**
 * Wraps user data in a custom CBOR encode data structure
 * which tracks what identifiers are removed.
 * The transformer used here allows for merging of data.
 *
 * @param {OutputOrchestrator} output
 */
export function s3(output) {
  const out = document.createElement(S3_OUTPUT_NAME);
  out.setAttribute("group", foundation.GROUP);
  out.setAttribute("id", "do-output__dob-s3");

  const sync = document.createElement(DASL_SYNC_NAME);
  sync.setAttribute("group", foundation.GROUP);
  sync.setAttribute("id", "do-output__dc-output__s3");
  sync.setAttribute("label", "S3");
  sync.setAttribute("namespace", "s3");
  sync.setAttribute(
    "connect",
    "facets/connect/s3/index.tile",
  );
  sync.setAttribute("output-selector", "#do-output__dob-s3");

  // Add elements to DOM, make sure the transformer
  // is added to the configurator so the user can select it.
  output.append(out);
  output.outputConfigurator.append(sync);
}

export function s3CustomElements() {
  import("~/components/output/bytes/s3/element.js");
  import(
    "~/components/transformer/output/bytes/dasl-sync/element.js"
  );
}
