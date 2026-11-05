import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

import { NAME as INPUT_NAME } from "~/components/input/wnfs-firkin/element.js";
import { NAME as UPLOAD_NAME } from "~/components/upload/wnfs-firkin/element.js";

/**
 * @import InputConfigurator from "~/components/configurator/input/element.js"
 * @import UploadConfigurator from "~/components/configurator/upload/element.js"
 * @import OutputOrchestrator from "~/components/orchestrator/output/element.js"
 */

// Tag name is stable; importing the element module here would pull the
// WNFS/WASM chunk on every interface load. Import it when the feature is
// actually wired up instead.
const WNFS_OUTPUT_NAME = "dob-wnfs-firkin";

const OUTPUT_ID = "do-output__dc-output__wnfs-firkin";
const INPUT_ID = "di-input__dc-input__wnfs-firkin";
const UPLOAD_ID = "du-upload__dc-upload__wnfs-firkin";

const CONNECT_URL = "facets/connect/wnfs-firkin/index.tile";
const NAMESPACE = "wnfs-firkin";

/**
 * Adds the Firkin + WNFS components: the output (storage itself), plus an
 * audio input and an upload destination that operate on the files in the same
 * WNFS.
 *
 * This is a feature (prelude) facet, so it only runs when the user enables
 * it. The input and upload elements are cheap; their workers talk to the
 * output element over a `BroadcastChannel`, so the WNFS/WASM code stays in the
 * output bundle.
 */
effect(() => {
  const output = foundation.signals.orchestrator.output();
  const input = foundation.signals.configurator.input();
  const upload = foundation.signals.configurator.upload();

  if (output) wnfsFirkin(output);
  if (input) wnfsFirkinInput(input);
  if (upload) wnfsFirkinUpload(upload);
  if (output) output.loadSelected();
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
      storage.setAttribute("namespace", NAMESPACE);
      storage.setAttribute("connect", CONNECT_URL);

      output.outputConfigurator.append(storage);
      output.loadSelected();
    })
    .catch((err) => {
      console.error("Failed to load the Firkin + WNFS output:", err);
    });
}

/**
 * @param {InputConfigurator} input
 */
function wnfsFirkinInput(input) {
  if (input.querySelector(`#${INPUT_ID}`)) return;

  const element = document.createElement(INPUT_NAME);
  element.setAttribute("group", foundation.GROUP);
  element.setAttribute("id", INPUT_ID);
  element.setAttribute("label", "Firkin + WNFS");
  element.setAttribute("namespace", NAMESPACE);

  input.append(element);
}

/**
 * @param {UploadConfigurator} upload
 */
function wnfsFirkinUpload(upload) {
  if (upload.querySelector(`#${UPLOAD_ID}`)) return;

  const element = document.createElement(UPLOAD_NAME);
  element.setAttribute("group", foundation.GROUP);
  element.setAttribute("id", UPLOAD_ID);
  element.setAttribute("label", "Firkin + WNFS");
  element.setAttribute("namespace", NAMESPACE);

  upload.append(element);
}
