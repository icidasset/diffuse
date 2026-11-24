import foundation from "~/common/foundation.js";
import {
  CONNECT_URL,
  FIRKIN_METHODS,
  optionId,
} from "~/common/firkin/methods.js";
import { effect } from "~/common/signal.js";

/**
 * The Record Sync userdata output, as an opt-in feature facet.
 *
 * The output itself is one self-contained element
 * (`~/components/output/raw/firkin/encrypted/record-sync/element.js`); this
 * facet only puts it in front of the user: it appends the output option to the
 * configurator, and imports the element when the option is activated so pages
 * that don't use Record Sync never load it.
 *
 * The connect facet finds the option through the configurator's usual
 * `waitForOutputOption(label)` handshake, so it works regardless of which
 * facet added it.
 */

/** @type {Set<string>} */
let previouslyActivated = new Set();

/**
 * Add the option to the output configurator.
 */
effect(() => {
  const output = foundation.signals.orchestrator.output();
  if (!output) return;

  for (const method of FIRKIN_METHODS) {
    if (method.kind !== "record-sync") continue;

    // The effect can re-run; never append the same option twice.
    if (document.getElementById(optionId(method))) continue;

    output.outputConfigurator.append(optionElement(method));
  }

  output.loadSelected();
});

/**
 * Import the element when the option becomes the active one.
 */
effect(() => {
  const output = foundation.signals.orchestrator.output();
  if (!output) return;

  const set = output.activated();
  const newlyActivated = set.difference(previouslyActivated);

  previouslyActivated = set;

  for (const method of FIRKIN_METHODS) {
    if (method.kind !== "record-sync") continue;
    if (!newlyActivated.has(optionId(method))) continue;

    import(
      "~/components/output/raw/firkin/encrypted/record-sync/element.js"
    ).then(async () => {
      await customElements.whenDefined("dob-record-sync");
    }).catch((err) => {
      console.error("Failed to load the Record Sync output:", err);
    });
  }
});

/**
 * @param {import("~/common/firkin/methods.js").FirkinMethod} method
 */
function optionElement(method) {
  const element = document.createElement("dob-record-sync");

  element.setAttribute("group", foundation.GROUP);
  element.setAttribute("id", optionId(method));
  element.setAttribute("namespace", method.id);
  if (method.keep !== undefined) {
    element.setAttribute("keep", String(method.keep));
  }
  element.setAttribute("label", method.label);
  element.setAttribute("connect", CONNECT_URL);
  element.setAttribute("provider", method.provider);

  // Deliberately no `provider-default`: Firkin (Loro) stays the preferred
  // method for the provider row; Record Sync is chosen explicitly.

  return element;
}
