import foundation from "~/common/foundation.js";
import {
  CLASS as PathCollectionsTransformer,
} from "~/components/transformer/output/refiner/path-collections/element.js";

/**
 * @param {Object} [args] - Options
 * @param {number} [args.pathPart=1] - 1-based path part of each track's URI to group playlists by.
 */
export async function setup({ pathPart = 1 } = {}) {
  // The output orchestrator. On loader pages this returns the element the
  // loader already created.
  const output = await foundation.orchestrator.output();

  await customElements.whenDefined(output.localName);

  // Insert the path collections refiner into the orchestrator's internal
  // output chain, so every consumer of the orchestrator sees the generated
  // collections.
  let transformer = output.querySelector("dtor-path-collections");

  if (!transformer) {
    transformer = new PathCollectionsTransformer();
    transformer.id = "do-output__path-collections";
    transformer.setAttribute("group", foundation.GROUP);
    transformer.setAttribute("path-part", String(pathPart));

    // The entry point of the orchestrator's internal output chain. The
    // transformer takes over its `output-selector`, so the chain becomes:
    // entry → transformer → previous target.
    const entry = output.output;
    const entrySelector = entry.getAttribute("output-selector");

    if (!entrySelector) {
      throw new Error("Output entry point is missing its output-selector");
    }

    transformer.setAttribute("output-selector", entrySelector);
    entry.after(transformer);
    entry.setAttribute("output-selector", `#${transformer.id}`);
  }

  await customElements.whenDefined(transformer.localName);
}
