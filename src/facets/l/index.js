import foundation from "@common/facets/foundation.js";
import { createLoader } from "@common/loader.js";

createLoader({
  $type: "sh.diffuse.output.facet",
  label: "Facet",
  source: () => {
    const output = foundation.orchestrator.output();
    return output.facets;
  },
  render(facet) {
    // TODO: Validate if CID matches HTML
    const container = /** @type {HTMLDivElement} */ (
      document.querySelector("#container")
    );

    const range = document.createRange();
    range.selectNode(container);
    const documentFragment = range.createContextualFragment(facet.html ?? "");

    container.innerHTML = "";
    container.append(documentFragment);
  },
});
