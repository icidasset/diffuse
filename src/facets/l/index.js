import foundation from "~/common/facets/foundation.js";
import * as CID from "~/common/cid.js";
import { createLoader, renderError } from "~/common/loader.js";

createLoader({
  $type: "sh.diffuse.output.facet",
  label: "Facet",
  source: () => {
    const output = foundation.orchestrator.output();
    return output.facets;
  },
  async render(facet) {
    const container = /** @type {HTMLDivElement} */ (
      document.querySelector("#container")
    );

    if (facet.cid) {
      const valid = await CID.verify(
        new TextEncoder().encode(facet.html ?? ""),
        facet.cid,
      );

      if (!valid) {
        renderError(
          container,
          "CID mismatch: HTML content does not match the CID",
        );
        return;
      }
    }

    const range = document.createRange();
    range.selectNode(container);
    const documentFragment = range.createContextualFragment(facet.html ?? "");

    container.innerHTML = "";
    container.append(documentFragment);
  },
});
