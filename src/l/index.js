import foundation from "~/common/facets/foundation.js";
import * as CID from "~/common/cid.js";
import * as Output from "~/common/output.js";
import { createLoader, renderError } from "~/common/loader.js";
import { insertPreludes } from "~/common/facets/prelude.js";

// Output element
const output = await foundation.orchestrator.output();

// Contaienr
const container = /** @type {HTMLDivElement} */ (
  document.querySelector("#container")
);

// Preludes
const facets = await Output.data(output.facets);

// Load
createLoader({
  $type: "sh.diffuse.output.facet",
  label: "Facet",
  source: () => output.facets,
  async render(facet) {
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

    // Remove loading animation
    container.innerHTML = "";

    // Execute all the prelude facets
    await insertPreludes(facets, container);

    // Execute main facet
    const range = document.createRange();
    range.selectNode(container);
    const documentFragment = range.createContextualFragment(facet.html ?? "");
    container.append(documentFragment);
  },
});
