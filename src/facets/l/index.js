import foundation from "~/common/facets/foundation.js";
import * as CID from "~/common/cid.js";
import * as Output from "~/common/output.js";
import { createLoader, loadURI, renderError } from "~/common/loader.js";

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

    container.innerHTML = "";

    const range = document.createRange();
    range.selectNode(container);
    const documentFragment = range.createContextualFragment(facet.html ?? "");

    const preludes = facets
      .filter((f) => f.kind === "prelude")
      .sort((a, b) => a.name.localeCompare(b.name));

    for (const prelude of preludes) {
      const html = prelude.html ??
        (prelude.uri ? await loadURI(prelude.uri) : "");
      if (!html) continue;
      const preludeFragment = range.createContextualFragment(html);
      container.append(preludeFragment);
    }

    container.append(documentFragment);
  },
});
