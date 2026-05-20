import foundation from "~/common/foundation.js";
import * as CID from "~/common/cid.js";
import * as Output from "~/common/output.js";
import { createLoader, renderError } from "~/common/loader.js";
import { insertPreludes } from "~/common/facets/prelude.js";
import { computed, effect } from "~/common/signal.js";

// Output element
const output = await foundation.orchestrator.output();

// Contaienr
const container = /** @type {HTMLDivElement} */ (
  document.querySelector("#container")
);

// Preludes
const facets = await Output.data(output.facets);
let preludesInserted = false;

// Reload when the prelude facets change after initial load
const preludeKey = computed(() => {
  const col = output.facets.collection();
  if (col.state !== "loaded") return "";
  return col.data
    .filter((f) => f.kind === "prelude")
    .map((f) => `${f.id}:${f.cid ?? ""}:${f.enabled !== false}`)
    .join(",");
});

const initialPreludeKey = preludeKey();

effect(() => {
  if (preludeKey() === initialPreludeKey) return;
  window.location.reload();
});

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

    if (!preludesInserted) {
      preludesInserted = true;
      await insertPreludes(facets, document.body);
    }

    const range = document.createRange();
    range.selectNode(container);
    const documentFragment = range.createContextualFragment(facet.html ?? "");
    container.append(documentFragment);
  },
});
