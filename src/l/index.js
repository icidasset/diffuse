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

// Reload when the prelude facets change after initial load.
// initialPreludeKey is captured the first time the collection finishes loading
// (not at startup when it's still pending), so the effect never fires on the
// very first load and causes an infinite reload loop.
const preludeKey = computed(() => {
  const col = output.facets.collection();
  if (col.state !== "loaded") return null;
  return col.data
    .filter((f) => f.kind === "prelude")
    .map((f) => `${f.id}:${f.cid ?? ""}:${f.enabled !== false}`)
    .join(",");
});

let initialPreludeKey = /** @type {string | null} */ (null);

effect(() => {
  const key = preludeKey();
  if (key === null) return;
  if (initialPreludeKey === null) { initialPreludeKey = key; return; }
  if (key !== initialPreludeKey) {
    const lastReload = Number(sessionStorage.getItem("diffuse/l/last-reload") ?? 0);
    if (Date.now() - lastReload < 60_000) return;
    sessionStorage.setItem("diffuse/l/last-reload", String(Date.now()));
    window.location.reload();
  }
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
