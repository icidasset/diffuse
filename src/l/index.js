import foundation from "~/common/foundation.js";
import * as CID from "~/common/cid.js";
import * as Output from "~/common/output.js";
import { createLoader, linkTileResources, renderError } from "~/common/loader.js";
import { insertPreludes } from "~/common/facets/prelude.js";
import { computed, effect } from "~/common/signal.js";

// Container
const container = /** @type {HTMLDivElement} */ (
  document.querySelector("#container")
);

// The loader relies on features that only work in a secure context.
// Fail fast with a helpful message instead of letting those APIs throw cryptically.
if (!window.isSecureContext) {
  renderError(
    container,
    "Diffuse requires a secure context (HTTPS or localhost)",
  );
  throw new Error("Insecure context: Diffuse loader requires a secure context");
}

// Output element
// Without the orchestrator there's nothing to load, so surface the failure
// instead of leaving the spinner bouncing forever on an unhandled rejection.
const output = await foundation.orchestrator.output().catch((err) => {
  renderError(container, "Failed to initialize the output orchestrator", {
    context: err,
  });
  throw err;
});

// Preludes
const facets = await Output.data(output.facets);
let preludesInserted = false;

// Remember which facets were opened so other facets (e.g. the command menu)
// can return the user to the interface they were last using.
const FACET_HISTORY_KEY = "diffuse/l/facet-history";
const FACET_HISTORY_LIMIT = 10;

/**
 * @param {{ id?: string; name?: string }} facet
 */
function rememberFacet(facet) {
  if (!facet.id || facet.name === "temporary") return;

  try {
    const stored = JSON.parse(
      localStorage.getItem(FACET_HISTORY_KEY) ?? "[]",
    );

    const history = Array.isArray(stored)
      ? stored.filter((id) => typeof id === "string" && id !== facet.id)
      : [];

    history.unshift(facet.id);
    localStorage.setItem(
      FACET_HISTORY_KEY,
      JSON.stringify(history.slice(0, FACET_HISTORY_LIMIT)),
    );
  } catch {
    // localStorage can be unavailable (private mode); history is optional.
  }
}

// Reload when the prelude facets change after initial load.
// initialPreludeKey is captured the first time the collection finishes loading
// (not at startup when it's still pending), so the effect never fires on the
// very first load and causes an infinite reload loop.
const preludeKey = computed(() => {
  const col = output.facets.collection();
  if (col.state !== "loaded") return null;
  return col.data
    .filter((f) => f.kind === "prelude")
    .map((f) => `${f.id}:${f.enabled !== false}:${JSON.stringify(f.resources ?? null)}`)
    .join(",");
});

let initialPreludeKey = /** @type {string | null} */ (null);

effect(() => {
  // Never reload mid-OAuth-callback: a prelude change (e.g. synthesized
  // recovery preludes being replaced by the real synced set) would otherwise
  // abort the in-flight `finalizeAuthorization` token exchange.
  const params = new URLSearchParams(window.location.hash.slice(1));
  if (params.has("code") || params.has("state")) return;

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
    rememberFacet(facet);

    // Tile facets are content addressed: `facet.cid` is resolved during loading
    // from the MASL `resources["/"].src` CID, so the loaded root HTML is
    // verified against it. Plain HTML facets carry no `cid`, so nothing to
    // verify.
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

    // Rewrite absolute `/…` resource URLs to Blob URLs on the detached fragment
    // BEFORE inserting it, so the browser never starts a request for the raw
    // (un-rewritten) path — attaching first would intermittently fetch e.g.
    // `/styles.css` from the build root and 404. If linking fails for any
    // reason, still render the facet rather than leaving the page blank.
    if (facet.tile) {
      try {
        linkTileResources(documentFragment, facet.tile.resources, facet.tile.blocks);
      } catch (err) {
        console.error("Failed to link tile resources", err);
      }
    }
    container.append(documentFragment);
  },
});
