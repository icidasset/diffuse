import * as Output from "~/common/output.js";
import foundation from "~/common/facets/foundation.js";
import { facetFromURI } from "~/common/facets/utils.js";
import { effect } from "~/common/signal.js";

////////////////////////////////////////////
// TOGGLE BUTTONS
////////////////////////////////////////////

export function insertToggleButtons() {
  const gridItems = /** @type {NodeListOf<HTMLLIElement>} */ (
    document.querySelectorAll(".grid li")
  );

  for (const li of gridItems) {
    const container = li.querySelector(".grid-item__title");
    if (!container) continue;

    const button = document.createElement("button");
    button.className = "button--transparent";
    button.style.cssText = "font-size: var(--fs-md); opacity: 0; padding: 0;";
    button.innerHTML = `<i class="ph-fill ph-toggle-left"></i>`;

    button.addEventListener("click", async (event) => {
      event.preventDefault();

      const uri = li.getAttribute("data-uri");
      const name = li.getAttribute("data-name");
      if (!uri || !name) return;

      const out = foundation.orchestrator.output();
      await Output.waitUntilLoaded(out.facets);

      const collection = out.facets.collection();
      const isActive = collection.some((f) => f.uri === uri);

      if (isActive) {
        out.facets.save(collection.filter((f) => f.uri !== uri));
      } else {
        const facet = await facetFromURI({ name, uri }, { fetchHTML: false });
        out.facets.save([...collection, facet]);
      }
    });

    container.appendChild(button);
  }
}

////////////////////////////////////////////
// SYNC ACTIVE STATES
////////////////////////////////////////////

/** @type {() => void | undefined} */
let stopMonitor;

export async function monitorToggleButtonStates() {
  if (stopMonitor) stopMonitor();

  const out = foundation.orchestrator.output();
  await Output.waitUntilLoaded(out.facets);

  stopMonitor = effect(() => {
    const gridItems = /** @type {NodeListOf<HTMLLIElement>} */ (
      document.querySelectorAll(".grid li")
    );

    const collection = out.facets.collection();
    const activeURIs = new Set(collection.map((f) => f.uri));

    for (const li of gridItems) {
      const uri = li.getAttribute("data-uri");
      const button =
        /** @type {HTMLElement | null} */ (li.querySelector("button"));
      const icon = button?.querySelector("i");

      if (!button || !icon || !uri) continue;

      button.style.opacity = "revert-layer";

      const isActive = activeURIs.has(uri);
      button.title = isActive
        ? "Remove from your collection"
        : "Add to your collection";
      icon.className = isActive
        ? "ph-fill ph-toggle-right"
        : "ph-fill ph-toggle-left";
      /** @type {HTMLElement} */ (icon).style.color = isActive
        ? "var(--accent-twist-2)"
        : "";
    }
  });
}
