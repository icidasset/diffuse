import * as Output from "~/common/output.js";
import { facetFromURI } from "~/common/facets/utils.js";
import { effect } from "~/common/signal.js";

import { output } from "./output.js";

////////////////////////////////////////////
// FILTER
////////////////////////////////////////////

export function setupFilter() {
  /** @type {NodeListOf<HTMLElement>} */
  const buttons = document.querySelectorAll(".grid-filter button");

  /** @type {NodeListOf<HTMLElement>} */
  const items = document.querySelectorAll(".grid-item");

  buttons.forEach((b) => {
    b.addEventListener("click", () => {
      buttons.forEach((b) => b.classList.add("button--transparent"));
      b.classList.remove("button--transparent");

      const filter = b.dataset.filter;

      items.forEach((item) => {
        const kind = item.dataset.kind;
        const show = filter === "all" || kind === filter;
        item.hidden = !show;

        /** @type {HTMLElement | null} */
        const kindEl = item.querySelector(".grid-item__kind");
        if (!kindEl) return;
        kindEl.hidden = filter !== "all";
      });
    });
  });
}

////////////////////////////////////////////
// OUTPUT INDICATOR
////////////////////////////////////////////

/** @type {() => void | undefined} */
let stopOutputIndicator;

export async function setupOutputIndicator() {
  if (stopOutputIndicator) stopOutputIndicator();

  const filterEl = document.querySelector(".grid-filter");
  if (!filterEl) return;

  const out = await output();

  /** @type {HTMLElement | null} */
  const indicator = filterEl.querySelector(".grid-filter--output");
  if (!indicator) return;

  /** @type {HTMLElement | null} */
  const label = filterEl.querySelector(".grid-filter--label-output");
  if (!label) return;

  setTimeout(() => {
    indicator.style.opacity = "1";
    label.style.opacity = "0.4";
  }, 250);

  stopOutputIndicator = effect(() => {
    const selected = out.selected();
    const label = selected?.label ?? selected?.getAttribute?.("label") ??
      "Local storage";
    indicator.textContent = label;
  });
}

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
    button.style.cssText =
      `color: oklch(from currentColor l c h / 0.4); font-size: var(--fs-md); opacity: 0; padding: 0;`;
    button.innerHTML = `<i class="ph-fill ph-toggle-left"></i>`;

    button.addEventListener("click", async (event) => {
      event.preventDefault();

      const uri = li.getAttribute("data-uri");
      const name = li.getAttribute("data-name");
      const kind = li.getAttribute("data-kind") ?? undefined;
      const description = li.getAttribute("data-description") ?? undefined;

      if (!uri || !name) return;

      const out = await output();
      const collection = await Output.data(out.facets);
      const isActive = collection.some((f) =>
        f.uri === uri && f.html === undefined
      );

      if (isActive) {
        out.facets.save(collection.filter((f) => f.uri !== uri));
      } else {
        const facet = await facetFromURI({ description, kind, name, uri }, {
          fetchHTML: false,
        });
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
  const out = await output();

  stopMonitor = effect(() => {
    const gridItems = /** @type {NodeListOf<HTMLLIElement>} */ (
      document.querySelectorAll(".grid li")
    );

    const col = out.facets.collection();
    const collection = col.state === "loaded" ? col.data : [];
    const colMap = new Map(collection.map((f) => [f.uri, f]));

    for (const li of gridItems) {
      const uri = li.getAttribute("data-uri");
      const button =
        /** @type {HTMLElement | null} */ (li.querySelector("button"));
      const icon = button?.querySelector("i");

      if (!button || !icon || !uri) continue;

      button.style.opacity = "revert-layer";

      const item = colMap.get(uri);
      const isActive = item && item.html === undefined;

      button.title = isActive
        ? "Remove from your collection"
        : "Add to your collection";
      icon.className = isActive
        ? "ph-fill ph-toggle-right"
        : "ph-fill ph-toggle-left";
      /** @type {HTMLElement} */ (icon).style.color = isActive
        ? li.dataset.activeColor ?? "var(--accent-twist-2)"
        : "";
    }
  });
}
