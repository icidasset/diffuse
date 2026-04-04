import * as Output from "~/common/output.js";
import { facetFromURI } from "~/common/facets/utils.js";
import { effect } from "~/common/signal.js";

import { output } from "./output.js";

////////////////////////////////////////////
// FILTER
////////////////////////////////////////////

export function setupFilter() {
  /** @type {NodeListOf<HTMLElement>} */
  const kindButtons = document.querySelectorAll(
    ".grid-filter button[data-filter]",
  );

  /** @type {NodeListOf<HTMLElement>} */
  const items = document.querySelectorAll(".grid-item");

  // Build category buttons from the categories present in the current grid
  const categoriesEl = document.querySelector(".grid-filter--categories");
  const categories = /** @type {string[]} */ (
    [...new Set([...items].map((i) => i.dataset.category).filter(Boolean))]
      .sort()
  );

  /** @type {HTMLElement | null} */
  let categoryLabelEl = null;
  /** @type {HTMLElement | null} */
  let categoryMenuEl = null;

  if (categoriesEl && categories.length > 1) {
    categoryLabelEl = document.createElement("span");
    categoryLabelEl.textContent = "All";

    const triggerBtn = document.createElement("button");
    triggerBtn.className = "button--border button--tiny button--transparent";
    triggerBtn.setAttribute("popovertarget", "grid-category-menu");
    const span = document.createElement("span");
    span.className = "with-icon";
    span.appendChild(categoryLabelEl);
    const caret = document.createElement("i");
    caret.className = "ph-bold ph-caret-down";
    span.appendChild(caret);
    triggerBtn.appendChild(span);

    categoryMenuEl = document.createElement("div");
    categoryMenuEl.id = "grid-category-menu";
    categoryMenuEl.className = "dropdown";
    categoryMenuEl.setAttribute("popover", "");

    for (const cat of ["all", ...categories]) {
      const item = document.createElement("button");
      item.dataset.category = cat;
      item.textContent = cat === "all" ? "All" : cat;
      item.addEventListener("click", () => {
        activeCategory = cat;
        const url = new URL(location.href);
        if (cat === "all") url.searchParams.delete("category");
        else url.searchParams.set("category", cat);
        history.replaceState(null, "", url);
        categoryMenuEl?.hidePopover();
        applyFilter(activeKind, activeCategory);
      });
      categoryMenuEl.appendChild(item);
    }

    categoriesEl.appendChild(triggerBtn);
    categoriesEl.appendChild(categoryMenuEl);
  }

  let activeKind = "all";
  let activeCategory = "all";

  /**
   * @param {string} kind
   * @param {string} category
   */
  function applyFilter(kind, category) {
    kindButtons.forEach((b) => {
      const transparent = b.dataset.filter !== kind;
      if (b.classList.contains("button--transparent") !== transparent) {
        b.classList.toggle("button--transparent", transparent);
      }
    });
    if (categoryLabelEl) {
      categoryLabelEl.textContent = category === "all" ? "All" : category;
    }
    items.forEach((item) => {
      const kindMatch = kind === "all" || item.dataset.kind === kind;
      const catMatch = category === "all" || item.dataset.category === category;
      item.hidden = !(kindMatch && catMatch);
    });
  }

  kindButtons.forEach((b) => {
    b.addEventListener("click", () => {
      activeKind = b.dataset.filter ?? "all";
      const url = new URL(location.href);
      if (activeKind === "all") url.searchParams.delete("filter");
      else url.searchParams.set("filter", activeKind);
      history.replaceState(null, "", url);
      applyFilter(activeKind, activeCategory);
    });
  });

  const params = new URL(location.href).searchParams;
  activeKind = params.get("filter") ?? "all";
  activeCategory = params.get("category") ?? "all";
  applyFilter(activeKind, activeCategory);
}

////////////////////////////////////////////
// TOGGLE BUTTONS
////////////////////////////////////////////

export function insertToggleButtons() {
  const gridItems = /** @type {NodeListOf<HTMLLIElement>} */ (
    document.querySelectorAll(".grid li")
  );

  for (const li of gridItems) {
    const button = li.querySelector("button[data-action='toggle']");
    if (!button) continue;

    button.addEventListener("click", async () => {
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
      const button = /** @type {HTMLElement | null} */ (
        li.querySelector("button[data-action='toggle']")
      );
      const icon = button?.querySelector("i");

      if (!button || !icon || !uri) continue;

      button.style.opacity = "revert-layer";

      const item = colMap.get(uri);
      const isActive = item && item.html === undefined;
      const isPrelude = li.dataset.kind === "prelude";

      button.title = isActive
        ? (isPrelude ? "Remove feature" : "Remove bookmark")
        : (isPrelude ? "Add feature" : "Add bookmark");
      icon.className = isActive
        ? isPrelude ? "ph-bold ph-check" : "ph-fill ph-bookmark-simple"
        : isPrelude
        ? "ph-bold ph-plus"
        : "ph-bold ph-bookmark-simple";
      /** @type {HTMLElement} */ (icon).style.color = isActive
        ? li.dataset.activeColor ?? "var(--accent-twist-2)"
        : "";
    }
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
