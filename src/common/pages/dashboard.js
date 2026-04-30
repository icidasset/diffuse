import { html, render } from "lit-html";
import { classMap } from "lit-html/directives/class-map.js";
import { keyed } from "lit-html/directives/keyed.js";
import { marked } from "marked";
import { unsafeHTML } from "lit-html/directives/unsafe-html.js";

import * as FacetCategory from "~/common/facets/category.js";
import { effect, signal } from "~/common/signal.js";

import { nothing } from "~/common/element.js";

import { deleteFacet, toggleFacetEnabled } from "./crud.js";
import { output } from "./output.js";
import { openAddFromURIModal } from "./from-uri.js";

// Signals
const FILTER_STORAGE_KEY = "diffuse/dashboard/filter";
const storedFilter = localStorage.getItem(FILTER_STORAGE_KEY);
const activeFilter = signal(
  storedFilter === "prelude" || storedFilter === "interface" ||
    storedFilter === "base"
    ? storedFilter
    : "all",
);

effect(() => {
  localStorage.setItem(FILTER_STORAGE_KEY, activeFilter.get());
});

/**
 * @import OutputOrchestrator from "~/components/orchestrator/output/element.js";
 */

const emptyFacetsList = () =>
  html`
    <p>
      <span>
        You haven't saved anything yet. Add a facet by browsing the <a
          href="featured/"
        >featured ones</a> or any of the other categories. You can click the toggle
        to quickly add or remove from your collection. Alternatively, add one using
        an URI:
      </span>
    </p>
  `;

////////////////////////////////////////////
// LIST
////////////////////////////////////////////

/** @type {() => void | undefined} */
let stopMonitor;

/** */
export async function renderList() {
  if (stopMonitor) stopMonitor();
  activeFilter.set((() => {
    const stored = localStorage.getItem(FILTER_STORAGE_KEY);
    return stored === "prelude" || stored === "interface" || stored === "base"
      ? stored
      : "all";
  })());

  /** @type {HTMLElement | null} */
  const listEl = document.querySelector("#list");
  if (!listEl) throw new Error("List element not found");

  if (listEl.getAttribute("data-rendered") === "f") {
    listEl.innerHTML = "";
    listEl.removeAttribute("data-rendered");
  }

  const out = await output();

  stopMonitor = effect(() => {
    _renderList(out, listEl);
  });
}

/**
 * @param {OutputOrchestrator} output
 * @param {HTMLElement} listEl
 */
function _renderList(output, listEl) {
  const facetsCol = output.facets.collection();

  if (facetsCol.state !== "loaded") {
    const loading = html`
      <div class="with-icon" style="font-size: var(--fs-sm)">
        <i class="ph-bold ph-spinner animate-spin"></i>
        Loading your software
      </div>
    `;

    render(loading, listEl);
    return;
  }

  const filter = activeFilter.get();

  const col = facetsCol.state === "loaded"
    ? [...facetsCol.data]
      .filter((c) =>
        filter === "base" ? !!c.tags?.includes("base") : (filter === "all" ||
          (filter === "prelude"
            ? c.kind === "prelude"
            : c.kind !== "prelude")) &&
          !c.tags?.includes("base")
      )
      .sort((a, b) => {
        return a.name.toLocaleLowerCase().localeCompare(
          b.name.toLocaleLowerCase(),
        ) || a.id.localeCompare(b.id);
      })
    : [];

  const selected = output.selected();
  const outputLabel = selected?.label ?? selected?.getAttribute?.("label") ??
    "Local storage";

  const filterBar = html`
    <div class="grid-filter">
      <span class="grid-filter--label">Filter by</span>
      <button
        class="button--border button--tiny ${filter === "all"
          ? ""
          : "button--transparent"}"
        @click="${() => activeFilter.set("all")}"
      >
        All
      </button>
      <button
        class="button--border button--tiny button--bg-twist-4 button--tr-twist-4 ${filter ===
            "prelude"
          ? ""
          : "button--transparent"}"
        @click="${() => activeFilter.set("prelude")}"
      >
        Features
      </button>
      <button
        class="button--border button--tiny button--bg-twist-2 button--tr-twist-2 ${filter ===
            "interface"
          ? ""
          : "button--transparent"}"
        @click="${() => activeFilter.set("interface")}"
      >
        Interfaces
      </button>

      <button
        class="button--border button--tiny ${filter === "base"
          ? ""
          : "button--transparent"}"
        title="Show the hidden essential features"
        @click="${() => activeFilter.set("base")}"
      >
        Base
      </button>

      <span class="divider"></span>

      <button
        class="button--border button--tiny button--bg-accent button--tr-accent button--transparent"
        @click="${() => openAddFromURIModal()}"
      >
        <span class="with-repositioned-icon">
          <i class="ph-fill ph-plus-circle"></i>
          <span class="button__supplementary-text">Add from URI</span>
        </span>
      </button>

      <div style="flex: 1"></div>

      <span class="grid-filter--label grid-filter--label-output"
      >Userdata from</span>
      <span class="grid-filter--output">${outputLabel}</span>
    </div>
  `;

  const h = col.length || filter !== "all"
    ? html`
      ${filterBar}
      <ul class="grid" style="margin: 0">
        ${col.map((c, index) => {
          const color = FacetCategory.color(c);
          const kind = FacetCategory.name(c);

          const title = c.kind === "prelude"
            ? html`
              <span style="display: inline-block; padding: var(--space-3xs) 0">
                ${c.name}
              </span>
            `
            : html`
              <a
                href="l/?id=${c
                  .id}"
                style="display: inline-block; padding: var(--space-3xs) 0"
              >
                ${c.name}
              </a>
            `;

          return keyed(
            c.id,
            html`
              <li
                class="grid-item"
                style="--grid-item-color: ${color}"
                ?data-disabled="${!(c.enabled ?? true)}"
              >
                <div class="grid-item__contents">
                  <div class="grid-item__title" style="color: ${color}">
                    ${title}
                  </div>
                  <div class="list-description">
                    <div>
                      ${c.description?.trim().length
                        ? unsafeHTML(
                          marked.parse(c.description, { async: false }),
                        )
                        : nothing}
                    </div>
                    <div>
                      ${c.uri && !c.html
                        ? html`
                          <span class="with-icon">
                            <i class="ph-fill ph-binoculars"></i>
                            <span>Tracking the original <a href="${c
                              .uri}">URI</a></span>
                          </span>
                        `
                        : html`
                          <span class="with-icon">
                            <i class="ph-fill ph-code-simple"></i>
                            <span>Custom code</span>
                          </span>
                        `}
                    </div>
                  </div>
                </div>

                <div class="grid-item__menu ${classMap({
                  "grid-item__menu--active": c.enabled ?? true,
                })}">
                  <button
                    class="button--transparent"
                    title="${(c.enabled ?? true)
                      ? c.kind === "prelude" ? "Disable" : "Dim"
                      : c.kind === "prelude"
                      ? "Enable"
                      : "Light"}"
                    @click="${toggleFacetEnabled({ id: c.id })}"
                  >
                    <i class="${(c.enabled ?? true)
                      ? c.kind === "prelude"
                        ? "ph-fill ph-lightning"
                        : "ph-fill ph-eye"
                      : c.kind === "prelude"
                      ? "ph-bold ph-lightning-slash"
                      : "ph-bold ph-eye-slash"}"></i>
                  </button>
                  <hr />
                  <button
                    class="button--transparent"
                    title="More actions"
                    popovertarget="facet-menu-${c.id}"
                  >
                    <i class="ph-bold ph-dots-three-vertical"></i>
                  </button>
                  <div id="facet-menu-${c.id}" class="dropdown" popover>
                    <a
                      class="with-icon"
                      href="create/?id=${encodeURIComponent(c.id)}"
                    >
                      <i class="ph-fill ph-code-block"></i>
                      Edit
                    </a>
                    <a
                      class="with-icon"
                      href="#"
                      @click="${(/** @type {MouseEvent} */ e) => {
                        e.preventDefault();
                        deleteFacet({ id: c.id })();
                      }}"
                    >
                      <i class="ph-fill ph-skull"></i>
                      Delete
                    </a>
                  </div>
                </div>
              </li>
            `,
          );
        })}
      </ul>
    `
    : html`
      ${filterBar} ${emptyFacetsList()}
    `;

  render(h, listEl);

  setTimeout(() => {
    /** @type {HTMLElement | null} */
    const l = listEl.querySelector(".grid-filter--label-output");

    /** @type {HTMLElement | null} */
    const o = listEl.querySelector(".grid-filter--output");

    if (o && l) {
      l.style.opacity = "0.4";
      o.style.opacity = "1";
    }
  }, 250);
}
