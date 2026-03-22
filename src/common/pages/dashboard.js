import { html, render } from "lit-html";
import { keyed } from "lit-html/directives/keyed.js";
import { marked } from "marked";
import { unsafeHTML } from "lit-html/directives/unsafe-html.js";

import * as FacetCategory from "~/common/facets/category.js";
import { effect, signal } from "~/common/signal.js";
import { facetFromURI } from "~/common/facets/utils.js";
import { nothing } from "~/common/element.js";

import { deleteFacet, saveFacet } from "./crud.js";
import { output } from "./output.js";
import OutputOrchestrator from "@toko/diffuse/components/orchestrator/output/element.js";

// Signals
const activeFilter = signal("all");

/**
 * @import {OutputElement} from "~/components/output/types.d.ts";
 */

const addFromUri = () =>
  html`
    <li
      class="grid-item"
      style="color: ${activeFilter.value === "all"
        ? "inherit"
        : FacetCategory.color(
          /** @type {any} */ ({ kind: activeFilter.value }),
        )}; background: oklch(from currentColor l c h / 0.0625);"
    >
      <div
        class="grid-item__contents"
        style="display: flex; align-items: center; justify-content: center;"
      >
        <button
          class="button--transparent with-icon"
          style="color: inherit; font-size: var(--fs-sm); font-weight: 600;"
          @click="${openAddFromURIModal}"
        >
          <i class="ph-fill ph-plus-circle"></i>
          Add from URI
        </button>
      </div>
    </li>
  `;

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
// DIALOG
////////////////////////////////////////////

function openAddFromURIModal() {
  let dialog = /** @type {HTMLDialogElement | null} */ (
    document.getElementById("add-from-uri-dialog")
  );

  if (!dialog) {
    dialog = /** @type {HTMLDialogElement} */ (
      document.createElement("dialog")
    );

    dialog.id = "add-from-uri-dialog";
    dialog.style.cssText =
      "position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); margin: 0;";

    render(
      html`
        <form id="add-from-uri-form">
          <p>
            <strong>Load a facet from a URI.</strong> Currently supported URI schemes:
            <code>https</code>, <code>at</code> (AT Protocol) and <code>diffuse</code>
            (references internal facets).
          </p>

          <div style="display: flex; flex-direction: column; gap: var(--space-xs)">
            <div>
              <label>Name</label>
              <input
                id="add-uri-name"
                type="text"
                placeholder="My Feature Name"
                required
                autocomplete="off"
              />
            </div>
            <div>
              <label>Kind</label>
              <select id="add-uri-kind">
                <option value="interactive">interface</option>
                <option value="prelude">feature</option>
              </select>
            </div>
            <div>
              <label>URI</label>
              <input
                id="add-uri-uri"
                type="url"
                placeholder="at://..."
                required
                autocomplete="off"
              />
            </div>
          </div>
          <div style="display: flex; gap: var(--space-xs); margin-top: var(--space-sm)">
            <button type="submit">Add</button>
            <button type="button" id="add-uri-cancel">
              Cancel
            </button>
          </div>
        </form>
      `,
      dialog,
    );

    document.body.appendChild(dialog);

    dialog.querySelector("#add-uri-cancel")?.addEventListener("click", () => {
      /** @type {HTMLDialogElement} */ (dialog).close();
    });

    dialog.querySelector("#add-from-uri-form")?.addEventListener(
      "submit",
      async (e) => {
        e.preventDefault();

        const nameEl = /** @type {HTMLInputElement} */ (
          dialog?.querySelector("#add-uri-name")
        );

        const kindEl = /** @type {HTMLSelectElement} */ (
          dialog?.querySelector("#add-uri-kind")
        );

        const uriEl = /** @type {HTMLInputElement} */ (
          dialog?.querySelector("#add-uri-uri")
        );

        const name = nameEl?.value.trim() ?? "";
        const kind = kindEl?.value ?? "interactive";
        const uri = uriEl?.value.trim() ?? "";
        if (!name || !uri) return;

        const facet = await facetFromURI({ kind, name, uri }, {
          fetchHTML: false,
        });
        await saveFacet(facet);

        /** @type {HTMLDialogElement} */ (dialog).close();
      },
    );
  }

  const nameEl = /** @type {HTMLInputElement} */ (
    dialog.querySelector("#add-uri-name")
  );
  const kindEl = /** @type {HTMLSelectElement} */ (
    dialog.querySelector("#add-uri-kind")
  );
  const uriEl = /** @type {HTMLInputElement} */ (
    dialog.querySelector("#add-uri-uri")
  );
  if (nameEl) nameEl.value = "";
  if (kindEl) kindEl.value = "interactive";
  if (uriEl) uriEl.value = "";

  dialog.showModal();
}

////////////////////////////////////////////
// LIST
////////////////////////////////////////////

/** @type {() => void | undefined} */
let stopMonitor;

/** */
export async function renderList() {
  if (stopMonitor) stopMonitor();

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
      <div class="with-icon">
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
        filter === "all" ||
        (filter === "prelude" ? c.kind === "prelude" : c.kind !== "prelude")
      )
      .sort((a, b) => {
        return a.name.toLocaleLowerCase().localeCompare(
          b.name.toLocaleLowerCase(),
        );
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
        class="button--border button--tiny button--bg-accent button--tr-accent ${filter ===
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
              <li class="grid-item">
                <div class="grid-item__contents">
                  <div class="grid-item__title">
                    ${title}
                    <span class="grid-item__kind" style="background-color: ${color};"
                    >${kind}</span>
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

                <div class="grid-item__menu">
                  <a
                    class="button button--transparent"
                    title="Edit"
                    href="build/?id=${encodeURIComponent(c.id)}"
                  >
                    <i class="ph-fill ph-code-block"></i>
                  </a>
                  <hr />
                  <button
                    class="button--transparent"
                    title="Delete"
                    @click="${deleteFacet({ id: c.id })}"
                  >
                    <i class="ph-fill ph-skull"></i>
                  </button>
                </div>
              </li>
            `,
          );
        })} ${addFromUri()}
      </ul>
    `
    : html`
      ${filterBar} ${emptyFacetsList()}
      <ul class="grid" style="margin: var(--space-sm) 0 0">
        ${addFromUri()}
      </ul>
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
