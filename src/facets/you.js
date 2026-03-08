import { html, render } from "lit-html";
import { keyed } from "lit-html/directives/keyed.js";
import { marked } from "marked";
import { unsafeHTML } from "lit-html/directives/unsafe-html.js";

import * as Output from "~/common/output.js";
import foundation from "~/common/facets/foundation.js";
import { effect } from "~/common/signal.js";
import { nothing } from "~/common/element.js";

////////////////////////////////////////////
// YOUR COLLECTION
////////////////////////////////////////////

/** @type {HTMLElement | null} */
const listEl = document.querySelector("#list");
if (!listEl) throw new Error("List element not found");

const output = foundation.orchestrator.output();

listEl.innerHTML = "";

effect(() => {
  const col = output.facets.collection().sort((a, b) => {
    return a.name.toLocaleLowerCase().localeCompare(b.name.toLocaleLowerCase());
  });

  const state = output.facets.state();

  const h = col.length && state === "loaded"
    ? html`
      <ul style="margin: 0; max-width: none;">
        ${col.map((c) =>
          keyed(
            c.id,
            html`
              <li style="margin-bottom: var(--space-sm)">
                <div style="position: relative;">
                  <a href="facets/l/?id=${c.id}">
                    ${c.name}
                  </a>
                  <button
                    class="button--fixed button--transparent"
                    popovertarget="facet-menu-col-${c.id}"
                    style="anchor-name: --facet-anchor-col-${c
                      .id}; position: absolute; right: 0; top: 50%; transform: translateY(-50%);"
                  >
                    <i class="ph-fill ph-dots-three-circle"></i>
                  </button>
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
                          <i class="ph-fill ph-code"></i>
                          <span>Custom code</span>
                        </span>
                      `}
                  </div>
                </div>

                <!-- Dropdown Menu -->
                <div
                  id="facet-menu-col-${c.id}"
                  class="dropdown"
                  style="position-anchor: --facet-anchor-col-${c.id}"
                  popover
                >
                  <a href="facets/l/?id=${c.id}">
                    <span class="with-icon">
                      <i class="ph-fill ph-globe"></i> Open
                    </span>
                  </a>
                  <a @click="${() => editFacet(c)}">
                    <span class="with-icon">
                      <i class="ph-fill ph-cursor-text"></i> Edit
                    </span>
                  </a>
                  <a @click="${deleteFacet({ id: c.id })}">
                    <span class="with-icon">
                      <i class="ph-fill ph-eraser"></i> Delete
                    </span>
                  </a>
                </div>
              </li>
            `,
          )
        )}
      </ul>
    `
    : state === "loaded"
    ? emptyFacetsList
    : html`
      <div class="with-icon">
        <i class="ph-bold ph-spinner-gap"></i>
        Loading items
      </div>
    `;

  render(h, listEl);
});

const emptyFacetsList = html`
  <div>
    <i class="ph-fill ph-info"></i> You have not saved any facets yet.
  </div>
`;

/**
 * @param {{ id: string }} _
 */
function deleteFacet({ id }) {
  return async () => {
    const c = confirm("Are you sure you want to delete this facet?");
    if (!c) return;

    await Output.waitUntilLoaded(output.facets);

    output.facets.save(
      output.facets.collection().filter((c) => !(c.id === id)),
    );
  };
}
