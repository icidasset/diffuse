import { html, render } from "lit-html";
import { keyed } from "lit-html/directives/keyed.js";
import { marked } from "marked";
import { unsafeHTML } from "lit-html/directives/unsafe-html.js";

import * as Output from "~/common/output.js";
import foundation from "~/common/facets/foundation.js";
import { nothing } from "~/common/element.js";

import { deleteFacet } from "./crud.js";

const EMPTY_FACETS_LIST = html`
  <div>
    <i class="ph-fill ph-info"></i> You have not saved any facets yet.
  </div>
`;

/** */
export async function renderList() {
  /** @type {HTMLElement | null} */
  const listEl = document.querySelector("#list");
  if (!listEl) throw new Error("List element not found");
  listEl.innerHTML = "";

  const output = foundation.orchestrator.output();

  if (output.facets.state() !== "loaded") {
    const loading = html`
      <div class="with-icon">
        <i class="ph-bold ph-spinner-gap"></i>
        Loading items
      </div>
    `;

    render(loading, listEl);
  }

  await Output.waitUntilLoaded(output.facets);

  const col = output.facets.collection().sort((a, b) => {
    return a.name.toLocaleLowerCase().localeCompare(b.name.toLocaleLowerCase());
  });

  const h = col.length
    ? html`
      <ul class="grid" style="margin: 0">
        ${col.map((c, index) =>
          keyed(
            c.id,
            html`
              <li class="grid-item">
                <div class="grid-item__contents">
                  <div>
                    <a
                      href="facets/l/?id=${c
                        .id}"
                      style="display: inline-block; padding: var(--space-3xs) 0"
                    >
                      ${c.name}
                    </a>
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
                    href="facets/build/?id=${encodeURIComponent(c.id)}"
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
          )
        )}
      </ul>
    `
    : EMPTY_FACETS_LIST;

  render(h, listEl);
}
