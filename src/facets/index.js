import { html, render } from "lit-html";
import { keyed } from "lit-html/directives/keyed.js";
import { marked } from "marked";
import { unsafeHTML } from "lit-html/directives/unsafe-html.js";

import { basicSetup, EditorView } from "codemirror";
import { css as langCss } from "@codemirror/lang-css";
import { html as langHtml } from "@codemirror/lang-html";
import { javascript as langJs } from "@codemirror/lang-javascript";
import { autocompletion } from "@codemirror/autocomplete";

import * as CID from "@common/cid.js";
import foundation from "@common/facets/foundation.js";
import { effect, signal } from "@common/signal.js";
import { facetFromURI } from "@common/facets/utils.js";
import { nothing } from "@common/element.js";
import { loadURI } from "@common/loader.js";

/**
 * @import {Facet} from "@definitions/types.d.ts"
 */

////////////////////////////////////////////
// SAVE & FORK
////////////////////////////////////////////

document.body.addEventListener(
  "click",
  /**
   * @param {MouseEvent} event
   */
  async (event) => {
    const target = /** @type {HTMLElement} */ (event.target);
    const rel = target.getAttribute("rel");
    if (!rel) return;

    const uri = target.closest("li")?.getAttribute("data-uri");
    if (!uri) return;

    const name = target.closest("li")?.getAttribute("data-name");
    if (!name) return;

    switch (rel) {
      case "fork": {
        const facet = await facetFromURI({ name, uri }, { fetchHTML: true });
        editFacet(facet);
        document.querySelector("#build")?.scrollIntoView();
        break;
      }
      case "save": {
        const facet = await facetFromURI({ name, uri }, { fetchHTML: false });
        const out = foundation.orchestrator.output();

        out.facets.save([
          ...out.facets.collection(),
          facet,
        ]);
        break;
      }
    }
  },
);

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
      <ul>
        ${col.map((c) =>
          keyed(
            c.id,
            html`
              <li>
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
      <div class="with-icon" style="font-size: var(--fs-sm);">
        <i class="ph-bold ph-spinner-gap"></i>
        Loading items
      </div>
    `;

  render(h, listEl);
});

const emptyFacetsList = html`
  <p style="margin-bottom: 0;">
    <i class="ph-fill ph-info"></i> You have not saved any facets yet.
  </p>
`;

/**
 * @param {{ id: string }} _
 */
function deleteFacet({ id }) {
  return () => {
    const c = confirm("Are you sure you want to delete this facet?");
    if (!c) return;

    output.facets.save(
      output.facets.collection().filter((c) => !(c.id === id)),
    );
  };
}

////////////////////////////////////////////
// BUILD
////////////////////////////////////////////

const $editingFacet = signal(/** @type {Facet | null} */ (null));

// Code editor
const editorContainer = document.body.querySelector("#html-input-container");
if (!editorContainer) throw new Error("Editor container not found");

const editor = new EditorView({
  parent: editorContainer,
  doc: `
<main>
  <h1 id="now-playing">
    Waiting on tracks &amp; queue to load ...
  </h1>
</main>

<style>
  @import "./styles/base.css";
  @import "./styles/diffuse/page.css";
</style>

<script type="module">
  import foundation from "./common/facets/foundation.js";
  import { effect } from "./common/signal.js";

  const components = foundation.features.fillQueueAutomatically();
  const myHtmlElement = document.querySelector("#now-playing");

  effect(() => {
    const now = components.engine.queue.now();
    const currentlyPlaying = now ? components.orchestrator.output.tracks.collection().find(t => t.id === now.id) : undefined;
    if (currentlyPlaying && myHtmlElement) {
      myHtmlElement.innerText = \`\$\{currentlyPlaying.tags.artist} - \$\{currentlyPlaying.tags.title}\`;
    }
  })
</script>
  `.trim(),
  extensions: [
    basicSetup,
    langHtml(),
    langCss(),
    langJs(),
    autocompletion(),
  ],
});

// Form submit
document.querySelector("#build-form")?.addEventListener(
  "submit",
  onBuildSubmit,
);

/**
 * @param {Event} event
 */
async function onBuildSubmit(event) {
  event.preventDefault();

  const nameEl = /** @type {HTMLInputElement | null} */ (document.querySelector(
    "#name-input",
  ));

  const descriptionEl = /** @type {HTMLTextAreaElement | null} */ (
    document.querySelector("#description-input")
  );

  const html = editor.state.doc.toString();
  const cid = await CID.create(0x55, new TextEncoder().encode(html));
  const name = nameEl?.value ?? "nameless";
  const description = descriptionEl?.value ?? "";

  /** @type {Facet} */
  const facet = $editingFacet.value
    ? {
      ...$editingFacet.value,
      cid,
      description,
      html,
      name,
    }
    : {
      $type: "sh.diffuse.output.facet",
      id: crypto.randomUUID(),
      cid,
      description,
      html,
      name,
    };

  switch (/** @type {any} */ (event).submitter.name) {
    case "save":
      await saveFacet(facet);
      break;
    case "save+open":
      await saveFacet(facet);
      globalThis.open(`./facets/l/?id=${facet.id}`, "blank");
      break;
  }
}

/**
 * @param {Facet} ogFacet
 */
async function editFacet(ogFacet) {
  const facet = { ...ogFacet };
  const nameEl = /** @type {HTMLInputElement | null} */ (document.querySelector(
    "#name-input",
  ));

  const descriptionEl = /** @type {HTMLTextAreaElement | null} */ (
    document.querySelector("#description-input")
  );

  if (!nameEl) return;

  // Scroll to builder
  document.querySelector("#build")?.scrollIntoView();

  // Make sure HTML is loaded
  if (!facet.html && facet.uri) {
    const html = await loadURI(facet.uri);
    const cid = await CID.create(0x55, new TextEncoder().encode(html));

    facet.html = html;
    facet.cid = cid;
  }

  $editingFacet.value = facet;
  nameEl.value = facet.name;

  if (descriptionEl) {
    descriptionEl.value = facet.description ?? "";
  }

  editor.dispatch({
    changes: { from: 0, to: editor.state.doc.length, insert: facet.html },
  });
}

/**
 * @param {Facet} facet
 */
async function saveFacet(facet) {
  const col = output.facets.collection();
  const colWithoutId = col.filter((c) => c.id !== facet.id);
  await output.facets.save([...colWithoutId, {
    ...facet,
    updatedAt: new Date().toISOString(),
  }]);
}
