import * as CID from "@atcute/cid";
import { Temporal } from "@js-temporal/polyfill";
import { html, render } from "lit-html";

import { basicSetup, EditorView } from "codemirror";
import { css as langCss } from "@codemirror/lang-css";
import { html as langHtml } from "@codemirror/lang-html";
import { javascript as langJs } from "@codemirror/lang-javascript";
import { autocompletion } from "@codemirror/autocomplete";

import foundation from "@common/facets/foundation.js";
import { effect, signal } from "@common/signal.js";
import { facetFromUrl } from "@common/facets/utils.js";

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

    const url = target.closest("li")?.getAttribute("data-url");
    if (!url) return;

    const name = target.closest("li")?.getAttribute("data-name");
    if (!name) return;

    switch (rel) {
      case "fork": {
        const facet = await facetFromUrl({ name, url }, { fetchHTML: true });
        editFacet(facet);
        document.querySelector("#build")?.scrollIntoView();
        break;
      }
      case "save": {
        const facet = await facetFromUrl({ name, url }, { fetchHTML: false });
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

effect(() => {
  const col = output.facets.collection().sort((a, b) => {
    return a.name.toLocaleLowerCase().localeCompare(b.name.toLocaleLowerCase());
  });

  const h = col.length
    ? html`
      <ul>
        ${col.map((c) =>
          html`
            <li style="margin-bottom: var(--space-2xs)">
              <span>${c.name}</span>
              <div class="list-description">
                <div style="margin-bottom: var(--space-2xs)">
                  ${c.url && !c.html
                    ? html`
                      <span class="with-icon">
                        <i class="ph-fill ph-binoculars"></i>
                        <span>Tracking the original <a href="${c
                          .url}">URL</a></span>
                      </span>
                    `
                    : html`
                      <span class="with-icon">
                        <i class="ph-fill ph-code"></i>
                        <span>Custom code</span>
                      </span>
                    `}
                </div>
                <div class="button-row">
                  <a href="facets/l/?id=${c.id}" class="button">Open</a>
                  <button
                    style="background-color: var(--accent-twist-4);"
                    @click="${() => editFacet(c)}"
                  >
                    Edit
                  </button>
                  <button
                    style="background-color: var(--accent-twist-2);"
                    @click="${deleteFacet({
                      id: c.id,
                    })}"
                  >
                    Delete
                  </button>
                </div>
              </div>
            </li>
          `
        )}
      </ul>
    `
    : output.facets.state() === "loaded"
    ? emptyFacetsList
    : html`
      <i class="ph-bold ph-spinner-gap"></i>
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
    const currentlyPlaying = components.engine.queue.now();
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

  const html = editor.state.doc.toString();
  const cid = await CID.create(0x55, new TextEncoder().encode(html));
  const name = nameEl?.value ?? "nameless";

  /** @type {Facet} */
  const facet = $editingFacet.value
    ? {
      ...$editingFacet.value,
      cid: CID.toString(cid),
      html,
      name,
    }
    : {
      $type: "sh.diffuse.output.facet",
      id: crypto.randomUUID(),
      cid: CID.toString(cid),
      html,
      name,
    };

  switch (/** @type {any} */ (event).submitter.name) {
    case "save":
      await saveFacet(facet);
      break;
    case "save+open":
      await saveFacet(facet);
      globalThis.open(`./facets/l/?cid=${facet.cid}`, "blank");
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

  if (!nameEl) return;

  // Make sure HTML is loaded
  if (!facet.html && facet.url) {
    const html = await fetch(facet.url).then((res) => res.text());
    const cid = await CID.create(0x55, new TextEncoder().encode(html));

    facet.html = html;
    facet.cid = CID.toString(cid);
  }

  $editingFacet.value = facet;
  nameEl.value = facet.name;

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
  const timestamp = Temporal.Now.zonedDateTimeISO().toString();

  await output.facets.save([...colWithoutId, {
    ...facet,
    updatedAt: timestamp,
  }]);
}
