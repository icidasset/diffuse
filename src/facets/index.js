import * as CID from "@atcute/cid";
import { html, render } from "lit-html";

import { basicSetup, EditorView } from "codemirror";
import { css as langCss } from "@codemirror/lang-css";
import { html as langHtml } from "@codemirror/lang-html";
import { javascript as langJs } from "@codemirror/lang-javascript";
import { autocompletion } from "@codemirror/autocomplete";

import foundation from "@common/facets/foundation.js";
import { effect } from "@common/signal.js";

/**
 * @import {Facet} from "@definitions/types.d.ts"
 */

////////////////////////////////////////////
// LIST
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
                <div class="button-row">
                  <a href="facets/l/?cid=${c.cid}" class="button">Open</a>
                  <button
                    style="background-color: var(--accent-twist-2);"
                    @click="${deleteFacet({
                      cid: c.cid,
                      name: c.name,
                    })}"
                  >
                    Delete
                  </button>
                  <!--<button style="background-color: var(--accent-twist-1);">Save</button>-->
                  <!--<button style="background-color: var(--accent-twist-2);">Fork</button>-->
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
 * @param {{ cid: string; name: string }} _
 */
function deleteFacet({ cid, name }) {
  return () => {
    const c = confirm("Are you sure you want to delete this facet?");
    if (!c) return;

    output.facets.save(
      output.facets.collection().filter((c) =>
        !(c.name === name && c.cid === cid)
      ),
    );
  };
}

////////////////////////////////////////////
// BUILD
////////////////////////////////////////////

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
  const facet = {
    $type: "sh.diffuse.output.facet",
    cid: CID.toString(cid),
    html,
    name,
  };

  switch (/** @type {any} */ (event).submitter.name) {
    case "load-example": {
      /** @type {HTMLSelectElement | null} */
      const selected = document.body.querySelector("#example-select");

      if (selected?.value) {
        const text = await fetch(selected.value).then((r) => r.text());

        editor.dispatch({
          changes: { from: 0, to: editor.state.doc.length, insert: text },
        });
      }
      break;
    }
    case "save":
      await saveFacet(facet);
      break;
    case "save+open":
      await saveFacet(facet);
      window.open(`./facets/l/?cid=${facet.cid}`, "blank");
      break;
  }
}

/**
 * @param {Facet} facet
 */
async function saveFacet(facet) {
  const col = output.facets.collection();
  const colWithoutName = col.filter((c) => c.name !== facet.name);

  await output.facets.save([...colWithoutName, facet]);
}
