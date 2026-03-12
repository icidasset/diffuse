import { basicSetup, EditorView } from "codemirror";
import { css as langCss } from "@codemirror/lang-css";
import { html as langHtml } from "@codemirror/lang-html";
import { javascript as langJs } from "@codemirror/lang-javascript";
import { autocompletion } from "@codemirror/autocomplete";

import * as TID from "@atcute/tid";

import * as CID from "~/common/cid.js";
import * as Output from "~/common/output.js";
import foundation from "~/common/facets/foundation.js";
import { facetFromURI } from "~/common/facets/utils.js";
import { loadURI } from "~/common/loader.js";
import { signal } from "~/common/signal.js";

import { saveFacet } from "./crud.js";

/**
 * @import {Facet} from "~/definitions/types.d.ts"
 */

const $editor = signal(/** @type {EditorView | null} */ (null));
const $editingFacet = signal(/** @type {Facet | null} */ (null));
const output = await foundation.orchestrator.output();

////////////////////////////////////////////
// EDITOR
////////////////////////////////////////////

export function renderEditor() {
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
  import foundation from "~/common/facets/foundation.js";
  import { effect } from "~/common/signal.js";

  const components = await foundation.features.fillQueueAutomatically();
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

  $editor.value = editor;
  return editor;
}

////////////////////////////////////////////
// FORM
////////////////////////////////////////////

/**
 * @param {EditorView} editor
 */
const onBuildSubmit = (editor) =>
/**
 * @param {Event} event
 */
async (event) => {
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
      id: TID.now(),
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
};

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

  // Reset url — remove `id` param if not matching the facet
  const url = new URL(location.href);
  const id = url.searchParams.get("id");

  if (id && facet.id !== id) {
    url.searchParams.delete("id");
    history.replaceState(null, "", url);
  }

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

  const editor = $editor.value;
  editor?.dispatch({
    changes: { from: 0, to: editor.state.doc.length, insert: facet.html },
  });
}

export function handleBuildFormSubmit() {
  const editor = $editor.value;
  if (!editor) return;

  document.querySelector("#build-form")?.addEventListener(
    "submit",
    onBuildSubmit(editor),
  );
}

////////////////////////////////////////////
// EDIT EXAMPLES
////////////////////////////////////////////

let isListening = false;

export function listenForExamplesEdit() {
  if (isListening) return;
  isListening = true;

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
        case "edit": {
          const facet = await facetFromURI({ name, uri }, { fetchHTML: true });
          editFacet(facet);
          document.querySelector("#build")?.scrollIntoView();
          break;
        }
      }
    },
  );
}

////////////////////////////////////////////
// EDIT FACET FROM URL
////////////////////////////////////////////

export async function editFacetFromURL() {
  const idParam = new URLSearchParams(location.search).get("id");

  if (idParam) {
    const col = await Output.data(output.facets);
    const facet = col.find((f) => f.id === idParam);
    if (facet) await editFacet(facet);
  }
}
