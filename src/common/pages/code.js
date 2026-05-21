import { basicSetup, EditorView } from "codemirror";
import { css as langCss } from "@codemirror/lang-css";
import { html as langHtml } from "@codemirror/lang-html";
import { javascript as langJs } from "@codemirror/lang-javascript";
import { autocompletion } from "@codemirror/autocomplete";

import * as TID from "@atcute/tid";

import * as CID from "~/common/cid.js";
import * as Output from "~/common/output.js";
import { facetFromURI } from "~/common/facets/utils.js";
import { loadURI } from "~/common/loader.js";
import { signal } from "~/common/signal.js";

import { saveFacet } from "./crud.js";
import { output } from "./output.js";

/**
 * @import {Facet} from "~/definitions/types.d.ts"
 */

const $editor = signal(/** @type {EditorView | null} */ (null));
const $editingFacet = signal(/** @type {Facet | null} */ (null));

////////////////////////////////////////////
// LOADING
////////////////////////////////////////////

const LOADING_EL_ID = "editor-loading";

/**
 * @param {boolean} loading
 */
function setEditorLoading(loading) {
  const container = /** @type {HTMLElement | null} */ (
    document.querySelector("#html-input-container")
  );
  if (!container) return;

  if (loading) {
    if (document.getElementById(LOADING_EL_ID)) return;
    const el = document.createElement("div");
    el.id = LOADING_EL_ID;
    el.className = "with-icon";
    el.style.fontSize = "var(--fs-sm)";
    el.innerHTML = '<i class="ph-bold ph-spinner animate-spin"></i> Loading…';
    container.before(el);
    container.hidden = true;
  } else {
    document.getElementById(LOADING_EL_ID)?.remove();
    container.hidden = false;
  }
}

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
<style>
  @import "./styles/base.css";
</style>

<script type="module">
  import foundation from "~/common/foundation.js";
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

  const kindEl = /** @type {HTMLSelectElement | null} */ (
    document.querySelector("#kind-input")
  );

  const html = editor.state.doc.toString();
  const cid = await CID.create(0x55, new TextEncoder().encode(html));
  const name = nameEl?.value ?? "nameless";
  const description = descriptionEl?.value ?? "";
  const kind =
    /** @type {"interactive" | "prelude"} */ (kindEl?.value ?? "interactive");

  /** @type {Facet} */
  const facet = $editingFacet.value
    ? {
      ...$editingFacet.value,
      cid,
      description,
      html,
      kind,
      name,
    }
    : {
      $type: "sh.diffuse.output.facet",
      id: TID.now(),
      cid,
      description,
      html,
      kind,
      name,
    };

  $editingFacet.value = facet;

  switch (/** @type {any} */ (event).submitter.name) {
    case "save":
      await saveFacet(facet);
      break;
    case "save+open":
      await saveFacet(facet);
      globalThis.open(`./l/?id=${facet.id}`, "blank");
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

  const kindEl = /** @type {HTMLSelectElement | null} */ (
    document.querySelector("#kind-input")
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
  document.querySelector("#code")?.scrollIntoView();

  // Make sure HTML is loaded
  if (!facet.html && facet.uri) {
    setEditorLoading(true);
    const html = await loadURI(facet.uri);
    const cid = await CID.create(0x55, new TextEncoder().encode(html));
    setEditorLoading(false);

    facet.html = html;
    facet.cid = cid;
  }

  $editingFacet.value = facet;
  nameEl.value = facet.name;

  if (kindEl) {
    kindEl.value = facet.kind ?? "interactive";
  }

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

  document.querySelector("#code-form")?.addEventListener(
    "submit",
    onBuildSubmit(editor),
  );

  const importBtn = document.querySelector("#import-button");
  const importInput = document.querySelector("#import-input");

  importBtn?.addEventListener("click", () => importInput?.click());

  importInput?.addEventListener("change", async (event) => {
    const file = /** @type {HTMLInputElement} */ (event.target).files?.[0];
    if (!file) return;

    const html = await file.text();
    const cid = await CID.create(0x55, new TextEncoder().encode(html));

    editor.dispatch({
      changes: { from: 0, to: editor.state.doc.length, insert: html },
    });
  });
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

      const kind = target.closest("li")?.getAttribute("data-kind") ?? undefined;

      switch (rel) {
        case "edit": {
          setEditorLoading(true);
          const facet = await facetFromURI({ kind, name, uri }, {
            fetchHTML: true,
          });
          setEditorLoading(false);
          editFacet(facet);
          document.querySelector("#code")?.scrollIntoView();
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
  const params = new URLSearchParams(location.search);
  const idParam = params.get("id");
  const uriParam = params.get("uri");

  setEditorLoading(true);
  try {
    if (idParam) {
      const out = await output();
      const col = await Output.data(out.facets);
      const facet = col.find((f) => f.id === idParam);
      if (facet) await editFacet(facet);
    } else if (uriParam) {
      const facet = await facetFromURI({
        uri: uriParam,
        name: params.get("name") ?? "",
        kind: /** @type {any} */ (params.get("kind") ?? undefined),
        description: params.get("description") ?? undefined,
      }, { fetchHTML: true });
      await editFacet(facet);
    }
  } finally {
    setEditorLoading(false);
  }
}
