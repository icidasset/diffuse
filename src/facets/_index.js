import { basicSetup, EditorView } from "codemirror";
import { css as langCss } from "@codemirror/lang-css";
import { html as langHtml } from "@codemirror/lang-html";
import { javascript as langJs } from "@codemirror/lang-javascript";
import { autocompletion } from "@codemirror/autocomplete";

import * as TID from "@atcute/tid";

import * as CID from "~/common/cid.js";
import foundation from "~/common/facets/foundation.js";
import { signal } from "~/common/signal.js";
import { loadURI } from "~/common/loader.js";

/**
 * @import {Facet} from "~/definitions/types.d.ts"
 */

////////////////////////////////////////////
// BUILD
////////////////////////////////////////////

const output = foundation.orchestrator.output();
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
  import foundation from "@diffuse/foundation";
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
