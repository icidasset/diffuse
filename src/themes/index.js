import { Temporal } from "@js-temporal/polyfill";
import { html, render } from "lit-html";
import { keyed } from "lit-html/directives/keyed.js";

import { basicSetup, EditorView } from "codemirror";
import { css as langCss } from "@codemirror/lang-css";
import { html as langHtml } from "@codemirror/lang-html";
import { javascript as langJs } from "@codemirror/lang-javascript";
import { autocompletion } from "@codemirror/autocomplete";

import * as CID from "@common/cid.js";
import foundation from "@common/facets/foundation.js";
import { effect, signal } from "@common/signal.js";
import { themeFromUrl } from "@common/themes/utils.js";

/**
 * @import {Theme} from "@definitions/types.d.ts"
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
        const theme = await themeFromUrl({ name, url }, { fetchHTML: true });
        editTheme(theme);
        document.querySelector("#build")?.scrollIntoView();
        break;
      }
      case "save": {
        const theme = await themeFromUrl({ name, url }, { fetchHTML: false });
        const out = foundation.orchestrator.output();

        out.themes.save([
          ...out.themes.collection(),
          theme,
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

listEl.innerHTML = "";

const output = foundation.orchestrator.output();

effect(() => {
  const col = output.themes.collection().sort((a, b) => {
    return a.name.toLocaleLowerCase().localeCompare(b.name.toLocaleLowerCase());
  });

  const state = output.themes.state();

  const h = col.length && state === "loaded"
    ? html`
      <ul>
        ${col.map((c) =>
          keyed(
            c.id,
            html`
              <li>
                <div style="position: relative;">
                  <a href="themes/l/?id=${c.id}">
                    ${c.name}
                  </a>
                  <button
                    class="button--fixed button--transparent"
                    popovertarget="theme-menu-col-${c.id}"
                    style="anchor-name: --theme-anchor-col-${c
                      .id}; position: absolute; right: 0; top: 50%; transform: translateY(-50%);"
                  >
                    <i class="ph-fill ph-dots-three-circle"></i>
                  </button>
                </div>
                <div class="list-description">
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

                <!-- Dropdown Menu -->
                <div
                  id="theme-menu-col-${c.id}"
                  class="dropdown"
                  style="position-anchor: --theme-anchor-col-${c.id}"
                  popover
                >
                  <a href="themes/l/?id=${c.id}">
                    <span class="with-icon">
                      <i class="ph-fill ph-globe"></i> Open
                    </span>
                  </a>
                  <a @click="${() => editTheme(c)}">
                    <span class="with-icon">
                      <i class="ph-fill ph-cursor-text"></i> Edit
                    </span>
                  </a>
                  <a @click="${deleteTheme({ id: c.id })}">
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
    ? emptyThemesList
    : html`
      <div class="with-icon" style="font-size: var(--fs-sm);">
        <i class="ph-bold ph-spinner-gap"></i>
        Loading items
      </div>
    `;

  render(h, listEl);
});

const emptyThemesList = html`
  <p style="margin-bottom: 0;">
    <i class="ph-fill ph-info"></i> You have not saved any themes yet.
  </p>
`;

/**
 * @param {{ id: string }} _
 */
function deleteTheme({ id }) {
  return () => {
    const c = confirm("Are you sure you want to delete this theme?");
    if (!c) return;

    output.themes.save(
      output.themes.collection().filter((c) => !(c.id === id)),
    );
  };
}

////////////////////////////////////////////
// BUILD
////////////////////////////////////////////

const $editingTheme = signal(/** @type {Theme | null} */ (null));

// Code editor
const editorContainer = document.body.querySelector("#html-input-container");
if (!editorContainer) throw new Error("Editor container not found");

const editor = new EditorView({
  parent: editorContainer,
  doc: ``.trim(),
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

  /** @type {Theme} */
  const theme = $editingTheme.value
    ? {
      ...$editingTheme.value,
      cid,
      html,
      name,
    }
    : {
      $type: "sh.diffuse.output.theme",
      id: crypto.randomUUID(),
      cid,
      html,
      name,
    };

  switch (/** @type {any} */ (event).submitter.name) {
    case "save":
      await saveTheme(theme);
      break;
    case "save+open":
      await saveTheme(theme);
      globalThis.open(`./themes/l/?id=${theme.id}`, "blank");
      break;
  }
}

/**
 * @param {Theme} ogTheme
 */
async function editTheme(ogTheme) {
  const theme = { ...ogTheme };
  const nameEl = /** @type {HTMLInputElement | null} */ (document.querySelector(
    "#name-input",
  ));

  if (!nameEl) return;

  // Make sure HTML is loaded
  if (!theme.html && theme.url) {
    const html = await fetch(theme.url).then((res) => res.text());
    const cid = await CID.create(0x55, new TextEncoder().encode(html));

    theme.html = html;
    theme.cid = cid;
  }

  $editingTheme.value = theme;
  nameEl.value = theme.name;

  editor.dispatch({
    changes: { from: 0, to: editor.state.doc.length, insert: theme.html },
  });
}

/**
 * @param {Theme} theme
 */
async function saveTheme(theme) {
  const col = output.themes.collection();
  const colWithoutId = col.filter((c) => c.id !== theme.id);
  const timestamp = Temporal.Now.zonedDateTimeISO().toString();

  await output.themes.save([...colWithoutId, {
    ...theme,
    updatedAt: timestamp,
  }]);
}
