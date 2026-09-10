import { basicSetup, EditorView } from "codemirror";
import { css as langCss } from "@codemirror/lang-css";
import { html as langHtml } from "@codemirror/lang-html";
import { javascript as langJs } from "@codemirror/lang-javascript";
import { autocompletion } from "@codemirror/autocomplete";
import { StateEffect } from "@codemirror/state";

import * as TID from "@atcute/tid";

import * as Output from "~/common/output.js";
import { facetFromURI } from "~/common/facets/utils.js";
import { resolveFacetTile } from "~/common/loader.js";
import {
  tileFromFiles,
  tileResourceEntries,
} from "~/common/tiles.js";
import { signal } from "~/common/signal.js";

import { saveFacet } from "./crud.js";
import { output } from "./output.js";

/**
 * @import {Facet} from "~/definitions/types.d.ts"
 */

const $editor = signal(/** @type {EditorView | null} */ (null));
const $editingFacet = signal(/** @type {Facet | null} */ (null));

/** @type {{ path: string; content: string }[]} */
let files = [{ path: "/", content: "" }];
let activeIndex = 0;

// True once the user has modified the new-facet template in any way (typed in
// the editor, added/renamed/removed a tab, or imported a file). Used so we only
// swap in the default template when the kind changes before any editing happens.
let startedEditing = false;

// Guards `setEditorContent` so the doc-change update listener can tell apart
// programmatic content swaps from genuine user edits.
let isProgrammaticSet = false;

const fileDecoder = new TextDecoder();

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
// EDITOR — TABS + SINGLE CodeMirror VIEW
////////////////////////////////////////////

const DEFAULT_INTERFACE = [{ path: "/", content: `
<!-- Absolute URLs = Used to reference files of this facet -->
<!-- Relative URLs = Relative to root of current version of Diffuse -->

<style>
  @import "./styles/base.css"; /* Import Diffuse base styles */
  @import "/example.css";
</style>

<div id="placeholder"></div>

<script src="/example.js" type="module"></script>
  `.trim() }, {
  path: "/example.js",
  content: `
import foundation from "~/common/foundation.js"; // This is also a relative URL, "~/" equals "./" (see importmap in html)
import { effect } from "~/common/signal.js";

// Set document title
foundation.setup({ title: "Example" });

// Show what's currently playing
const output = await foundation.orchestrator.output();
const queue = await foundation.engine.queue();

effect(() => {
  const now = queue.now();
  const tracks = output.tracks.collection();
  const currentlyPlaying = now && tracks.state === "loaded"
    ? tracks.data.find((t) => t.id === now.id)
    : undefined;

  const el = foundation.container().querySelector("#placeholder");

  if (currentlyPlaying) {
    el.innerHTML = (
      (currentlyPlaying?.tags?.artist ?? "Unknown artist") + " - " +
      (currentlyPlaying?.tags?.title ?? "Unknown title")
    );
    el.classList.remove("is-faded");
  } else if (tracks.state === "loading") {
    el.innerHTML = "Loading ...";
    el.classList.add("is-faded");
  } else {
    el.innerHTML = "😶‍🌫️";
    el.classList.add("is-faded");
  }
});

// Indicate interface is loaded and ready to use
// (this hides the default loading animation)
foundation.ready();
  `.trim(),
}, {
  path: "/example.css",
  content: `
#placeholder {
  align-items: center;
  display: flex;
  font-style: italic;
  height: 100dvh;
  justify-content: center;
}

.is-faded {
  opacity: 0.4;
}
  `.trim(),
}];

const DEFAULT_PRELUDE = [{ path: "/", content: `
<script src="/example.js" type="module"></script>
  `.trim() }, {
  path: "/example.js",
  content: `
import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

effect(() => {
  // When an interface includes the audio engine in addition to this feature facet, call 'setup'
  const audio = foundation.signals.engine.audio();
  if (audio) setup(audio);
});

const STORAGE_KEY = "example/resume";
const RESTORED = new WeakMap();

/**
 * Remember the active track's position and restore it the next time it plays.
 */
function setup(audio) {
  const saved = JSON.parse(localStorage.getItem(STORAGE_KEY) ?? "{}");

  effect(() => {
    const item = audio.items().find((i) => !i.isPreload);
    const st = item && audio.state(item.id);
    if (!st) return;

    // Restore once, when the active track is ready to play.
    if (st.loadingState() === "loaded" && !RESTORED.has(item)) {
      RESTORED.set(item, true);
      const seconds = saved[item.id];
      if (seconds > 5) audio.seek({ audioId: item.id, currentTime: seconds });
    }

    // Persist progress; forget finished tracks so they restart next time.
    const duration = st.duration();
    if (duration > 0) {
      if (st.currentTime() < 0.98 * duration) {
        saved[item.id] = st.currentTime();
      } else {
        delete saved[item.id];
      }

      localStorage.setItem(STORAGE_KEY, JSON.stringify(saved));
    }
  });
}
  `.trim(),
}];

/**
 * The built-in examples shown on the Create page. Each one corresponds to a
 * default template in this file (DEFAULT_INTERFACE / DEFAULT_PRELUDE) that the
 * editor can start from when clicked.
 */
export const DEFAULT_EXAMPLES = [
  {
    title: "Interface starter template",
    kind: "interactive",
    desc: "The default interactive facet example. It shows what's currently playing, or a placeholder if nothing's in the queue yet.",
  },
  {
    title: "Feature starter template",
    kind: "prelude",
    desc: "The default feature facet example. Shows how to watch for the creation of a particular foundation element and then do something when that happens.",
  },
];

/**
 * Renders the built-in default examples into the `#examples-list` container on
 * the Create page.
 */
export function renderDefaultExamples() {
  const container = /** @type {HTMLElement | null} */ (
    document.getElementById("examples-list")
  );
  if (!container) return;

  const ul = document.createElement("ul");
  for (const example of DEFAULT_EXAMPLES) {
    const li = document.createElement("li");
    li.setAttribute("data-kind", example.kind);
    li.style.marginTop = "var(--space-md)";

    const head = document.createElement("div");
    head.style.cssText =
      "display: flex; gap: var(--space-xs); justify-content: space-between;";

    const title = document.createElement("span");
    title.textContent = example.title;

    const edit = document.createElement("button");
    edit.className = "button--tiny button--bg-accent";
    edit.setAttribute("rel", "edit");
    edit.innerHTML =
      '<span class="with-icon"><i class="ph-fill ph-code-block"></i> Edit</span>';

    head.append(title, edit);

    const desc = document.createElement("div");
    desc.className = "list-description";
    desc.textContent = example.desc;

    li.append(head, desc);
    ul.appendChild(li);
  }
  container.replaceChildren(ul);
}

/**
 * @param {string} path
 */
function basename(path) {
  if (path === "/") return "index.html";
  const trimmed = path.replace(/\/+$/, "");
  const idx = trimmed.lastIndexOf("/");
  return idx >= 0 ? trimmed.slice(idx + 1) : trimmed;
}

/**
 * Is this path reserved as the tile's index document (`index.html`), which the
 * `/` root tab already represents? Such paths cannot be created or renamed to.
 *
 * @param {string} path
 * @returns {boolean}
 */
function isReservedIndexPath(path) {
  return /^\/?index\.html?$/i.test(path.trim());
}

/**
 * @param {string} path
 */
function languageForPath(path) {
  if (path === "/" || /\.html?$/i.test(path)) return langHtml();
  if (/\.css$/i.test(path)) return langCss();
  // js, mjs, json, etc.
  return langJs();
}

/**
 * The base extensions shared by every tab.
 */
const markEditedExtension = EditorView.updateListener.of((update) => {
  // A user-typed change marks the template as edited, but never a programmatic
  // `setEditorContent` swap. Kept in `baseExtensions` so it survives every
  // `StateEffect.reconfigure` (when switching tabs or the facet kind).
  if (update.docChanged && !isProgrammaticSet) startedEditing = true;
});
const baseExtensions = [basicSetup, autocompletion(), markEditedExtension];

/** @returns {string} */
function currentContent() {
  return $editor.value?.state.doc.toString() ?? "";
}

/** @param {string} content */
function setEditorContent(content) {
  isProgrammaticSet = true;
  $editor.value?.dispatch({
    changes: { from: 0, to: $editor.value.state.doc.length, insert: content },
  });
  isProgrammaticSet = false;
}

/**
 * Persists the active tab's current doc into `files`, then switches to `index`.
 *
 * @param {number} index
 */
function activateTab(index) {
  const editor = $editor.value;
  if (!editor) return;
  if (index < 0 || index >= files.length) return;
  if (index === activeIndex) return;

  files[activeIndex].content = editor.state.doc.toString();
  activeIndex = index;

  // Swap language extension + content for the new tab.
  editor.dispatch({
    effects: StateEffect.reconfigure.of([
      ...baseExtensions,
      languageForPath(files[activeIndex].path),
    ]),
  });
  setEditorContent(files[activeIndex].content);

  renderTabs();
}

/**
 * Renames the file at `index` to `newPath`, adjusting the editor's language
 * if the extension changes. The `/` index cannot be renamed.
 *
 * @param {number} index
 * @param {string} newPath
 */
function renameFile(index, newPath) {
  if (index === 0) return;
  const trimmed = newPath.trim();
  if (!trimmed) return;
  const path = trimmed.startsWith("/") ? trimmed : `/${trimmed}`;
  if (path === files[index].path) return;
  if (isReservedIndexPath(path)) return;
  if (files.some((f, i) => i !== index && f.path === path)) return;

  const langChanged = languageForPath(path) !== languageForPath(files[index].path);
  const renamedPath = path;
  files[index].path = path;
  startedEditing = true;

  // Keep `/` first, then alphabetical, and track the renamed file.
  files.sort((a, b) => (a.path === "/" ? -1 : b.path === "/" ? 1 : a.path.localeCompare(b.path)));
  activeIndex = files.findIndex((f) => f.path === renamedPath);
  if (activeIndex < 0) activeIndex = 0;

  if (langChanged) {
    $editor.value?.dispatch({
      effects: StateEffect.reconfigure.of([
        ...baseExtensions,
        languageForPath(files[activeIndex].path),
      ]),
    });
  }
  renderTabs();
}

/**
 * Adds a new file to the tile. Opens a modal dialog (styled like other facet
 * dialogs) where the file path, e.g. <code>/style.css</code>, can be entered.
 */
function addFile() {
  let dialog = /** @type {HTMLDialogElement | null} */ (
    document.getElementById("add-file-dialog")
  );

  if (!dialog) {
    dialog = /** @type {HTMLDialogElement} */ (
      document.createElement("dialog")
    );
    dialog.id = "add-file-dialog";
    dialog.style.cssText =
      "padding: 0; position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); margin: 0;";
    dialog.innerHTML = `
      <form id="add-file-form">
        <div class="dialog-header">
          <strong>Add a file to the tile</strong>
        </div>
        <div class="dialog-body">
          <p style="font-size: var(--fs-sm); margin: 0">
            Path of the new file, e.g. <code>/style.css</code>. The language is
            chosen from the extension.
          </p>
          <div>
            <label for="add-file-path">Path</label>
            <input id="add-file-path" type="text" placeholder="/file.js" required autocomplete="off" />
          </div>
        </div>
        <div class="dialog-footer" style="justify-content: flex-end">
          <button type="button" id="add-file-cancel">Cancel</button>
          <button type="submit" class="button--bg-accent">Add file</button>
        </div>
      </form>
    `;
    document.body.appendChild(dialog);

    dialog.querySelector("#add-file-cancel")?.addEventListener("click", () => {
      /** @type {HTMLDialogElement} */ (dialog).close();
    });

    dialog.querySelector("#add-file-form")?.addEventListener("submit", (e) => {
      e.preventDefault();
      const input = /** @type {HTMLInputElement | null} */ (
        dialog?.querySelector("#add-file-path")
      );
      const existing = new Set(files.map((f) => f.path));
      let path = input?.value.trim() ?? "";
      if (!path.startsWith("/")) path = `/${path}`;
      if (!path || path === "/" || isReservedIndexPath(path) || existing.has(path)) return;
      files.push({ path, content: "" });
      /** @type {HTMLDialogElement} */ (dialog).close();
      startedEditing = true;
      activateTab(files.length - 1);
    });
  }

  const pathInput = /** @type {HTMLInputElement | null} */ (
    dialog.querySelector("#add-file-path")
  );
  if (pathInput) pathInput.value = "";

  dialog.showModal();
  pathInput?.focus();
}

/**
 * Removes the tab at `index`. The `/` (index) tab cannot be removed.
 *
 * @param {number} index
 */
function removeFile(index) {
  if (index === 0 || index >= files.length) return;
  const editor = $editor.value;
  if (!editor) return;
  files[activeIndex].content = editor.state.doc.toString();
  files.splice(index, 1);
  if (index <= activeIndex) activeIndex = Math.max(0, activeIndex - 1);
  startedEditing = true;
  setEditorContent(files[activeIndex].content);
  renderTabs();
}

/**
 * Renders the tab bar (<code>#editor-tabs</code>) from the current `files`.
 */
function renderTabs() {
  const tabsEl = document.getElementById("editor-tabs");
  if (!tabsEl) return;

  tabsEl.textContent = "";

  // Add button first, left of the file tabs, sized to match the editor gutter.
  const add = document.createElement("button");
  add.type = "button";
  add.className = "editor-tab editor-tab--add";
  add.title = "Add file";
  add.innerHTML = '<i class="ph-bold ph-plus editor-tab__icon"></i>';
  add.addEventListener("click", addFile);
  /**
   * Sizes the add button to the editor gutter width. If the gutter isn't laid
   * out yet (width is 0), re-measure on the next animation frame until it is.
   */
  const sizeAddButton = () => {
    const gutter = /** @type {Element | null} */ (
      document.querySelector("#html-input-container .cm-gutters")
    );
    if (!gutter) return;
    const width = gutter.getBoundingClientRect().width;
    if (width > 0) {
      add.style.width = `${width}px`;
    } else {
      requestAnimationFrame(sizeAddButton);
    }
  };
  sizeAddButton();
  tabsEl.append(add);

  files.forEach((file, index) => {
    try {
      const tab = document.createElement("div");
      tab.role = "button";
      tab.tabIndex = 0;
      tab.className = "editor-tab" + (index === activeIndex ? " is-active" : "");
      tab.title = file.path;
      tab.addEventListener("click", () => activateTab(index));
      tab.addEventListener("keydown", (e) => {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          activateTab(index);
        }
      });

      const label = document.createElement("span");
      label.textContent = basename(file.path);
      label.title = "Double-click to rename";
      label.addEventListener("dblclick", (e) => {
        e.stopPropagation();
        const input = document.createElement("input");
        input.type = "text";
        input.value = file.path;
        input.className = "editor-tab__rename editor-tab__rename--inline";
        label.replaceWith(input);
        input.focus();
        input.select();

        const commit = () => {
          if (done) return;
          done = true;
          renameFile(index, input.value);
          if (input.isConnected) input.remove();
          renderTabs();
        };
        let done = false;
        input.addEventListener("keydown", (e) => {
          if (e.key === "Enter") {
            e.preventDefault();
            commit();
          } else if (e.key === "Escape") {
            done = true;
            if (input.isConnected) input.remove();
            renderTabs();
          }
        });
        input.addEventListener("blur", commit);
      });
      tab.append(label);

      if (index !== 0) {
        const close = document.createElement("button");
        close.type = "button";
        close.className = "editor-tab__close";
        close.title = "Remove file";
        close.innerHTML = '<i class="ph-bold ph-x editor-tab__icon"></i>';
        close.addEventListener("click", (e) => {
          e.stopPropagation();
          removeFile(index);
        });
        tab.append(close);
      }

      tabsEl.append(tab);
    } catch (err) {
      console.error("editor: failed to build tab", file.path, err);
    }
  });
}

/**
 * Loads a facet's files into the editor tabs. Inline tiles (with a `resources`
 * map + `blocks`) load every resource into its own tab; otherwise the `/` index
 * (resolved via `resolveFacetHTML`) is shown as a single tab.
 *
 * @param {Facet} facet
 */
async function loadFacetFiles(facet) {
  // Resolve the facet's tile (inline `resources`+`blocks`, or a `.tile` CAR
  // referenced by `uri`), then load every absolute resource into its own tab.
  const tile = await resolveFacetTile(facet);
  const resources = tile?.resources;
  const blocks = tile?.blocks;

  let loaded;
  if (resources && Object.keys(resources).length && blocks) {
    const entries = tileResourceEntries(resources, blocks);
    loaded = [...entries.entries()].map(([path, entry]) => ({
      path,
      content: fileDecoder.decode(entry.bytes),
    }));
    if (!loaded.length) loaded = [{ path: "/", content: tile.html }];
  } else {
    loaded = [{ path: "/", content: tile?.html ?? "" }];
  }
  files = loaded;

  activeIndex = 0;
  // Ensure the `/` index is first.
  files.sort((a, b) => (a.path === "/" ? -1 : b.path === "/" ? 1 : a.path.localeCompare(b.path)));
  activeIndex = files.findIndex((f) => f.path === "/");
  if (activeIndex < 0) activeIndex = 0;

  $editor.value?.dispatch({
    effects: StateEffect.reconfigure.of([
      ...baseExtensions,
      languageForPath(files[activeIndex].path),
    ]),
  });
  setEditorContent(files[activeIndex].content);
  renderTabs();
}

/**
 * Replaces the new-facet editor's tabs with the default template for `kind`.
 * Only called when creating a brand-new facet that hasn't been edited yet.
 *
 * @param {"interactive" | "prelude"} kind
 */
function applyDefaultTemplateForKind(kind) {
  const defaults = kind === "prelude" ? DEFAULT_PRELUDE : DEFAULT_INTERFACE;
  files = defaults.map((f) => ({ ...f }));
  activeIndex = files.findIndex((f) => f.path === "/");
  if (activeIndex < 0) activeIndex = 0;

  $editor.value?.dispatch({
    effects: StateEffect.reconfigure.of([
      ...baseExtensions,
      languageForPath(files[activeIndex].path),
    ]),
  });
  setEditorContent(files[activeIndex].content);
  startedEditing = false;
  renderTabs();
}

/**
 * Loads a built-in default template (interactive or prelude) into the editor
 * as a brand-new facet, e.g. when a default example is clicked.
 *
 * @param {"interactive" | "prelude"} kind
 */
function loadDefaultExample(kind) {
  $editingFacet.value = null;
  const kindEl = /** @type {HTMLSelectElement | null} */ (
    document.querySelector("#kind-input")
  );
  if (kindEl) kindEl.value = kind;
  applyDefaultTemplateForKind(kind);
  globalThis.scrollTo({ top: 0 });
}

export function renderEditor() {
  // Code editor
  const editorContainer = document.body.querySelector("#html-input-container");
  if (!editorContainer) throw new Error("Editor container not found");

  // Tab bar rendered just above the editor.
  const tabsEl = document.createElement("div");
  tabsEl.id = "editor-tabs";
  tabsEl.className = "editor-tabs";
  editorContainer.before(tabsEl);

  const editor = new EditorView({
    parent: editorContainer,
    doc: "",
    extensions: [...baseExtensions, languageForPath("/")],
  });

  $editor.value = editor;

  files = DEFAULT_INTERFACE.map((f) => ({ ...f }));
  activeIndex = 0;
  startedEditing = false;
  setEditorContent(files[0].content);
  renderTabs();
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

  // Persist the active tab's current doc before collecting files.
  files[activeIndex].content = editor.state.doc.toString();

  /** @type {Record<string, string>} */
  const fileMap = {};
  for (const file of files) fileMap[file.path] = file.content;
  const tile = tileFromFiles(fileMap);

  const name = nameEl?.value ?? "nameless";
  const description = descriptionEl?.value ?? "";
  const kind =
    /** @type {"interactive" | "prelude"} */ (kindEl?.value ?? "interactive");

  /** @type {Facet} */
  const facet = $editingFacet.value
    ? {
      ...$editingFacet.value,
      blocks: tile.blocks,
      description,
      kind,
      name,
      resources: tile.resources,
    }
    : {
      $type: "sh.diffuse.output.facet",
      id: TID.now(),
      blocks: tile.blocks,
      description,
      kind,
      name,
      resources: tile.resources,
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

  // Load the facet's files into the editor tabs. `loadFacetFiles` handles
  // inline tiles (each resource becomes a tab) and plain HTML/URI facets (a
  // single index tab).
  setEditorLoading(true);
  await loadFacetFiles(facet);
  setEditorLoading(false);

  $editingFacet.value = facet;
  nameEl.value = facet.name;

  if (kindEl) {
    kindEl.value = facet.kind ?? "interactive";
  }

  if (descriptionEl) {
    descriptionEl.value = facet.description ?? "";
  }
}

export function handleBuildFormSubmit() {
  const editor = $editor.value;
  if (!editor) return;

  document.querySelector("#code-form")?.addEventListener(
    "submit",
    onBuildSubmit(editor),
  );

  const kindEl = /** @type {HTMLSelectElement | null} */ (
    document.querySelector("#kind-input")
  );
  // When creating a brand-new facet, switching the kind swaps in that kind's
  // default template — but only if the user hasn't started editing yet, and
  // never while editing an existing facet.
  kindEl?.addEventListener("change", () => {
    if ($editingFacet.value) return;
    if (startedEditing) return;
    applyDefaultTemplateForKind(
      /** @type {"interactive" | "prelude"} */ (kindEl?.value ?? "interactive"),
    );
  });

  const importBtn = document.querySelector("#import-button");
  const importInput = document.querySelector("#import-input");

  importBtn?.addEventListener("click", () => /** @type {HTMLElement} */ (importInput)?.click());

  importInput?.addEventListener("change", async (event) => {
    const file = /** @type {HTMLInputElement} */ (event.target).files?.[0];
    if (!file) return;

    const content = await file.text();
    files[activeIndex].content = content;
    startedEditing = true;
    setEditorContent(content);
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

      const li = target.closest("li");
      if (!li) return;

      // Built-in default template example — no source URI, just load the
      // matching default from this page into the editor.
      if (rel === "edit" && li.dataset.kind && !li.dataset.uri) {
        loadDefaultExample(
          /** @type {"interactive" | "prelude"} */ (li.dataset.kind),
        );
        return;
      }

      const uri = li.getAttribute("data-uri");
      if (!uri) return;

      const name = li.getAttribute("data-name");
      if (!name) return;

      const kind = li.getAttribute("data-kind") ?? undefined;

      if (rel === "edit") {
        setEditorLoading(true);
        const facet = await facetFromURI({ kind, name, uri }, {
          fetchHTML: true,
        });
        setEditorLoading(false);
        editFacet(facet);
        globalThis.scrollTo({ top: 0 });
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

  // Reset editing state; if we're loading a specific facet, editFacet() will
  // set it again. Without this, $editingFacet persists across SPA navigations
  // and a subsequent "save" will reuse the stale id, silently overwriting the
  // previously edited facet instead of creating a new entry.
  $editingFacet.value = null;

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
