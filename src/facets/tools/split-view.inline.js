import "@awesome.me/webawesome/dist/components/split-panel/split-panel.js";
import "@awesome.me/webawesome/dist/components/dialog/dialog.js";
import "@awesome.me/webawesome/dist/components/button/button.js";
import "@awesome.me/webawesome/dist/components/input/input.js";
import "@awesome.me/webawesome/dist/components/icon/icon.js";
import "@awesome.me/webawesome/dist/components/select/select.js";
import "@awesome.me/webawesome/dist/components/option/option.js";

/**
 * @import { default as WaSplitPanel } from "@awesome.me/webawesome/dist/components/split-panel/split-panel.js"
 * @import { default as WaDialog } from "@awesome.me/webawesome/dist/components/dialog/dialog.js"
 * @import { default as WaInput } from "@awesome.me/webawesome/dist/components/input/input.js"
 * @import { default as WaIcon } from "@awesome.me/webawesome/dist/components/icon/icon.js"
 * @import { default as WaSelect } from "@awesome.me/webawesome/dist/components/select/select.js"
 */

/**
 * @typedef {{ type: "pane", facet: string | null }} PaneNode
 * @typedef {{ type: "split", orientation: "horizontal" | "vertical", position: number, start: Node, end: Node }} SplitNode
 * @typedef {PaneNode | SplitNode} Node
 */

const STORAGE_KEY = "diffuse:split-view:layout";

// ─── State ───────────────────────────────────────────────────────────────────

/** @type {Node} */
let state = loadState();

let editMode = false;

/** @type {string | null} */
let pendingPaneId = null;

/** @returns {Node} */
function loadState() {
  try {
    return /** @type {Node} */ (JSON.parse(
      localStorage.getItem(STORAGE_KEY) ?? "null",
    )) ?? defaultState();
  } catch {
    return defaultState();
  }
}

function saveState() {
  localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
}

/** @returns {PaneNode} */
function defaultState() {
  return { type: "pane", facet: null };
}

// ─── Tree helpers ─────────────────────────────────────────────────────────────
// Nodes are identified by a dot-separated path: "root", "root.start", "root.end.start"

/**
 * @param {string} path
 * @returns {Node}
 */
function getNode(path) {
  if (path === "root") return state;
  const parts = path.replace(/^root\./, "").split(".");
  /** @type {any} */
  let node = state;
  for (const part of parts) node = node[part];
  return /** @type {Node} */ (node);
}

/**
 * @param {string} path
 * @param {Node} newNode
 */
function replaceNode(path, newNode) {
  if (path === "root") {
    state = newNode;
    return;
  }
  const parts = path.split(".");
  const parentPath = parts.slice(0, -1).join(".");
  const childKey = parts[parts.length - 1];
  const parent = /** @type {any} */ (getNode(parentPath));
  parent[childKey] = newNode;
}

// ─── Mutations ────────────────────────────────────────────────────────────────

/**
 * @param {string} nodePath
 * @param {"horizontal" | "vertical"} orientation
 */
function splitPane(nodePath, orientation) {
  const existing = getNode(nodePath);
  replaceNode(nodePath, {
    type: "split",
    orientation,
    position: 50,
    start: existing,
    end: { type: "pane", facet: null },
  });
  saveState();
  render();
}

/** @param {string} nodePath */
function removePane(nodePath) {
  if (nodePath === "root") return;
  const parts = nodePath.split(".");
  const parentPath = parts.slice(0, -1).join(".");
  const childKey = parts[parts.length - 1];
  const siblingKey = childKey === "start" ? "end" : "start";
  const parent = /** @type {any} */ (getNode(parentPath));
  const sibling = /** @type {Node} */ (parent[siblingKey]);
  replaceNode(parentPath, sibling);
  saveState();
  render();
}

/**
 * @param {string} nodePath
 * @param {string} facetPath
 */
function setFacet(nodePath, facetPath) {
  const pane = /** @type {PaneNode} */ (getNode(nodePath));
  pane.facet = facetPath;
  saveState();
  render();
}

// ─── Rendering ────────────────────────────────────────────────────────────────

const layout = /** @type {HTMLElement} */ (document.querySelector("#layout"));

function render() {
  layout.innerHTML = "";
  layout.appendChild(renderNode(state, "root", true));
  layout.classList.toggle("edit-mode", editMode);
}

/**
 * @param {Node} node
 * @param {string} path
 * @param {boolean} isRoot
 * @returns {HTMLElement}
 */
function renderNode(node, path, isRoot) {
  if (node.type === "split") {
    const panel =
      /** @type {WaSplitPanel} */ (document.createElement("wa-split-panel"));
    panel.position = node.position;
    if (node.orientation === "vertical") panel.orientation = "vertical";

    const startSlot = document.createElement("div");
    startSlot.slot = "start";
    startSlot.appendChild(renderNode(node.start, path + ".start", false));

    const endSlot = document.createElement("div");
    endSlot.slot = "end";
    endSlot.appendChild(renderNode(node.end, path + ".end", false));

    panel.appendChild(startSlot);
    panel.appendChild(endSlot);

    panel.addEventListener("wa-reposition", () => {
      const splitNode = /** @type {SplitNode} */ (getNode(path));
      splitNode.position = panel.position;
      saveState();
    });

    return panel;
  }

  // Pane
  const pane = document.createElement("div");
  pane.className = "pane" + (node.facet ? "" : " pane--empty");

  if (node.facet) {
    const iframe = document.createElement("iframe");
    const uri = node.facet.includes("://")
      ? node.facet
      : `diffuse://${node.facet}`;
    iframe.src = "facets/l/?uri=" + encodeURIComponent(uri);
    iframe.allow = "autoplay";
    pane.appendChild(iframe);
  }

  const overlay = document.createElement("div");
  overlay.className = "pane-overlay";

  if (node.facet) {
    const title =
      document.querySelector(`#facet-select wa-option[value="${node.facet}"]`)
        ?.textContent?.trim() ?? node.facet;
    const label = document.createElement("div");
    label.className = "pane-name";
    label.style.fontWeight = "700";
    label.textContent = title;
    overlay.appendChild(label);
  }

  overlay.appendChild(
    makeWaButton(
      node.facet ? "Change facet" : "+ Add facet",
      "neutral",
      "filled",
      () => openPicker(path),
    ),
  );
  overlay.appendChild(
    makeWaButton(
      "Split left / right",
      "neutral",
      "outlined",
      () => splitPane(path, "horizontal"),
    ),
  );
  overlay.appendChild(
    makeWaButton(
      "Split top / bottom",
      "neutral",
      "outlined",
      () => splitPane(path, "vertical"),
    ),
  );

  if (!isRoot) {
    overlay.appendChild(
      makeWaButton("Remove", "danger", "outlined", () => removePane(path)),
    );
  }

  pane.appendChild(overlay);
  return pane;
}

/**
 * @param {string} text
 * @param {string} variant
 * @param {string} appearance
 * @param {() => void} onClick
 * @returns {HTMLElement}
 */
function makeWaButton(text, variant, appearance, onClick) {
  const btn = document.createElement("wa-button");
  btn.setAttribute("variant", variant);
  btn.setAttribute("appearance", appearance);
  btn.setAttribute("size", "small");
  btn.style.width = "100%";
  btn.textContent = text;
  btn.addEventListener("click", (e) => {
    e.stopPropagation();
    onClick();
  });
  return btn;
}

// ─── Divider drag: disable iframe pointer events while dragging ───────────────

document.addEventListener("mousedown", (e) => {
  const isDivider = e.composedPath().some(
    (el) => el instanceof Element && el.getAttribute("part") === "divider",
  );
  if (isDivider) layout.classList.add("dragging");
}, { capture: true });

document.addEventListener("mouseup", () => {
  layout.classList.remove("dragging");
});

// ─── Edit mode ────────────────────────────────────────────────────────────────

const editToggle =
  /** @type {HTMLElement} */ (document.querySelector("#edit-toggle"));
const editIcon = /** @type {WaIcon} */ (document.querySelector("#edit-icon"));

editToggle.addEventListener("click", () => {
  editMode = !editMode;
  editToggle.setAttribute("aria-label", editMode ? "Done" : "Edit layout");
  editIcon.name = editMode ? "xmark" : "border-all";
  layout.classList.toggle("edit-mode", editMode);
});

// ─── Facet picker ─────────────────────────────────────────────────────────────

const pickerDialog =
  /** @type {WaDialog} */ (document.querySelector("#facet-picker"));
const facetSelect =
  /** @type {WaSelect} */ (document.querySelector("#facet-select"));
const customPath =
  /** @type {WaInput} */ (document.querySelector("#custom-path"));
const customConfirm =
  /** @type {HTMLElement} */ (document.querySelector("#custom-confirm"));

customConfirm.addEventListener("click", () => {
  const val = customPath.value?.trim() || /** @type {string} */
    (facetSelect.value) || "";
  if (val && pendingPaneId !== null) setFacet(pendingPaneId, val);
  pendingPaneId = null;
  pickerDialog.open = false;
});

pickerDialog.addEventListener("wa-hide", (e) => {
  if (e.target === pickerDialog) pendingPaneId = null;
});

/** @param {string} nodePath */
function openPicker(nodePath) {
  pendingPaneId = nodePath;
  facetSelect.value = null;
  customPath.value = "";
  pickerDialog.open = true;
}

// ─── Init ─────────────────────────────────────────────────────────────────────

render();
