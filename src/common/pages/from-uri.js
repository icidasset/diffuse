import { html, render } from "lit-html";
import * as URI from "fast-uri";

import { facetFromURI } from "~/common/facets/utils.js";
import facetsData from "~/_data/facets.json" with { type: "json" };
import { loadAtProtoRecord } from "~/common/loader.js";
import { saveFacet } from "./crud.js";

////////////////////////////////////////////
// DIALOG
////////////////////////////////////////////

// Track whether the user has manually edited the name/kind/description
// fields, so the auto-fill triggered by URI changes doesn't stomp on their
// input.
let nameEdited = false;
let kindEdited = false;
let descriptionEdited = false;

/**
 * Best-effort auto-fill of the name, kind and description fields based on the
 * entered URI.
 *
 * - `diffuse://` URIs are looked up in the bundled facet catalogue (offline).
 * - `at://` URIs resolve the stored record; `name`, `kind` and `description`
 *   are taken from the record when present.
 * - `https://` URIs use the document's `<title>` and meta description, falling
 *   back to a name derived from the URL path when the document can't be
 *   fetched.
 *
 * Fields the user has already edited are left alone. Failures are silent.
 *
 * @param {HTMLInputElement | null} uriEl
 * @param {HTMLInputElement | null} nameEl
 * @param {HTMLSelectElement | null} kindEl
 * @param {HTMLTextAreaElement | null} descriptionEl
 */
async function inferFromURI(uriEl, nameEl, kindEl, descriptionEl) {
  const uri = uriEl?.value.trim();
  if (!uri) return;

  const scheme = URI.parse(uri).scheme;

  if (scheme === "diffuse") {
    const entry = facetsData.find(
      (c) => c.url === uri.replace(/^diffuse:\/\//, ""),
    );
    if (!entry) return;
    if (!nameEdited && nameEl && entry.title) nameEl.value = entry.title;
    if (!descriptionEdited && descriptionEl && entry.desc) {
      descriptionEl.value = entry.desc;
    }
    const kind = entry.kind === "prelude" ? "prelude" : "interactive";
    if (!kindEdited && kindEl && kindEl.value !== kind) kindEl.value = kind;
    return;
  }

  if (scheme === "at") {
    try {
      const value = await loadAtProtoRecord(uri);
      // A newer URI may have been entered while resolving — don't apply stale
      // results over it.
      if (uriEl?.value.trim() !== uri) return;
      if (!nameEdited && nameEl && typeof value.name === "string" && value.name) {
        nameEl.value = value.name;
      }
      if (
        !descriptionEdited &&
        descriptionEl &&
        typeof value.description === "string" &&
        value.description
      ) {
        descriptionEl.value = value.description;
      }
      if (
        !kindEdited &&
        kindEl &&
        (value.kind === "interactive" || value.kind === "prelude") &&
        kindEl.value !== value.kind
      ) {
        kindEl.value = value.kind;
      }
    } catch {
      // Unresolvable URI — leave the fields as-is.
    }
    return;
  }

  if (scheme === "http" || scheme === "https") {
    const fallback = nameFromURL(uri);
    try {
      const res = await fetch(uri);
      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      const doc = new DOMParser().parseFromString(
        await res.text(),
        "text/html",
      );
      // A newer URI may have been entered while fetching — don't apply stale
      // results over it.
      if (uriEl?.value.trim() !== uri) return;
      const title = doc.querySelector("title")?.textContent?.trim();
      if (title && !nameEdited && nameEl) nameEl.value = title;
      const meta = doc.querySelector('meta[name="description"]')
          ?.getAttribute("content")
        ?? doc.querySelector('meta[property="og:description"]')
          ?.getAttribute("content");
      if (meta?.trim() && !descriptionEdited && descriptionEl) {
        descriptionEl.value = meta.trim();
      }
    } catch {
      // Network/CORS failure — fall back to a name derived from the URL.
      if (!nameEdited && nameEl && fallback) nameEl.value = fallback;
    }
  }
}

/**
 * Derives a display name from an HTTP(S) URL's path, used when the document
 * itself can't be fetched. E.g. `https://example.com/facets/foo-bar/index.html`
 * becomes `Foo Bar`.
 *
 * @param {string} url
 * @returns {string | undefined}
 */
function nameFromURL(url) {
  try {
    const { pathname } = new URL(url);
    const segments = pathname.replace(/\/+$/, "").split("/").filter(Boolean);
    let last = segments.at(-1) ?? "";
    if (last === "index.html" || last === "index.htm") {
      last = segments.at(-2) ?? "";
    }
    const name = last
      .replace(/\.(html?|tile)$/, "")
      .split(/[-_]+/)
      .filter(Boolean)
      .map((word) => word[0].toUpperCase() + word.slice(1))
      .join(" ");
    return name || undefined;
  } catch {
    return undefined;
  }
}

export function openAddFromURIModal() {
  let dialog = /** @type {HTMLDialogElement | null} */ (
    document.getElementById("add-from-uri-dialog")
  );

  if (!dialog) {
    dialog = /** @type {HTMLDialogElement} */ (
      document.createElement("dialog")
    );

    dialog.id = "add-from-uri-dialog";
    dialog.style.cssText =
      "position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); margin: 0;";

    render(
      html`
        <form id="add-from-uri-form">
          <p style="font-size: var(--fs-sm)">
            <strong>Load a facet from a URI.</strong> Currently supported URI schemes:
            <code>https</code>, <code>at</code> (AT Protocol) and <code>diffuse</code>
            (references internal facets).
          </p>

          <div style="display: flex; flex-direction: column; gap: var(--space-xs)">
            <div>
              <label>URI</label>
              <input
                id="add-uri-uri"
                type="text"
                placeholder="at://..., https://... or diffuse://..."
                required
                autocomplete="off"
              />
            </div>
            <div>
              <label>Name</label>
              <input
                id="add-uri-name"
                type="text"
                placeholder="My Feature Name"
                required
                autocomplete="off"
              />
            </div>
            <div>
              <label>Kind</label>
              <select id="add-uri-kind">
                <option value="interactive">interface</option>
                <option value="prelude">feature</option>
              </select>
            </div>
            <div>
              <label>Description</label>
              <textarea
                id="add-uri-description"
                rows="3"
                placeholder="What does this facet do?"
                autocomplete="off"
              ></textarea>
            </div>
          </div>
          <div
            style="display: flex; font-size: var(--fs-sm); gap: var(--space-xs); margin-top: var(--space-sm)"
          >
            <button type="submit" class="button--bg-accent">Add</button>
            <button type="button" id="add-uri-cancel">
              Cancel
            </button>
          </div>
        </form>
      `,
      dialog,
    );

    document.body.appendChild(dialog);

    const nameEl = /** @type {HTMLInputElement | null} */ (
      dialog.querySelector("#add-uri-name")
    );
    const kindEl = /** @type {HTMLSelectElement | null} */ (
      dialog.querySelector("#add-uri-kind")
    );
    const uriEl = /** @type {HTMLInputElement | null} */ (
      dialog.querySelector("#add-uri-uri")
    );
    const descriptionEl = /** @type {HTMLTextAreaElement | null} */ (
      dialog.querySelector("#add-uri-description")
    );

    nameEl?.addEventListener("input", () => {
      nameEdited = true;
    });

    kindEl?.addEventListener("change", () => {
      kindEdited = true;
    });

    descriptionEl?.addEventListener("input", () => {
      descriptionEdited = true;
    });

    /** @type {ReturnType<typeof setTimeout> | undefined} */
    let fillTimer;
    uriEl?.addEventListener("input", () => {
      clearTimeout(fillTimer);
      fillTimer = setTimeout(() => {
        void inferFromURI(uriEl, nameEl, kindEl, descriptionEl);
      }, 500);
    });

    dialog.querySelector("#add-uri-cancel")?.addEventListener("click", () => {
      /** @type {HTMLDialogElement} */ (dialog).close();
    });

    dialog.querySelector("#add-from-uri-form")?.addEventListener(
      "submit",
      async (e) => {
        e.preventDefault();

        const name = nameEl?.value.trim() ?? "";
        const kind = kindEl?.value ?? "interactive";
        const uri = uriEl?.value.trim() ?? "";
        const description = descriptionEl?.value.trim() || undefined;
        if (!name || !uri) return;

        const facet = await facetFromURI({ kind, name, uri, description }, {
          fetchHTML: false,
        });

        await saveFacet(facet);

        /** @type {HTMLDialogElement} */ (dialog).close();
      },
    );
  }

  const nameEl = /** @type {HTMLInputElement} */ (
    dialog.querySelector("#add-uri-name")
  );

  const kindEl = /** @type {HTMLSelectElement} */ (
    dialog.querySelector("#add-uri-kind")
  );

  const uriEl = /** @type {HTMLInputElement} */ (
    dialog.querySelector("#add-uri-uri")
  );

  const descriptionEl = /** @type {HTMLTextAreaElement} */ (
    dialog.querySelector("#add-uri-description")
  );

  if (nameEl) nameEl.value = "";
  if (kindEl) kindEl.value = "interactive";
  if (uriEl) uriEl.value = "";
  if (descriptionEl) descriptionEl.value = "";

  nameEdited = false;
  kindEdited = false;
  descriptionEdited = false;

  dialog.showModal();
}