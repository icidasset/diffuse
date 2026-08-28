import { html, render } from "lit-html";

import { facetFromURI } from "~/common/facets/utils.js";
import { saveFacet } from "./crud.js";

////////////////////////////////////////////
// DIALOG
////////////////////////////////////////////

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
              <label>URI</label>
              <input
                id="add-uri-uri"
                type="text"
                placeholder="at://..."
                required
                autocomplete="off"
              />
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

    dialog.querySelector("#add-uri-cancel")?.addEventListener("click", () => {
      /** @type {HTMLDialogElement} */ (dialog).close();
    });

    dialog.querySelector("#add-from-uri-form")?.addEventListener(
      "submit",
      async (e) => {
        e.preventDefault();

        const nameEl = /** @type {HTMLInputElement} */ (
          dialog?.querySelector("#add-uri-name")
        );

        const kindEl = /** @type {HTMLSelectElement} */ (
          dialog?.querySelector("#add-uri-kind")
        );

        const uriEl = /** @type {HTMLInputElement} */ (
          dialog?.querySelector("#add-uri-uri")
        );

        const name = nameEl?.value.trim() ?? "";
        const kind = kindEl?.value ?? "interactive";
        const uri = uriEl?.value.trim() ?? "";
        if (!name || !uri) return;

        const facet = await facetFromURI({ kind, name, uri }, {
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

  if (nameEl) nameEl.value = "";
  if (kindEl) kindEl.value = "interactive";
  if (uriEl) uriEl.value = "";

  dialog.showModal();
}
