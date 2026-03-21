import "@awesome.me/webawesome/dist/components/badge/badge.js";
import "@awesome.me/webawesome/dist/components/button/button.js";
import "@awesome.me/webawesome/dist/components/callout/callout.js";
import "@awesome.me/webawesome/dist/components/card/card.js";
import "@awesome.me/webawesome/dist/components/dialog/dialog.js";
import "@awesome.me/webawesome/dist/components/divider/divider.js";
import "@awesome.me/webawesome/dist/components/icon/icon.js";

import "~/common/webawesome/detect-dark.js";
import "~/common/webawesome/phosphor/bold.js";
import "~/common/webawesome/phosphor/fill.js";

import { html, nothing, render as litRender } from "lit-html";

/**
 * @import { default as WaDialog } from "@awesome.me/webawesome/dist/components/dialog/dialog.js"
 * @import { TemplateResult } from "lit-html"
 */

/**
 * @typedef {{ name: string; detail: string; isInput: boolean; isOutput: boolean; isSelectedOutput: boolean; onRemove: () => void }} ConnectItem
 */

/**
 * Sets up a connect facet UI: a card with "Add audio input" and
 * "Use as userdata storage" buttons, a dialog with a form, and a
 * reactive list of configured items below a divider.
 *
 * @param {Object} config
 * @param {string} config.title - Card header title
 * @param {TemplateResult | string} config.description - Content above the buttons
 * @param {TemplateResult} config.formFields - Form body content (inputs, footnotes, etc.)
 * @param {(mode: 'input' | 'output') => Promise<void>} config.onSubmit
 * @param {boolean} [config.hasInput] - Whether to show the "Add audio input" button (default: true)
 * @param {boolean} [config.hasOutput] - Whether to show the "Use as userdata storage" button (default: true)
 * @param {() => Promise<void>} [config.onOutputActivate] - Called instead of opening the dialog when output is already configured but inactive
 *
 * @returns {{ setItems: (items: ConnectItem[]) => void, setError: (message: string | null) => void }}
 */
export function setup(
  {
    title,
    description,
    formFields,
    onSubmit,
    hasInput = true,
    hasOutput = true,
    onOutputActivate,
  },
) {
  const main = document.querySelector("main");
  if (!main) throw new Error("No <main> element");

  litRender(
    html`
      <wa-card>
        <div slot="header" class="card-header">
          <strong>${title}</strong>
        </div>
        <div class="card-body">
          ${description}
          <div class="button-row">
            ${hasInput
              ? html`
                <wa-button id="connect-add-input-btn" variant="neutral" appearance="filled">
                  <wa-icon slot="start" library="phosphor/fill" name="music-notes"></wa-icon>
                  Add audio input
                </wa-button>
              `
              : nothing}
            ${hasOutput
              ? html`
                <wa-button
                  id="connect-add-output-btn"
                  variant="brand"
                  appearance="filled"
                >
                  <wa-icon slot="start" library="phosphor/fill" name="person"></wa-icon>
                  Use as userdata storage
                </wa-button>
              `
              : nothing}
          </div>
          <wa-callout id="connect-card-error" variant="danger" hidden></wa-callout>
          <wa-divider id="connect-divider" hidden></wa-divider>
          <ul id="connect-list" class="connect-list" hidden></ul>
        </div>
      </wa-card>

      <wa-dialog id="connect-dialog" label="">
        <form id="connect-form" class="dialog-body">
          ${formFields}
          <wa-callout id="connect-error" variant="danger" hidden></wa-callout>
        </form>
        <div slot="footer" class="dialog-footer">
          <wa-button
            id="connect-submit-btn"
            type="submit"
            form="connect-form"
            variant="brand"
            appearance="filled"
          >Add</wa-button>
          <wa-button id="connect-cancel-btn" variant="neutral" appearance="outlined">
            Cancel
          </wa-button>
        </div>
      </wa-dialog>
    `,
    main,
  );

  const dialog =
    /** @type {WaDialog} */ (main.querySelector("#connect-dialog"));
  const form =
    /** @type {HTMLFormElement} */ (main.querySelector("#connect-form"));
  const dialogErrorEl =
    /** @type {HTMLElement} */ (main.querySelector("#connect-error"));
  const cardErrorEl =
    /** @type {HTMLElement} */ (main.querySelector("#connect-card-error"));
  const divider =
    /** @type {HTMLElement} */ (main.querySelector("#connect-divider"));
  const list = /** @type {HTMLElement} */ (main.querySelector("#connect-list"));
  const outputBtn =
    /** @type {HTMLElement} */ (main.querySelector("#connect-add-output-btn"));

  /** @type {'input' | 'output'} */
  let mode = "input";

  /** @type {ConnectItem[]} */
  let currentItems = [];

  /** @param {string | null} message */
  const setDialogError = (message) => {
    dialogErrorEl.hidden = message === null;
    dialogErrorEl.textContent = message;
  };

  /** @param {string | null} message */
  const setError = (message) => {
    cardErrorEl.hidden = message === null;
    cardErrorEl.textContent = message;
  };

  /** @param {'input' | 'output'} m */
  const openDialog = (m) => {
    mode = m;
    dialog.label = m === "input"
      ? "Add audio input"
      : "Use as userdata storage";
    form.reset();
    setDialogError(null);
    dialog.open = true;
  };

  if (hasInput) {
    main
      .querySelector("#connect-add-input-btn")
      ?.addEventListener("click", () => openDialog("input"));
  }

  main
    .querySelector("#connect-add-output-btn")
    ?.addEventListener("click", () => {
      if (onOutputActivate && currentItems.some((i) => i.isOutput)) {
        onOutputActivate();
      } else {
        openDialog("output");
      }
    });

  main.querySelector("#connect-cancel-btn")?.addEventListener("click", () => {
    setDialogError(null);
    dialog.open = false;
  });

  const submitBtn =
    /** @type {HTMLElement} */ (main.querySelector("#connect-submit-btn"));

  form.addEventListener("submit", async (e) => {
    e.preventDefault();
    setDialogError(null);
    submitBtn.setAttribute("disabled", "");
    submitBtn.textContent = "Loading …";
    try {
      await onSubmit(mode);
      dialog.open = false;
    } catch (err) {
      setDialogError(
        err instanceof Error ? err.message : "Something went wrong",
      );
    } finally {
      submitBtn.removeAttribute("disabled");
      submitBtn.textContent = "Add";
    }
  });

  return {
    setError,

    /**
     * Updates the list of configured items below the divider.
     * Call inside an effect() for reactivity.
     *
     * @param {ConnectItem[]} items
     */
    setItems(items) {
      currentItems = items;
      divider.hidden = items.length === 0;
      list.hidden = items.length === 0;
      if (outputBtn) outputBtn.hidden = items.some((i) => i.isOutput && i.isSelectedOutput);
      litRender(
        html`
          ${items.map(
            ({ name, detail, isInput, isOutput, isSelectedOutput, onRemove }) =>
              html`
                <li class="connect-item">
                  <div class="connect-item__info">
                    <span class="connect-item__name">${name}</span>
                    <span class="connect-item__detail">${detail}</span>
                  </div>
                  <div class="connect-item__tags">
                    ${isInput
                      ? html`
                        <wa-badge appearance="outlined" variant="neutral">Input</wa-badge>
                      `
                      : nothing} ${isOutput
                      ? html`
                        <wa-badge appearance="outlined" variant="${isSelectedOutput
                          ? "brand"
                          : "warning"}">Output</wa-badge>
                      `
                      : nothing}
                  </div>
                  <wa-button
                    appearance="plain"
                    size="small"
                    aria-label="Remove"
                    @click="${onRemove}"
                  >
                    <wa-icon library="phosphor/bold" name="x"></wa-icon>
                  </wa-button>
                </li>
              `,
          )}
        `,
        list,
      );
    },
  };
}
