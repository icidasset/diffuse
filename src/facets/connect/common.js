import { encodeQR } from "@paulmillr/qr";
import { html, nothing, render as litRender } from "lit-html";

/**
 * @import { TemplateResult } from "lit-html"
 */

/**
 * @typedef {{ name: string; detail: string; isInput: boolean; isOutput: boolean; isSelectedOutput: boolean; isDisabled?: boolean; onShowQR?: () => void; onRemove: () => void; onToggleDisabled?: () => void }} ConnectItem
 */

/**
 * @param {string} label
 */
function outputErrorMessage(label) {
  return `${label} output was not enabled!`;
}

/**
 * Wait for an output option to be registered, failing after a timeout
 * instead of waiting indefinitely. The option is added by the output-bundle
 * prelude once it is loaded, so it may not exist yet when a connect page
 * first asks for it.
 *
 * @param {{ waitForOption: (label: string) => Promise<{ id: string }> }} outputOrchestrator
 * @param {string} label
 * @param {number} [timeoutMs=30_000]
 * @returns {Promise<{ id: string }>}
 */
export async function waitForOutputOption(
  outputOrchestrator,
  label,
  timeoutMs = 30_000,
) {
  let timer;
  const timeout = new Promise((_, reject) => {
    timer = setTimeout(
      () => reject(new Error(outputErrorMessage(label))),
      timeoutMs,
    );
  });

  try {
    return await Promise.race([outputOrchestrator.waitForOption(label), timeout]);
  } finally {
    clearTimeout(timer);
  }
}

/**
 * Sets up a connect facet UI: a card with "Add audio input" and
 * "Use as userdata storage" buttons, a dialog with a form, and a
 * reactive list of configured items below a divider.
 *
 * @param {Object} config
 * @param {string} config.title - Card header title
 * @param {TemplateResult | string} config.description - Content shown on the left side
 * @param {TemplateResult | typeof nothing} [config.rightContent] - Extra content shown at the top of the right side
 * @param {TemplateResult} config.formFields - Form body content (inputs, footnotes, etc.)
 * @param {(mode: 'input' | 'output') => Promise<void>} config.onSubmit
 * @param {boolean} [config.hasInput] - Whether to show the "Add audio input" button (default: true)
 * @param {boolean} [config.hasOutput] - Whether to show the "Use as userdata storage" button (default: true)
 * @param {TemplateResult | typeof nothing} [config.footerActions] - Extra buttons rendered in the dialog footer
 * @param {() => Promise<void>} [config.onOutputActivate] - Called instead of opening the dialog when output is already configured but inactive
 *
 * @returns {{ setItems: (items: ConnectItem[]) => void, setError: (message: string | null) => void, setDialogError: (message: string | null) => void, showQR: (data: string) => void }}
 */
export function setup(
  {
    title,
    description,
    rightContent = nothing,
    formFields,
    footerActions = nothing,
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
      <div class="facet__left">
        <div>
          <a href="./dashboard/" class="diffuse-logo-container">
            <svg viewBox="0 0 902 134" width="160">
              <title>Diffuse</title>
              <use
                xlink:href="images/diffuse-current.svg#diffuse"
                href="images/diffuse-current.svg#diffuse"
              ></use>
            </svg>
          </a>
        </div>
        <div class="h1-container">
          <h1>${title}</h1>
        </div>
        ${description}
      </div>
      <div class="facet__right">
        ${rightContent}
        <div class="button-row">
          ${hasInput
            ? html`
              <button id="connect-add-input-btn">
                <i class="ph-fill ph-music-notes"></i>
                Add audio input
              </button>
            `
            : nothing}
          ${hasOutput
            ? html`
              <button id="connect-add-output-btn" class="button--brand">
                <i class="ph-fill ph-person"></i>
                Use as userdata storage
              </button>
            `
            : nothing}
        </div>
        <div id="connect-card-error" class="callout callout--danger" hidden></div>
        <hr id="connect-divider" hidden>
        <ul id="connect-list" class="connect-list" hidden></ul>
      </div>

      <dialog id="connect-dialog">
        <div class="dialog-header">
          <strong id="connect-dialog-title"></strong>
        </div>
        <form id="connect-form" class="dialog-body">
          <div id="connect-error" class="callout callout--danger" hidden style="margin: 0"></div>
          ${formFields}
        </form>
        <div class="dialog-footer">
          <button id="connect-submit-btn" type="submit" form="connect-form" class="button--brand">Add</button>
          <button id="connect-cancel-btn" type="button">Cancel</button>
          ${footerActions}
        </div>
      </dialog>

      <dialog id="connect-qr-dialog">
        <div class="dialog-header">
          <strong>Scan to connect</strong>
        </div>
        <div id="connect-qr-body" class="dialog-body connect-qr-body"></div>
        <div class="dialog-footer">
          <button id="connect-qr-close-btn" type="button">Close</button>
        </div>
      </dialog>
    `,
    main,
  );

  const dialog =
    /** @type {HTMLDialogElement} */ (main.querySelector("#connect-dialog"));
  const dialogTitleEl =
    /** @type {HTMLElement} */ (main.querySelector("#connect-dialog-title"));
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
    dialogTitleEl.textContent = m === "input"
      ? "Add audio input"
      : "Use as userdata storage";
    form.reset();
    setDialogError(null);
    dialog.showModal();
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
    dialog.close();
  });

  const qrDialog =
    /** @type {HTMLDialogElement} */ (main.querySelector("#connect-qr-dialog"));
  const qrBody =
    /** @type {HTMLElement} */ (main.querySelector("#connect-qr-body"));

  main.querySelector("#connect-qr-close-btn")?.addEventListener("click", () => {
    qrDialog.close();
  });

  /** @param {string} data */
  const showQR = (data) => {
    qrBody.innerHTML = encodeQR(data, "svg");
    qrDialog.showModal();
  };

  const submitBtn =
    /** @type {HTMLElement} */ (main.querySelector("#connect-submit-btn"));

  form.addEventListener("submit", async (e) => {
    e.preventDefault();
    setDialogError(null);
    submitBtn.setAttribute("disabled", "");
    submitBtn.textContent = "Loading …";
    try {
      await onSubmit(mode);
      dialog.close();
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
    setDialogError,
    showQR,

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
            ({ name, detail, isInput, isOutput, isSelectedOutput, isDisabled, onShowQR, onRemove, onToggleDisabled }, index) =>
              html`
                <li class="connect-item${isDisabled ? " connect-item--disabled" : ""}">
                  <div class="connect-item__info">
                    <span class="connect-item__name">${name}</span>
                    <span class="connect-item__detail">${detail}</span>
                  </div>
                  <div class="connect-item__tags">
                    ${isInput
                      ? html`<span class="badge">Input</span>`
                      : nothing}
                    ${isOutput
                      ? html`<span class="badge ${isSelectedOutput ? "badge--brand" : "badge--warning"}">Output</span>`
                      : nothing}
                  </div>
                  <button
                    class="button--plain button--icon"
                    aria-label="More"
                    popovertarget="connect-item-menu-${index}"
                  >
                    <i class="ph-fill ph-dots-three-outline-vertical"></i>
                  </button>
                  <div id="connect-item-menu-${index}" class="dropdown" popover>
                    ${onShowQR
                      ? html`
                        <button
                          @click="${(/** @type {MouseEvent} */ e) => {
                            /** @type {HTMLElement | null} */ (/** @type {HTMLElement} */ (e.currentTarget).closest("[popover]"))?.hidePopover();
                            onShowQR();
                          }}"
                        >
                          <i class="ph-fill ph-qr-code"></i>
                          Show QR code
                        </button>
                      `
                      : nothing}
                    ${onToggleDisabled
                      ? html`
                        <button
                          @click="${(/** @type {MouseEvent} */ e) => {
                            /** @type {HTMLElement | null} */ (/** @type {HTMLElement} */ (e.currentTarget).closest("[popover]"))?.hidePopover();
                            onToggleDisabled();
                          }}"
                        >
                          <i class="ph-fill ${isDisabled ? "ph-eye" : "ph-eye-slash"}"></i>
                          ${isDisabled ? "Enable" : "Disable"}
                        </button>
                      `
                      : nothing}
                    <button
                      @click="${(/** @type {MouseEvent} */ e) => {
                        /** @type {HTMLElement | null} */ (/** @type {HTMLElement} */ (e.currentTarget).closest("[popover]"))?.hidePopover();
                        onRemove();
                      }}"
                    >
                      <i class="ph-fill ph-skull"></i>
                      Disconnect
                    </button>
                  </div>
                </li>
              `,
          )}
        `,
        list,
      );
    },
  };
}
