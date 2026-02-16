import { DiffuseElement, nothing, query } from "@common/element.js";
import { signal } from "@common/signal.js";

import { NAME as ATPROTO_NAME } from "@components/output/raw/atproto/element.js";

/**
 * @import {ATProtoOutputElement} from "@components/output/raw/atproto/types.d.ts"
 * @import {OutputElement} from "@components/output/types.d.ts"
 * @import {OutputConfiguratorElement, OutputOption} from "@components/configurator/output/types.d.ts"
 * @import {RenderArg} from "@common/element.d.ts"
 */

class OutputConfig extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
  }

  // SIGNALS

  $output = signal(
    /** @type {OutputElement | OutputConfiguratorElement | undefined} */ (undefined),
  );

  $atproto = signal(
    /** @type {OutputOption<ATProtoOutputElement> | null} */ (null),
  );

  // LIFECYCLE

  /** @override */
  async connectedCallback() {
    super.connectedCallback();

    /** @type {OutputElement | OutputConfiguratorElement} */
    const output = query(this, "output-selector");

    await customElements.whenDefined(output.localName);

    this.$output.value = output;

    // Try setting up specific outputs
    if ("options" in output === false) return;
    const options = await output.options();
    const atproto = options.find((o) => o.element.localName === ATPROTO_NAME);

    if (atproto) {
      this.$atproto.value =
        /** @type {OutputOption<ATProtoOutputElement>} */ (atproto);
    }
  }

  // EVENTS

  /** @param {Event} event */
  #handleAtprotoLogin = async (event) => {
    event.preventDefault();

    /** @type {HTMLInputElement | null} */
    const input = this.root().querySelector("#atproto-handle");
    const handle = input?.value?.trim();
    if (!handle) return;

    const atproto = this.$atproto.value;
    if (!atproto) return;

    /** @type {HTMLButtonElement | null} */
    const button = this.root().querySelector("#atproto-submit");
    if (button) button.disabled = true;

    await atproto.element.login(handle);
  };

  #handleAtprotoLogout = async () => {
    const atproto = this.$atproto.value;
    if (!atproto) return;

    await atproto.element.logout();
  };

  #handleAtprotoActivate = async () => {
    const output = this.$output.value;
    if (!output || !("select" in output)) return;

    const atproto = this.$atproto.value;
    if (!atproto) return;

    await output.select(atproto.id);
  };

  #handleDeactivate = async () => {
    const output = this.$output.value;
    if (!output || !("deselect" in output)) return;

    await output.deselect();
  };

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    const did = this.$atproto.value?.element.did() ?? null;
    const selectedOutput =
      this.$output.value && "selectedOutput" in this.$output.value
        ? this.$output.value.selectedOutput()
        : undefined;

    return html`
      <link rel="stylesheet" href="styles/vendor/98.css" />
      <link rel="stylesheet" href="themes/webamp/facet.css" />

      <style>
      @import "./themes/webamp/98-vars.css";

      .button-row {
        display: inline-flex;
        gap: var(--grouped-button-spacing);
      }

      #tabbed {
        display: flex;
        flex-direction: column;
        height: 100%;
      }

      .window {
        flex: 1;
        overflow-y: auto;
      }

      /* TABS */

      menu[role="tablist"] {
        padding-top: 2px;

        li > label {
          cursor: pointer;
          display: block;
          padding: var(--radio-label-spacing);
        }

        /* Copied styles from "li[aria-selected=true]" */
        li:has(input:checked) {
          padding-bottom: 2px;
          margin-top: -2px;
          background-color: var(--surface);
          position: relative;
          z-index: 8;
          margin-left: -3px;
        }

        input {
          display: none
        }
      }

      .window-body {
        display: none
      }

      #tabbed:has(#overview-tab:checked) #overview-contents { display: block }
      #tabbed:has(#atproto-tab:checked) #atproto-contents { display: block }
      #tabbed:has(#s3-tab:checked) #s3-contents { display: block }
      </style>

      <div id="tabbed">
        <menu role="tablist" class="multirows">
          <li role="tab">
            <label for="overview-tab">
              <span>Overview</span>
              <input name="output-tab" id="overview-tab" type="radio" checked="" />
            </label>
          </li>
          <li role="tab">
            <label for="atproto-tab">
              <span>AT Protocol</span>
              <input name="output-tab" id="atproto-tab" type="radio" />
            </label>
          </li>
          <li role="tab">
            <label for="s3-tab">
              <span>S3</span>
              <input name="output-tab" id="s3-tab" type="radio" />
            </label>
          </li>
        </menu>

        <div class="window" role="tabpanel">
          <!-- Overview -->
          <div class="window-body" id="overview-contents">
            <fieldset>
              <span class="with-icon with-icon--large">
                <img
                  src="images/icons/windows_98/computer_user_pencil-0.png"
                  width="24"
                />
                <span>Here you can configure where to keep your user data.<br />Each
                  storage method comes with its pros and cons.<br />By default your
                  data is only kept locally here in the browser.
                </span>
              </span>
            </fieldset>

            <fieldset>
              <span class="with-icon with-icon--large">
                <img
                  src="images/icons/windows_98/msg_information-0.png"
                  width="24"
                />
                <span>
                  Data does not transfer across storage methods!<br />You can however
                  merge data between them though, if you wish to do so.
                </span>
              </span>
            </fieldset>

            <fieldset>
              <legend>Active storage method</legend>
              <div class="with-icon with-icon--large">
                <img
                  src="images/icons/windows_98/${selectedOutput
                    ? `directory_channels-2.png`
                    : `msg_warning-0.png`}"
                  width="24"
                />
                <div>
                  ${this.$output.value && "selectedOutput" in this.$output.value
                    ? selectedOutput
                      ? html`
                        <p>
                          Selected output:
                          <strong>${selectedOutput.label}</strong><br />
                        </p>
                        <p>
                          <button @click="${this
                            .#handleDeactivate}">Deactivate</button>
                        </p>
                      `
                      : this.#defaultOutputMessage
                    : this.#defaultOutputMessage}
                </div>
              </div>
            </fieldset>
          </div>

          <!-- AT Protocol -->
          <div class="window-body" id="atproto-contents">
            ${did
              ? html`
                <fieldset>
                  <span class="with-icon with-icon--large">
                    <img src="images/icons/windows_98/computer_user_pencil-0.png" width="24" />
                    <span>Signed in as <strong>${did}</strong></span>
                  </span>
                </fieldset>

                <p class="button-row">
                  <button @click="${this
                    .#handleAtprotoLogout}">Sign out</button>
                  ${this.#renderAtprotoActivation(html, selectedOutput)}
                </p>
              `
              : html`
                <fieldset>
                  <span class="with-icon with-icon--large">
                    <img src="images/icons/windows_98/computer_user_pencil-0.png" width="24" />
                    <span>
                      Store your user data on the storage associated with your AT Protocol
                      identity.
                    </span>
                  </span>
                </fieldset>

                <form @submit="${this.#handleAtprotoLogin}">
                  <fieldset>
                    <div class="field-row">
                      <label for="atproto-handle">Your internet handle:</label>
                      <input
                        id="atproto-handle"
                        type="text"
                        required
                        placeholder="you.bsky.social"
                      />
                    </div>
                  </fieldset>

                  <p>
                    <button type="submit" id="atproto-submit">Sign in</button>
                  </p>
                </form>
              `}
          </div>

          <!-- S3 -->
          <div class="window-body" id="s3-contents">
            <p>TODO</p>
          </div>
        </div>
      </div>
    `;
  }

  /**
   * @param {RenderArg['html']} html
   * @param {OutputElement | null | undefined} selectedOutput
   */
  #renderAtprotoActivation(html, selectedOutput) {
    const output = this.$output.value;
    if (!output || !("select" in output)) return nothing;

    const atproto = this.$atproto.value;
    const isActive = selectedOutput && atproto &&
      selectedOutput.selector === atproto.element.selector;

    return isActive
      ? html`
        <button @click="${this.#handleDeactivate}">Deactivate</button>
      `
      : html`
        <button @click="${this
          .#handleAtprotoActivate}">Activate this storage</button>
      `;
  }

  #defaultOutputMessage =
    "Storing data locally in the browser without any backup or syncing enabled.";
}

export default OutputConfig;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = OutputConfig;
export const NAME = "dtw-output-config";

customElements.define(NAME, CLASS);
