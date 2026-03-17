import { DiffuseElement, nothing, query } from "~/common/element.js";
import { signal } from "~/common/signal.js";

import { NAME as ATPROTO_NAME } from "~/components/output/raw/atproto/element.js";
import { NAME as S3_NAME } from "~/components/output/bytes/s3/element.js";

import { NAME as PASSKEY_NAME } from "~/components/transformer/output/refiner/track-uri-passkey/element.js";

/**
 * @import {ATProtoOutputElement} from "~/components/output/raw/atproto/types.d.ts"
 *
 * @import {Bucket as S3Bucket} from "~/components/input/s3/types.d.ts"
 * @import {S3OutputElement} from "~/components/output/bytes/s3/types.d.ts"
 *
 * @import {OutputElement} from "~/components/output/types.d.ts"
 * @import {OutputConfiguratorElement} from "~/components/configurator/output/types.d.ts"
 * @import TrackUriPasskeyTransformer from "~/components/transformer/output/refiner/track-uri-passkey/element.js";
 *
 * @import {RenderArg} from "~/common/element.d.ts"
 */

/**
 * @typedef {OutputElement<any>} VariousOutputElements
 */

class OutputConfig extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
  }

  // SIGNALS

  $output = signal(
    /** @type {OutputElement | OutputConfiguratorElement<VariousOutputElements> | undefined} */ (undefined),
  );

  $atproto = signal(
    /** @type {ATProtoOutputElement | null} */ (null),
  );

  $atprotoPasskey = signal(
    /** @type {TrackUriPasskeyTransformer | null} */ (null),
  );

  $s3 = signal(
    /** @type {S3OutputElement | null} */ (null),
  );

  $atprotoError = signal(/** @type {string | null} */ (null));
  $passkeyError = signal(/** @type {string | null} */ (null));
  $passkeyWorking = signal(false);
  $tab = signal("overview");

  // LIFECYCLE

  /** @override */
  async connectedCallback() {
    super.connectedCallback();

    /** @type {OutputElement | OutputConfiguratorElement<VariousOutputElements>} */
    const output = query(this, "output-selector");

    await customElements.whenDefined(output.localName);

    this.$output.value = output;

    // Try setting up specific outputs
    const atproto = output.root().querySelector(ATPROTO_NAME);

    if (atproto) {
      this.$atproto.value = /** @type {ATProtoOutputElement} */ (atproto);
    }

    const atprotoPasskey = output.root().querySelector(
      `${PASSKEY_NAME}[namespace="atproto"]`,
    );

    if (atprotoPasskey) {
      await customElements.whenDefined(PASSKEY_NAME);
      this.$atprotoPasskey.value =
        /** @type {TrackUriPasskeyTransformer} */ (atprotoPasskey);
    }

    const s3 = output.root().querySelector(S3_NAME);

    if (s3) {
      this.$s3.value = /** @type {S3OutputElement} */ (s3);
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
    if (button) {
      button.disabled = true;
      button.textContent = "Loading ...";
    }

    this.$atprotoError.value = null;

    try {
      await atproto.login(handle);
    } catch (err) {
      this.$atprotoError.value = err instanceof Error
        ? err.message
        : "login failed";
      if (button) {
        button.disabled = false;
        button.textContent = "Sign in";
      }
    }
  };

  #handleAtprotoLogout = async () => {
    const atproto = this.$atproto.value;
    if (!atproto) return;

    await atproto.logout();
  };

  #handlePasskeySetup = async () => {
    const passkey = this.$atprotoPasskey.value;
    if (!passkey) return;

    this.$passkeyError.value = null;
    this.$passkeyWorking.value = true;

    try {
      await passkey.setupPasskey();
    } catch (err) {
      this.$passkeyError.value = err instanceof Error
        ? err.message
        : "Passkey setup failed";
    } finally {
      this.$passkeyWorking.value = false;
    }
  };

  #handlePasskeyAdopt = async () => {
    const passkey = this.$atprotoPasskey.value;
    if (!passkey) return;

    this.$passkeyError.value = null;
    this.$passkeyWorking.value = true;

    try {
      await passkey.adoptPasskey();
    } catch (err) {
      this.$passkeyError.value = err instanceof Error
        ? err.message
        : "Passkey adoption failed";
    } finally {
      this.$passkeyWorking.value = false;
    }
  };

  #handlePasskeyRemove = async () => {
    const passkey = this.$atprotoPasskey.value;
    if (!passkey) return;

    this.$passkeyError.value = null;
    await passkey.removePasskey();
  };

  /** @param {Event} event */
  #handleAtprotoActivate = async (event) => {
    event.preventDefault();

    const output = this.$output.value;
    if (!output || !("select" in output)) return;

    const atproto = this.$atproto.value;
    if (!atproto) return;

    const option = (await output.options()).find((o) =>
      o.label === "AT Protocol"
    );
    if (option) await output.select(option.id);
  };

  /**
   * @param {Event} event
   */
  #handleS3SetBucket = async (event) => {
    event.preventDefault();

    const s3 = this.$s3.value;
    if (!s3) return;

    /** @type {HTMLButtonElement | null} */
    const button = this.root().querySelector("#s3-submit");
    if (button) button.disabled = true;

    const accessKey =
      /** @type {HTMLInputElement | null} */ (this.root().querySelector(
        "#s3-access-key",
      ))?.value;
    const bucketName =
      /** @type {HTMLInputElement | null} */ (this.root().querySelector(
        "#s3-bucket-name",
      ))?.value;
    const host =
      /** @type {HTMLInputElement | null} */ (this.root().querySelector(
        "#s3-host",
      ))?.value;
    const path =
      /** @type {HTMLInputElement | null} */ (this.root().querySelector(
        "#s3-path",
      ))?.value;
    const region =
      /** @type {HTMLInputElement | null} */ (this.root().querySelector(
        "#s3-region",
      ))?.value;
    const secretKey =
      /** @type {HTMLInputElement | null} */ (this.root().querySelector(
        "#s3-secret-key",
      ))?.value;

    if (!accessKey || !bucketName || !secretKey) return;

    /** @type {S3Bucket} */
    const bucket = {
      accessKey,
      bucketName,
      host: host?.length ? host.replace(/^\w+:\/\//, "") : "s3.amazonaws.com",
      path: path?.length ? path : "/",
      region: region?.length ? region : "us-east-1",
      secretKey,
    };

    await s3.setBucket(bucket);

    if (button) button.disabled = false;
  };

  #handleS3Unset = async () => {
    const s3 = this.$s3.value;
    if (!s3) return;

    await s3.unsetBucket();
  };

  /** @param {Event} event */
  #handleS3Activate = async (event) => {
    event.preventDefault();

    const output = this.$output.value;
    if (!output || !("select" in output)) return;

    const s3 = this.$s3.value;
    if (!s3) return;

    const option = (await output.options()).find((o) => o.label === "S3");
    if (option) await output.select(option.id);
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
    return html`
      <link rel="stylesheet" href="vendor/98.css" />
      <link rel="stylesheet" href="themes/winamp/98-extra.css" />

      <style>
      @import "./themes/winamp/98-vars.css";

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

        li[aria-selected="true"] {
          padding-bottom: 2px;
          margin-top: -2px;
          background-color: var(--surface);
          position: relative;
          z-index: 8;
          margin-left: -3px;
        }
      }
      </style>

      <div id="tabbed">
        <menu role="tablist" class="multirows">
          <li role="tab" aria-selected="${this.$tab.value === "overview"}">
            <label @click="${() => this.$tab.value = "overview"}">
              <span>Overview</span>
            </label>
          </li>
          <li role="tab" aria-selected="${this.$tab.value === "atproto"}">
            <label @click="${() => this.$tab.value = "atproto"}">
              <span>AT Protocol</span>
            </label>
          </li>
          <li role="tab" aria-selected="${this.$tab.value === "s3"}">
            <label @click="${() => this.$tab.value = "s3"}">
              <span>S3</span>
            </label>
          </li>
        </menu>

        <div class="window" role="tabpanel">
          ${this.#renderTab(html)}
        </div>
      </div>
    `;
  }

  /**
   * @param {RenderArg["html"]} html
   */
  #renderTab(html) {
    switch (this.$tab.value) {
      case "overview":
        return this.#renderOverviewTab(html);
      case "atproto":
        return this.#renderAtprotoTab(html);
      case "s3":
        return this.#renderS3Tab(html);
      default:
        return nothing;
    }
  }

  /**
   * @param {RenderArg["html"]} html
   */
  #renderOverviewTab(html) {
    const selectedOutput =
      this.$output.value && "selected" in this.$output.value
        ? this.$output.value.selected()
        : undefined;

    return html`
      <div class="window-body">
        <fieldset>
          <span class="with-icon with-icon--large">
            <img
              src="images/icons/windows_98/computer_user_pencil-0.png"
              width="24"
            />
            <span>Here you can configure where to keep your user data.<br />Each
              storage method comes with its pros and cons.<br />By default your data
              is only kept locally here in the browser.
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
              ${this.$output.value &&
                  "selected" in this.$output.value
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
    `;
  }

  /**
   * @param {RenderArg["html"]} html
   */
  #renderAtprotoTab(html) {
    const did = this.$atproto.value?.did() ?? null;
    const selectedOutput =
      this.$output.value && "selected" in this.$output.value
        ? this.$output.value.selected()
        : undefined;

    const authenticated = () => {
      return html`
        <fieldset>
          <span class="with-icon with-icon--large">
            <img src="images/icons/windows_98/computer_user_pencil-0.png" width="24" />
            <span>Signed in as <strong>${did}</strong></span>
          </span>
        </fieldset>

        ${this.#renderPasskeySection(html)}

        <p class="button-row">
          <button @click="${this.#handleAtprotoLogout}">Sign out</button>
          ${this.#renderAtprotoActivation(html, selectedOutput)}
        </p>
      `;
    };

    const unauthenticated = () => {
      return html`
        <fieldset>
          <span class="with-icon with-icon--large">
            <img src="images/icons/windows_98/computer_user_pencil-0.png" width="24" />
            <span>
              Store your user data on the storage associated with your AT Protocol
              identity.
            </span>
          </span>
        </fieldset>

        <fieldset>
          <form @submit="${this.#handleAtprotoLogin}" class="field-row">
            <label for="atproto-handle">Your internet handle:</label>
            <input
              id="atproto-handle"
              type="text"
              required
              placeholder="you.bsky.social"
            />
          </form>
        </fieldset>

        ${this.$atprotoError.value
          ? html`
            <fieldset>
              <span class="with-icon with-icon--large">
                <img src="images/icons/windows_98/msg_error-0.png" width="24" />
                <span>
                  Sign in failed, please check the provided handle and try again.
                </span>
              </span>
            </fieldset>
          `
          : nothing} ${this.#renderPasskeySection(html)}

        <p>
          <button @click="${this
            .#handleAtprotoLogin}" id="atproto-submit">Sign in</button>
          ${this.#renderAtprotoActivation(html, selectedOutput)}
        </p>
      `;
    };

    return html`
      <div class="window-body">
        ${did ? authenticated() : unauthenticated()}
      </div>
    `;
  }

  /**
   * @param {RenderArg["html"]} html
   */
  #renderS3Tab(html) {
    const s3 = this.$s3.value;
    const ready = s3?.ready() ?? false;
    const selectedOutput =
      this.$output.value && "selected" in this.$output.value
        ? this.$output.value.selected()
        : undefined;

    const configured = () => {
      const bucket = s3?.bucket();

      return html`
        <fieldset>
          <div class="with-icon with-icon--large">
            <img src="images/icons/windows_98/computer_user_pencil-0.png" width="24" />
            <div>
              Bucket configured:
              <ul
                style="margin-bottom: 0; padding-left: 0; list-style-position: inside;"
              >
                <li>Name: <strong>${bucket?.bucketName}</strong></li>
                <li>Host: ${bucket?.host}</li>
                <li>Access key: ${bucket?.accessKey}</li>
              </ul>
            </div>
          </div>
        </fieldset>

        <fieldset>
          <span class="with-icon with-icon--large">
            <img
              src="images/icons/windows_98/msg_information-0.png"
              width="24"
            />
            <span>
              Make sure the bucket has CORS configured properly.
            </span>
          </span>
        </fieldset>

        <p class="button-row">
          <button id="s3-unset-bucket" @click="${this.#handleS3Unset}">
            Remove bucket configuration
          </button>
          ${this.#renderS3Activation(html, selectedOutput)}
        </p>
      `;
    };

    const unconfigured = () => {
      return html`
        <fieldset>
          <span class="with-icon with-icon--large">
            <img src="images/icons/windows_98/computer_user_pencil-0.png" width="24" />
            <span>
              Store your user data on an S3-compatible storage service.
            </span>
          </span>
        </fieldset>

        <form @submit="${this.#handleS3SetBucket}">
          <fieldset>
            <legend>Bucket details</legend>

            <div class="field-row">
              <label for="s3-access-key">Access Key:*</label>
              <input type="text" id="s3-access-key" required />
            </div>

            <div class="field-row">
              <label for="s3-secret-key">Secret Key:*</label>
              <input type="password" id="s3-secret-key" required />
            </div>

            <div class="field-row">
              <label for="s3-bucket-name">Bucket Name:*</label>
              <input type="text" id="s3-bucket-name" required />
            </div>

            <div class="field-row">
              <label for="s3-host">Host:</label>
              <input
                type="text"
                id="s3-host"
                placeholder="s3.amazonaws.com"
              />
            </div>

            <div class="field-row">
              <label for="s3-region">Region:</label>
              <input
                type="text"
                id="s3-region"
                placeholder="us-east-1"
              />
            </div>

            <div class="field-row">
              <label for="s3-path">Path:</label>
              <input type="text" id="s3-path" />
            </div>

            <p>
              * are required fields.
            </p>
          </fieldset>

          <p>
            <button type="submit" id="s3-submit">Set bucket</button>
            ${this.#renderS3Activation(html, selectedOutput)}
          </p>
        </form>
      `;
    };

    return html`
      <div class="window-body">
        ${ready ? configured() : unconfigured()}
      </div>
    `;
  }

  /**
   * @param {RenderArg["html"]} html
   */
  #renderPasskeySection(html) {
    const passkey = this.$atprotoPasskey.value;
    if (!passkey) return nothing;

    const passkeyActive = passkey.passkeyActive() ?? false;
    const lockedTracksCount = passkey.lockedTracks().length ?? 0;

    return html`
      <fieldset>
        <legend>Passkey encryption (optional)</legend>

        <div class="with-icon with-icon--large">
          <img src="images/icons/windows_98/keys-5.png" width="24" />

          <div>
            ${passkeyActive
              ? html`
                <p class="with-icon with-icon--large">
                  <input type="checkbox" checked />
                  <label>Passkey active — Track URIs are encrypted</label>
                </p>

                ${this.$passkeyError.value
                  ? html`
                    <fieldset>
                      <span class="with-icon with-icon--large">
                        <img src="images/icons/windows_98/msg_error-0.png" width="24" />
                        <span>${this.$passkeyError.value}</span>
                      </span>
                    </fieldset>
                  `
                  : nothing}

                <p>
                  <button @click="${this
                    .#handlePasskeyRemove}">Remove passkey</button>
                </p>

                <p>
                  Removing the passkey will expose all the sensitive<br />
                  information that was previously encrypted.
                </p>
              `
              : html`
                <p>
                  Track URIs can optionally be encrypted so that passwords and<br />
                  other sensitive authentication details are kept private.
                </p>
                <p>
                  Note that, with this enabled, other people can NOT play audio listed on your
                  account.
                </p>

                ${this.$passkeyError.value
                  ? html`
                    <fieldset>
                      <span class="with-icon with-icon--large">
                        <img src="images/icons/windows_98/msg_error-0.png" width="24" />
                        <span>${this.$passkeyError.value}</span>
                      </span>
                    </fieldset>
                  `
                  : nothing}

                <p class="button-row">
                  <button
                    ?disabled="${this.$passkeyWorking.value}"
                    @click="${this.#handlePasskeySetup}"
                  >
                    ${this.$passkeyWorking.value
                      ? "Setting up ..."
                      : "Set up passkey encryption"}
                  </button>
                  <button
                    ?disabled="${this.$passkeyWorking.value}"
                    @click="${this.#handlePasskeyAdopt}"
                  >
                    ${this.$passkeyWorking.value
                      ? "Authenticating ..."
                      : "Use existing passkey"}
                  </button>
                </p>
              `}
          </div>
        </div>
      </fieldset>

      ${lockedTracksCount > 0
        ? html`
          <fieldset>
            <p class="with-icon with-icon--large">
              <img
                src="images/icons/windows_98/msg_warning-0.png"
                width="24"
              />
              ${lockedTracksCount} encrypted track(s) cannot be played until you unlock them with
              your passkey. If you're already using a passkey, remember that you have to
              use same passkey as the one you originally locked the tracks with.
            </p>
          </fieldset>
        `
        : nothing}
    `;
  }

  /**
   * @param {RenderArg['html']} html
   * @param {VariousOutputElements | null | undefined} selectedOutput
   */
  #renderAtprotoActivation(html, selectedOutput) {
    const output = this.$output.value;
    if (!output || !("select" in output)) return nothing;

    const isActive = selectedOutput?.label === "AT Protocol";

    return isActive
      ? html`
        <button @click="${this.#handleDeactivate}">Deactivate</button>
      `
      : html`
        <button @click="${this
          .#handleAtprotoActivate}">Activate this storage</button>
      `;
  }

  /**
   * @param {RenderArg['html']} html
   * @param {VariousOutputElements | null | undefined} selectedOutput
   */
  #renderS3Activation(html, selectedOutput) {
    const output = this.$output.value;
    if (!output || !("select" in output)) return nothing;

    const isActive = selectedOutput?.label === "S3";

    return isActive
      ? html`
        <button @click="${this.#handleDeactivate}">Deactivate</button>
      `
      : html`
        <button @click="${this
          .#handleS3Activate}">Activate this storage</button>
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
