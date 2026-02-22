import {
  DiffuseElement,
  nothing,
  query,
  queryOptional,
} from "@common/element.js";
import { signal } from "@common/signal.js";

import { buildURI as buildOpenSubsonicURI } from "@components/input/opensubsonic/common.js";
import { buildURI as buildS3URI } from "@components/input/s3/common.js";

import { SCHEME as HTTPS_SCHEME } from "@components/input/https/constants.js";
import { SCHEME as OPENSUBSONIC_SCHEME } from "@components/input/opensubsonic/constants.js";
import { SCHEME as S3_SCHEME } from "@components/input/s3/constants.js";

import { highlightTableEntry } from "../../common/ui.js";

/**
 * @import {RenderArg} from "@common/element.d.ts"
 * @import {Track} from "@definitions/types.d.ts"
 * @import {InputElement} from "@components/input/types.d.ts"
 * @import {OutputElement} from "@components/output/types.d.ts"
 *
 * @import {Server as OpenSubsonicServer} from "@components/input/opensubsonic/types.d.ts"
 * @import {Bucket as S3Bucket} from "@components/input/s3/types.d.ts"
 */

class InputConfig extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
  }

  // SIGNALS

  $input = signal(
    /** @type {InputElement | undefined} */ (undefined),
  );

  $output = signal(
    /** @type {OutputElement | undefined} */ (undefined),
  );

  $sourcesOrchestrator = signal(
    /** @type {import("@components/orchestrator/sources/element.js").CLASS | undefined} */ (undefined),
  );

  $processTracksOrchestrator = signal(
    /** @type {import("@components/orchestrator/process-tracks/element.js").CLASS | undefined} */ (undefined),
  );

  $tab = signal("overview");

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();

    /** @type {InputElement} */
    const input = query(this, "input-selector");

    /** @type {OutputElement} */
    const output = query(this, "output-selector");

    /** @type {import("@components/orchestrator/sources/element.js").CLASS} */
    const sourcesOrchestrator = query(this, "sources-orchestrator-selector");

    /** @type {import("@components/orchestrator/process-tracks/element.js").CLASS | null} */
    const processTracksOrchestrator = queryOptional(
      this,
      "process-tracks-orchestrator-selector",
    );

    await customElements.whenDefined(input.localName);
    await customElements.whenDefined(output.localName);
    await customElements.whenDefined(sourcesOrchestrator.localName);

    if (processTracksOrchestrator) {
      await customElements.whenDefined(processTracksOrchestrator.localName);
    }

    this.$input.value = input;
    this.$output.value = output;
    this.$sourcesOrchestrator.value = sourcesOrchestrator;

    if (processTracksOrchestrator) {
      this.$processTracksOrchestrator.value = processTracksOrchestrator;
    }
  }

  // EVENTS

  /**
   * @param {Event} event
   */
  #addOpenSubsonicServer = async (event) => {
    event.preventDefault();

    /** @type {HTMLButtonElement | null} */
    const button = this.root().querySelector("#opensubsonic-submit");
    if (button) button.disabled = true;

    const host = this.formElement("opensubsonic-host")?.value;
    const tls = this.formElement("opensubsonic-tls")?.value === "true";
    const username = this.formElement("opensubsonic-username")?.value;
    const password = this.formElement("opensubsonic-password")?.value;
    const apiKey = this.formElement("opensubsonic-apikey")?.value;

    if (!host) {
      throw new Error("Missing required `host` input value");
    }

    /** @type {OpenSubsonicServer} */
    const server = {
      host,
      tls,
      username,
      password,
      apiKey,
    };

    const uri = buildOpenSubsonicURI(server);
    await this.addSource(uri);

    if (button) button.disabled = false;
  };

  /**
   * @param {Event} event
   */
  #addS3Bucket = async (event) => {
    event.preventDefault();

    /** @type {HTMLButtonElement | null} */
    const button = this.root().querySelector("s3-submit");
    if (button) button.disabled = true;

    const accessKey = this.formElement("s3-access-key")?.value;
    const bucketName = this.formElement("s3-bucket-name")?.value;
    const host = this.formElement("s3-host")?.value;
    const path = this.formElement("s3-path")?.value;
    const region = this.formElement("s3-region")?.value;
    const secretKey = this.formElement("s3-secret-key")?.value;

    if (!accessKey) {
      throw new Error("Missing required `accessKey` input value");
    }

    if (!bucketName) {
      throw new Error("Missing required `bucketName` input value");
    }

    if (!secretKey) {
      throw new Error("Missing required `secretKey` input value");
    }

    /** @type {S3Bucket} */
    const bucket = {
      accessKey,
      bucketName,
      host: host?.length ? host : "s3.amazonaws.com",
      path: path?.length ? path : "/",
      region: region?.length ? region : "us-east-1",
      secretKey,
    };

    const uri = buildS3URI(bucket);
    await this.addSource(uri);

    if (button) button.disabled = false;
  };

  /**
   * @param {Event} event
   */
  #addHttpsUrl = async (event) => {
    event.preventDefault();

    /** @type {HTMLButtonElement | null} */
    const button = this.root().querySelector("#https-submit");
    if (button) button.disabled = true;

    const url = this.formElement("https-url")?.value;

    if (!url) {
      throw new Error("Missing required `url` input value");
    }

    await this.addSource(url);

    if (button) button.disabled = false;
  };

  /**
   * @param {Event} event
   */
  #deleteSelected = async (event) => {
    const button = /** @type {HTMLElement} */ (event.target);
    const fieldset = event.target ? button.closest("fieldset") : null;
    if (!fieldset) return;

    const selected = fieldset.querySelector(
      "table tr.highlighted",
    );
    if (!selected) return;

    const uri = selected.getAttribute("data-uri");
    if (!uri) throw new Error("Missing `uri` attribute");

    const detachedTracks = await this.$input.value?.detach({
      fileUriOrScheme: uri,
      tracks: this.$output.value?.tracks.collection() ?? [],
    });

    if (detachedTracks) this.$output.value?.tracks.save(detachedTracks);
  };

  /** @param {MouseEvent} event */
  #highlightTableEntry(event) {
    highlightTableEntry(event);

    const fieldset = event.target
      ? /** @type {HTMLElement} */ (event.target).closest("fieldset")
      : null;
    if (!fieldset) return;

    fieldset.querySelector('button[role="delete"]')?.removeAttribute(
      "disabled",
    );
  }

  // 🛠️

  /**
   * @param {string} uri
   */
  async addSource(uri) {
    /** @type {Track} */
    const track = {
      $type: "sh.diffuse.output.track",
      id: crypto.randomUUID(),
      kind: "placeholder",
      uri,
    };

    await this.$output.value?.tracks.save(
      [...(this.$output.value?.tracks.collection() ?? []), track],
    );

    this.$tab.value = "overview";
  }

  /**
   * @param {string} id
   * @returns {HTMLInputElement | null}
   */
  formElement(id) {
    return this.root().querySelector(`#${id}`);
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    return html`
      <link rel="stylesheet" href="styles/vendor/98.css" />
      <link rel="stylesheet" href="themes/webamp/98-extra.css" />

      <style>
      @import "./themes/webamp/98-vars.css";

      #tabbed {
        display: flex;
        flex-direction: column;
        height: 100%;
      }

      fieldset {
        margin-bottom: var(--element-spacing);
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

      /* LIST */

      table {
        color: black;
        table-layout: fixed;
      }

      table td {
        overflow: hidden;
        text-overflow: ellipsis;
      }

      table tbody tr {
        cursor: pointer;
      }

      /* FORMS */

      input, select, textarea {
        flex: 1;
      }
      </style>

      <div id="tabbed">
        <menu role="tablist" class="multirows">
          <li role="tab" aria-selected="${this.$tab.value === "overview"}">
            <label @click="${() => this.$tab.value = "overview"}">
              <span>Overview</span>
            </label>
          </li>
          <li role="tab" aria-selected="${this.$tab.value === "https"}">
            <label @click="${() => this.$tab.value = "https"}">
              <span>HTTPS</span>
            </label>
          </li>
          <li role="tab" aria-selected="${this.$tab.value === "opensubsonic"}">
            <label @click="${() => this.$tab.value = "opensubsonic"}">
              <span>OpenSubsonic</span>
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
      case "https":
        return this.#renderHttpsTab(html);
      case "opensubsonic":
        return this.#renderOpenSubsonicTab(html);
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
    return html`
      <div class="window-body">
        <fieldset>
          <span class="with-icon with-icon--large">
            <img
              src="images/icons/windows_98/cd_audio_cd_a-0.png"
              width="24"
            />
            <span>Here you can configure where your audio comes from.<br />Add sources
              using the tabs above, then tracks will be processed automatically.
            </span>
          </span>
        </fieldset>

        ${this.#renderProcessingProgress(html)}
      </div>
    `;
  }

  /**
   * @param {RenderArg["html"]} html
   */
  #renderHttpsTab(html) {
    const sources = this.$sourcesOrchestrator.value?.sources();

    return html`
      <div class="window-body">
        <fieldset>
          ${this.#renderList(
            html,
            sources?.[HTTPS_SCHEME] ?? [],
            "Added URLs",
          )}

          <p>
            <button disabled role="delete" @click="${this.#deleteSelected}">
              Delete selected
            </button>
          </p>
        </fieldset>

        <form @submit="${this.#addHttpsUrl}">
          <fieldset>
            <div class="field-row">
              <label for="https-url">URL:</label>
              <input
                id="https-url"
                type="url"
                required
                placeholder="https://example.com/audio.mp3"
              />
            </div>
          </fieldset>

          <p>
            <button type="submit" id="https-submit">Add URL</button>
          </p>
        </form>
      </div>
    `;
  }

  /**
   * @param {RenderArg["html"]} html
   */
  #renderOpenSubsonicTab(html) {
    const sources = this.$sourcesOrchestrator.value?.sources();

    return html`
      <div class="window-body">
        <fieldset>
          ${this.#renderList(
            html,
            sources?.[OPENSUBSONIC_SCHEME] ?? [],
            "Added servers",
          )}

          <p>
            <button disabled role="delete" @click="${this.#deleteSelected}">
              Delete selected
            </button>
          </p>
        </fieldset>

        <form @submit="${this.#addOpenSubsonicServer}">
          <fieldset>
            <legend>Server details</legend>

            <div class="field-row">
              <label for="opensubsonic-host">Host domain:*</label>
              <input id="opensubsonic-host" type="text" required />
            </div>

            <div class="field-row">
              <label for="opensubsonic-tls">Use HTTPS/TLS:</label>
              <select id="opensubsonic-tls">
                <option value="true" selected>Yes</option>
                <option value="false">No</option>
              </select>
            </div>

            <p>
              Either provide a username & password combination:
            </p>

            <div class="field-row">
              <label for="opensubsonic-username">Username:</label>
              <input id="opensubsonic-username" type="text" />
            </div>

            <div class="field-row">
              <label for="opensubsonic-password">Password:</label>
              <input id="opensubsonic-password" type="password" />
            </div>

            <p>
              Or an API key:
            </p>

            <div class="field-row">
              <label for="opensubsonic-apikey">API key:</label>
              <input id="opensubsonic-apikey" type="text" />
            </div>

            <p>
              * are required fields.
            </p>
          </fieldset>

          <p>
            <button type="submit" id="opensubsonic-submit">Add server</button>
          </p>
        </form>
      </div>
    `;
  }

  /**
   * @param {RenderArg["html"]} html
   */
  #renderS3Tab(html) {
    const sources = this.$sourcesOrchestrator.value?.sources();

    return html`
      <div class="window-body">
        <fieldset>
          ${this.#renderList(
            html,
            sources?.[S3_SCHEME] ?? [],
            "Added buckets",
          )}

          <p>
            <button disabled role="delete" @click="${this.#deleteSelected}">
              Delete selected
            </button>
          </p>
        </fieldset>

        <form @submit="${this.#addS3Bucket}">
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
            <button type="submit" id="s3-submit">Add bucket</button>
          </p>
        </form>
      </div>
    `;
  }

  /**
   * @param {RenderArg["html"]} html
   */
  #renderProcessingProgress(html) {
    const orchestrator = this.$processTracksOrchestrator.value;
    if (!orchestrator?.isProcessing()) return nothing;

    const { processed, total } = orchestrator.progress();
    const percentage = total > 0 ? Math.round((processed / total) * 100) : 0;

    return html`
      <fieldset>
        <legend>Processing tracks</legend>
        <div class="with-icon with-icon--large">
          <img
            src="images/icons/windows_98/gears-0.png"
            width="24"
          />
          <span>
            ${total === 0
              ? `Going through all the inputs and gathering the tracks ...`
              : `Making sure each track has metadata & statistics (${processed} / ${total}) ...`}
          </span>
        </div>
        <div
          class="progress-indicator"
          style="margin-top: var(--grouped-element-spacing);"
        >
          <div class="progress-indicator-bar" style="width: ${percentage}%"></div>
        </div>
      </fieldset>
    `;
  }

  /**
   * @param {RenderArg["html"]} html
   * @param {Array<{label: string, uri: string}>} list
   * @param {string} title
   */
  #renderList(html, list, title) {
    return html`
      <div class="sunken-panel">
        <table style="width: 100%;" @click="${this.#highlightTableEntry}">
          <thead>
            <tr>
              <th>${title}</th>
            </tr>
          </thead>
          <tbody>
            ${list.map((item) =>
              html`
                <tr data-uri="${item.uri}">
                  <td>${item.label}</td>
                </tr>
              `
            )}
          </tbody>
        </table>
      </div>
    `;
  }
}

export default InputConfig;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = InputConfig;
export const NAME = "dtw-input-config";

customElements.define(NAME, CLASS);
