import { DiffuseElement, query, whenElementsDefined } from "@common/element.js";
import { signal } from "@common/signal.js";

import { buildURI as buildOpenSubsonicURI } from "@components/input/opensubsonic/common.js";
import { buildURI as buildS3cURI } from "@components/input/s3/common.js";

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
    /** @type {OutputElement<Track[]> | undefined} */ (undefined),
  );

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    /** @type {InputElement} */
    const input = query(this, "input-selector");

    /** @type {OutputElement<Track[]>} */
    const output = query(this, "output-selector");

    this.$input.value = input;
    this.$output.value = output;

    // Wait for the elements to be defined before proceeding
    whenElementsDefined({ input, output }).then(() => {
      //
    });
  }

  // EVENTS

  /**
   * @param {Event} event
   */
  #addOpenSubsonicServer = (event) => {
    event.preventDefault();

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
    return this.addSource(uri);
  };

  /**
   * @param {Event} event
   */
  #addS3Bucket = (event) => {
    event.preventDefault();

    const accessKey = this.formElement("s3-access-key")?.value;
    const bucketName = this.formElement("s3-bucket-name")?.value;
    const host = this.formElement("s3-host")?.value ?? "s3.amazonaws.com";
    const path = this.formElement("s3-path")?.value ?? "/";
    const region = this.formElement("s3-region")?.value ?? "us-east-1";
    const secretKey = this.formElement("s3-secret-key")?.value;

    if (!accessKey) throw new Error("Missing required `accessKey` input value");
    if (!bucketName) {
      throw new Error("Missing required `bucketName` input value");
    }
    if (!secretKey) throw new Error("Missing required `secretKey` input value");

    /** @type {S3Bucket} */
    const bucket = {
      accessKey,
      bucketName,
      host,
      path,
      region,
      secretKey,
    };

    const uri = buildS3cURI(bucket);
    return this.addSource(uri);
  };

  // 🛠️

  /**
   * @param {string} uri
   */
  addSource(uri) {
    /** @type {Track} */
    const track = {
      $type: "sh.diffuse.output.tracks",
      id: crypto.randomUUID(),
      kind: "placeholder",
      uri,
    };

    const output = this.$output.value;
    if (!output) throw new Error("Output isn't ready yet!");

    output.tracks.save(
      [...output.tracks.collection(), track],
    );
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

      #tabbed:has(#opensubsonic-tab:checked) #opensubsonic-contents { display: block }
      #tabbed:has(#s3-tab:checked) #s3-contents { display: block }

      /* FORMS */

      input, select, textarea {
        flex: 1;
      }
      </style>

      <div id="tabbed">
        <menu role="tablist" class="multirows">
          <li role="tab">
            <label for="opensubsonic-tab">
              <span>OpenSubsonic</span>
              <input name="input-tab" id="opensubsonic-tab" type="radio" checked="" />
            </label>
          </li>
          <li role="tab">
            <label for="s3-tab">
              <span>S3</span>
              <input name="input-tab" id="s3-tab" type="radio" />
            </label>
          </li>
        </menu>

        <div class="window" role="tabpanel">
          <!-- Opensubsonic -->
          <div class="window-body" id="opensubsonic-contents">
            <fieldset>
              <legend>Added servers</legend>

              <p>TODO</p>
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
                <input type="submit" value="Add server" />
              </p>
            </form>
          </div>

          <!-- S3 -->
          <div class="window-body" id="s3-contents">
            <fieldset>
              <legend>Added buckets</legend>

              <p>TODO</p>
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
                <input type="submit" value="Add bucket" />
              </p>
            </form>
          </div>
        </div>
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
