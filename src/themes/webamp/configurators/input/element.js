import { DiffuseElement, query, whenElementsDefined } from "@common/element.js";
import { signal } from "@common/signal.js";

/**
 * @import {RenderArg} from "@common/element.d.ts"
 * @import {Track} from "@definitions/types.d.ts"
 * @import {InputElement} from "@components/input/types.d.ts"
 */

class InputConfig extends DiffuseElement {
  constructor() {
    super();

    this.attachShadow({ mode: "open" });
  }

  // EVENTS

  /**
   * @param {Event} event
   */
  #addOpenSubsonicServer = (event) => {
    event.preventDefault();
    console.log("TODO");
  };

  /**
   * @param {Event} event
   */
  #addS3Bucket = (event) => {
    event.preventDefault();
    console.log("TODO");
  };

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
                  <label for="access-key-input">Access Key:*</label>
                  <input type="text" id="access-key-input" required />
                </div>

                <div class="field-row">
                  <label for="secret-key-input">Secret Key:*</label>
                  <input type="password" id="secret-key-input" required />
                </div>

                <div class="field-row">
                  <label for="bucket-name-input">Bucket Name:*</label>
                  <input type="text" id="bucket-name-input" required />
                </div>

                <div class="field-row">
                  <label for="s3-host-input">Host:</label>
                  <input
                    type="text"
                    id="s3-host-input"
                    placeholder="s3.amazonaws.com"
                  />
                </div>

                <div class="field-row">
                  <label for="region-input">Region:</label>
                  <input
                    type="text"
                    id="region-input"
                    placeholder="us-east-1"
                  />
                </div>

                <div class="field-row">
                  <label for="path-input">Path:</label>
                  <input type="text" id="path-input" />
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
