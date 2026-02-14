import { DiffuseElement } from "@common/element.js";

/**
 * @import {RenderArg} from "@common/element.d.ts"
 */

class OutputConfig extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    return html`
      <link rel="stylesheet" href="styles/vendor/98.css" />
      <link rel="stylesheet" href="themes/webamp/facet.css" />

      <style>
      @import "./themes/webamp/98-vars.css";

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
                <img src="images/icons/windows_98/computer_user_pencil-0.png" width="24" />
                <span>Here you can configure where to keep your user data.<br />Each storage method comes with its pros and cons.<br />By default your data is only kept locally here in the browser.</span>
              </span>
            </fieldset>
          </div>

          <!-- S3 -->
          <div class="window-body" id="s3-contents">
            <p>TODO</p>
          </div>
        </div>
      </div>
    `;
  }
}

export default OutputConfig;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = OutputConfig;
export const NAME = "dtw-output-config";

customElements.define(NAME, CLASS);
