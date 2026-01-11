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

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    return html`
      <link rel="stylesheet" href="styles/vendor/98.css" />

      <style>
      @import "./themes/webamp/98-vars.css";

      menu[role="tablist"] {
        padding-top: 2px;

        li > label {
          display: block;
          margin: var(--radio-label-spacing);
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
            <p>TODO: Opensubsonic form</p>
          </div>

          <!-- S3 -->
          <div class="window-body" id="s3-contents">
            <p>TODO: S3 form</p>
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
