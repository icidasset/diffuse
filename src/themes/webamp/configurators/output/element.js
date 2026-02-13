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
      #tabbed:has(#automerge-tab:checked) #automerge-contents { display: block }
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
            <label for="automerge-tab">
              <span>Automerge Repo</span>
              <input name="output-tab" id="automerge-tab" type="radio" />
            </label>
          </li>
        </menu>

        <div class="window" role="tabpanel">
          <!-- Overview -->
          <div class="window-body" id="overview-contents">
            <form>
              <p>Do you want to sync your data somewhere?</p>
              <div class="field-row">
                <input id="idb-json" type="radio" checked />
                <label for="idb-json">No syncing, just keep data locally.</label>
              </div>
              <div class="field-row">
                <input id="idb-json" type="radio" disabled />
                <label for="idb-json">Automerge Repo</label>
              </div>
            </form>
          </div>

          <!-- Automerge -->
          <div class="window-body" id="automerge-contents">
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
