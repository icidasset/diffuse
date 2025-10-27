import { DiffuseElement } from "@common/element.js";

/**
 * @import {RenderArg} from "@common/element.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class WindowElement extends DiffuseElement {
  constructor() {
    super();

    this.id = this.id?.length ? this.id : crypto.randomUUID();
    this.attachShadow({ mode: "open" });
  }

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    const x = Math.floor(
      Math.random() * (document.body.clientWidth - 300),
    );

    this.style.position = "relative";
    this.style.left = `${x}px`;
  }

  /**
   * @override
   */
  disconnectedCallback() {
    super.disconnectedCallback();
  }

  // ACTIONS

  activate() {
    this.shadowRoot?.querySelector(".title-bar")?.classList.remove("inactive");
  }

  deactivate() {
    this.shadowRoot?.querySelector(".title-bar")?.classList.add("inactive");
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    return html`
      <link rel="stylesheet" href="/styles/vendor/98.css" />

      <style>
      dialog {
        background: transparent;
        border: 0;
        padding: 0;
      }
      </style>

      <dialog open>
        <div class="window" style="width: 300px">
          <div class="title-bar">
            <div class="title-bar-text">
              <slot name="title"></slot>
            </div>
            <div class="title-bar-controls">
              <!--<button aria-label="Minimize"></button>-->
              <!--<button aria-label="Maximize"></button>-->
              <button aria-label="Close"></button>
            </div>
          </div>
          <div class="window-body">
            <slot></slot>
          </div>
        </div>
      </dialog>
    `;
  }
}

export default WindowElement;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = WindowElement;
export const NAME = "dtw-window";

customElements.define(NAME, WindowElement);
