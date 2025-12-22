import { DiffuseElement } from "@common/element.js";

/**
 * @import {RenderArg} from "@common/element.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class WindowElement extends DiffuseElement {
  static observedAttributes = ["open"];

  constructor() {
    super();

    this.id = this.id?.length ? this.id : crypto.randomUUID();
    this.attachShadow({ mode: "open" });
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
      <link rel="stylesheet" href="../../styles/vendor/98.css" />

      <style>
      dialog {
        background: transparent;
        border: 0;
        padding: 0;
      }

      .window {
        min-width: 240px;
      }

      .title-bar {
        justify-content: unset;
        user-select: none;
      }

      .title-bar-icon {
        margin-right: 4px;
      }

      .title-bar-text {
        flex: 1;
      }
      </style>

      <dialog ?open="${this.hasAttribute("open")}">
        <div class="window">
          <div
            class="title-bar"
            @mousedown="${this.titleBarMouseDown.bind(this)}"
          >
            <div class="title-bar-icon">
              <slot name="title-icon"></slot>
            </div>
            <div class="title-bar-text" draggable="false">
              <slot name="title"></slot>
            </div>
            <div class="title-bar-controls">
              <!--<button aria-label="Minimize"></button>-->
              <!--<button aria-label="Maximize"></button>-->
              <button aria-label="Close" @click="${() =>
                this.removeAttribute("open")}"></button>
            </div>
          </div>
          <div class="window-body">
            <slot></slot>
          </div>
        </div>
      </dialog>
    `;
  }

  // EVENTS

  /**
   * @param {MouseEvent} mouse
   */
  titleBarMouseDown(mouse) {
    const event = new CustomEvent("dtw-window-start-move", {
      bubbles: true,
      composed: true,
      detail: {
        element: this,
        x: mouse.x,
        xElement: mouse.layerX,
        y: mouse.y,
        yElement: mouse.layerY,
      },
    });

    this.dispatchEvent(event);
  }
}

export default WindowElement;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = WindowElement;
export const NAME = "dtw-window";

customElements.define(NAME, WindowElement);
