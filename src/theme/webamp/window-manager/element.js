import { DiffuseElement } from "@common/element.js";
import { signal } from "@common/signal.js";

/**
 * @import {RenderArg} from "@common/element.d.ts"
 * @import WindowElement from "../window/element.js";
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class WindowManager extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
  }

  // SIGNALS

  $activeWindow = signal(/** @type {string | null} */ (null));

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    this.addEventListener("click", this.setActiveWindow);

    this.effect(() => {
      const activeId = this.$activeWindow.value;
      this.setWindowStatuses(activeId);
    });
  }

  /**
   * @override
   */
  disconnectedCallback() {
    super.disconnectedCallback();
    this.removeEventListener("click", this.setActiveWindow);
  }

  /**
   * @param {string | null} activeId
   */
  async setWindowStatuses(activeId) {
    await customElements.whenDefined("dtw-window");

    this.querySelectorAll("dtw-window").forEach(
      (window) => {
        const win = /** @type {WindowElement} */ (window);

        if (activeId && window.id === activeId) {
          win.activate();
        } else {
          win.deactivate();
        }
      },
    );
  }

  /**
   * @param {Event} event
   */
  setActiveWindow(event) {
    if (event.target instanceof HTMLElement) {
      const window = event.target?.closest("dtw-window");
      if (!window) return;
      if (window.id) this.$activeWindow.value = window.id;
    }
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    return html`
      <slot></slot>
    `;
  }
}

export default WindowManager;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = WindowManager;
export const NAME = "dtw-window-manager";

customElements.define(NAME, WindowManager);
