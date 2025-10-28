import { DiffuseElement } from "@common/element.js";
import { signal } from "@common/signal.js";
import { debounceMicrotask } from "@vicary/debounce-microtask";

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
  #lastZindex = 1000;

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    // Events
    this.addEventListener("mousedown", this.focusOnWindow);
    this.addEventListener("dtw-window-start-move", this.windowMoveStart);

    // Webamp stuff
    document.body.addEventListener(
      "mousedown",
      this.bringWebampToFront.bind(this),
    );

    // React to active window changing
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

    this.removeEventListener("mousedown", this.focusOnWindow);
    this.removeEventListener("dtw-window-start-move", this.windowMoveStart);

    document.body.removeEventListener(
      "mousedown",
      this.bringWebampToFront.bind(this),
    );
  }

  /**
   * @param {MouseEvent} event
   */
  bringWebampToFront(event) {
    if (event.target instanceof HTMLElement) {
      const webamp = event.target?.closest("#webamp");
      if (webamp instanceof HTMLElement) {
        this.#lastZindex++;
        webamp.style.zIndex = this.#lastZindex.toString();
      }
    }
  }

  /**
   * @param {Event} event
   */
  focusOnWindow(event) {
    if (event.target instanceof HTMLElement) {
      const win = event.target?.closest("dtw-window");
      if (win instanceof HTMLElement === false) return;
      if (win.id) this.$activeWindow.value = win.id;

      this.#lastZindex++;
      win.style.zIndex = this.#lastZindex.toString();
    }
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
   * @param {any} ogEvent
   */
  windowMoveStart(ogEvent) {
    /**
     * @param {Event} event
     */
    const moveFn = debounceMicrotask((event) => {
      if (event instanceof MouseEvent) {
        const x = event.x - ogEvent.detail.xElement;
        const y = event.y - ogEvent.detail.yElement;
        const target = ogEvent.target;

        if (target) {
          target.style.left = `${x}px`;
          target.style.top = `${y}px`;
        }
      }
    }, {
      updateArguments: true,
    });

    const stopMove = () => {
      this.removeEventListener("mousemove", moveFn);
      this.removeEventListener("dtw-window-end-move", stopMove);

      document.removeEventListener("mouseup", stopMove);
      document.removeEventListener("mouseleave", stopMove);
    };

    this.addEventListener("mousemove", moveFn);
    this.addEventListener("dtw-window-end-move", stopMove);

    document.addEventListener("mouseup", stopMove);
    document.addEventListener("mouseleave", stopMove);
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    return html`
      <style>
      :host {
        user-select: none;
      }
      </style>

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
