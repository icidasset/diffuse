import { defineElement, DiffuseElement } from "~/common/element.js";
import { signal } from "~/common/signal.js";
import { debounceMicrotask } from "@vicary/debounce-microtask";

import WindowElement from "../window/element.js";

/**
 * @import {RenderArg} from "~/common/element.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

const STORAGE_PREFIX = "diffuse/winamp/window/";

class WindowManager extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });

    this.focusOnWindow = this.focusOnWindow.bind(this);
    this.windowMoveStart = this.windowMoveStart.bind(this);
  }

  // SIGNALS

  $activeWindow = signal(/** @type {string | null} */ (null));
  #lastZindex = 1000;

  // STORAGE

  /**
   * @param {string} id
   * @param {string} left
   * @param {string} top
   */
  #savePosition(id, left, top) {
    localStorage.setItem(`${STORAGE_PREFIX}${id}`, JSON.stringify({ left, top }));
  }

  /**
   * @param {string} id
   * @returns {{ left: string; top: string } | null}
   */
  #loadPosition(id) {
    try {
      const raw = localStorage.getItem(`${STORAGE_PREFIX}${id}`);
      if (!raw) return null;
      return JSON.parse(raw);
    } catch {
      return null;
    }
  }

  #restorePositions() {
    this.querySelectorAll("dtw-window[id]").forEach((w) => {
      if (!(w instanceof HTMLElement) || !w.id) return;
      const pos = this.#loadPosition(w.id);
      if (pos) {
        w.style.left = pos.left;
        w.style.top = pos.top;
      }
    });
  }

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();

    this.#restorePositions();

    // Events
    this.root().addEventListener("mousedown", this.focusOnWindow);
    this.root().addEventListener("dtw-window-start-move", this.windowMoveStart);

    // Winamp stuff
    document.body.addEventListener(
      "mousedown",
      this.bringWinampToFront.bind(this),
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

    this.root().removeEventListener("mousedown", this.focusOnWindow);
    this.root().removeEventListener(
      "dtw-window-start-move",
      this.windowMoveStart,
    );

    document.body.removeEventListener(
      "mousedown",
      this.bringWinampToFront.bind(this),
    );
  }

  /**
   * @param {MouseEvent} event
   */
  bringWinampToFront(event) {
    if (event.target instanceof HTMLElement) {
      const winamp = event.target?.closest("dtw-winamp");
      if (winamp instanceof HTMLElement) {
        this.#lastZindex++;
        winamp.style.zIndex = this.#lastZindex.toString();
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
    this.activateWindow(activeId);
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
        const target = ogEvent.detail.element;

        if (target) {
          target.style.left = `${x}px`;
          target.style.top = `${y}px`;
        }
      }
    }, {
      updateArguments: true,
    });

    const stopMove = () => {
      document.removeEventListener("mousemove", moveFn);
      document.removeEventListener("mouseup", stopMove);
      document.removeEventListener("mouseleave", stopMove);

      const target = ogEvent.detail.element;
      if (target instanceof HTMLElement && target.id) {
        this.#savePosition(target.id, target.style.left, target.style.top);
      }
    };

    document.addEventListener("mousemove", moveFn);
    document.addEventListener("mouseup", stopMove);
    document.addEventListener("mouseleave", stopMove);
  }

  // ACTIONS

  /**
   * @param {string | null} activeId
   */
  activateWindow(activeId) {
    this.querySelectorAll("dtw-window").forEach((w) => {
      if (w instanceof WindowElement === false) return;

      if (activeId && w.id === activeId) {
        w.activate();
      } else {
        w.deactivate();
      }
    });
  }

  /**
   * @param {string} id
   */
  toggleWindow(id) {
    const w = this.querySelector(`dtw-window#${id}`);
    if (w instanceof WindowElement === false) return;

    w.toggleAttribute("open");

    if (w.hasAttribute("open")) {
      this.activateWindow(id);
      this.#lastZindex++;
      w.style.zIndex = this.#lastZindex.toString();

      if (!this.#loadPosition(id)) {
        const placeWindow = () => {
          const dialog = w.shadowRoot?.querySelector("dialog[open]");
          if (!dialog) { requestAnimationFrame(placeWindow); return; }
          const { width, height } = dialog.getBoundingClientRect();
          if (width === 0 || height === 0) { requestAnimationFrame(placeWindow); return; }
          const index = [...this.children].indexOf(w);
          const stagger = index * 12;
          w.style.left = `${Math.round(Math.max(0, (window.innerWidth - width) / 2) + stagger)}px`;
          w.style.top = `${Math.round(Math.max(0, (window.innerHeight - height) / 2) + stagger)}px`;
          this.#savePosition(id, w.style.left, w.style.top);
        };
        requestAnimationFrame(placeWindow);
      }
    }
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

defineElement(NAME, WindowManager);
