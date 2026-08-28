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
   * @param {string | undefined} [width]
   * @param {string | undefined} [height]
   * @param {boolean | undefined} [open]
   */
  #saveState(id, left, top, width, height, open) {
    localStorage.setItem(`${STORAGE_PREFIX}${id}`, JSON.stringify({ left, top, width, height, open }));
  }

  /**
   * @param {string} id
   * @returns {{ left: string; top: string; width?: string; height?: string; open?: boolean } | null}
   */
  #loadState(id) {
    try {
      const raw = localStorage.getItem(`${STORAGE_PREFIX}${id}`);
      if (!raw) return null;
      return JSON.parse(raw);
    } catch {
      return null;
    }
  }

  #restoreState() {
    this.querySelectorAll("dtw-window[id]").forEach((w) => {
      if (!(w instanceof HTMLElement) || !w.id) return;
      const state = this.#loadState(w.id);
      if (!state) return;
      if (state.left) w.style.left = state.left;
      if (state.top) w.style.top = state.top;
      if (state.width || state.height) {
        const body = w.shadowRoot?.querySelector(".window-body");
        if (body instanceof HTMLElement) {
          if (state.width) body.style.width = state.width;
          if (state.height) body.style.height = state.height;
        }
      }
      if (state.open) {
        w.setAttribute("open", "");
        this.#lastZindex++;
        w.style.zIndex = this.#lastZindex.toString();
      }
    });
  }

  /** @type {MutationObserver | null} */
  #openObserver = null;

  #setupOpenObserver() {
    this.#openObserver = new MutationObserver((mutations) => {
      for (const mutation of mutations) {
        if (mutation.attributeName !== "open") continue;
        const target = mutation.target;
        if (!(target instanceof HTMLElement) || !target.id) continue;
        const isOpen = target.hasAttribute("open");
        const state = this.#loadState(target.id) ?? /** @type {{ left: string; top: string; width?: string; height?: string; open?: boolean }} */ ({});
        this.#saveState(
          target.id,
          state.left ?? target.style.left,
          state.top ?? target.style.top,
          state.width,
          state.height,
          isOpen,
        );
      }
    });

    this.querySelectorAll("dtw-window[id]").forEach((w) => {
      this.#openObserver?.observe(w, { attributes: true, attributeFilter: ["open"] });
    });
  }

  #resizeObservers = /** @type {Map<string, ResizeObserver>} */ (new Map());

  #setupResizeObservers() {
    this.querySelectorAll("dtw-window[id]").forEach((w) => {
      if (!(w instanceof HTMLElement) || !w.id) return;
      if (this.#resizeObservers.has(w.id)) return;

      const body = w.shadowRoot?.querySelector(".window-body");
      if (!(body instanceof HTMLElement)) return;

      let initialized = false;
      const observer = new ResizeObserver(() => {
        if (!initialized) { initialized = true; return; }
        const { width, height } = body.getBoundingClientRect();
        if (width === 0 || height === 0) return;
        const state = this.#loadState(w.id) ?? /** @type {{ left: string; top: string; width?: string; height?: string; open?: boolean }} */ ({});
        this.#saveState(
          w.id,
          state.left ?? w.style.left,
          state.top ?? w.style.top,
          `${Math.round(width)}px`,
          `${Math.round(height)}px`,
          state.open,
        );
      });

      observer.observe(body);
      this.#resizeObservers.set(w.id, observer);
    });
  }

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();

    this.#restoreState();
    this.#setupResizeObservers();
    this.#setupOpenObserver();

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

    this.#resizeObservers.forEach((observer) => observer.disconnect());
    this.#resizeObservers.clear();
    this.#openObserver?.disconnect();
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
        const state = this.#loadState(target.id) ?? /** @type {{ left: string; top: string; width?: string; height?: string; open?: boolean }} */ ({});
        this.#saveState(target.id, target.style.left, target.style.top, state.width, state.height, state.open);
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

      if (!this.#loadState(id)) {
        const placeWindow = () => {
          const dialog = w.shadowRoot?.querySelector("dialog[open]");
          if (!dialog) { requestAnimationFrame(placeWindow); return; }
          const { width, height } = dialog.getBoundingClientRect();
          if (width === 0 || height === 0) { requestAnimationFrame(placeWindow); return; }
          const index = [...this.children].indexOf(w);
          const stagger = index * 12;
          w.style.left = `${Math.round(Math.max(0, (window.innerWidth - width) / 2) + stagger)}px`;
          w.style.top = `${Math.round(Math.max(0, (window.innerHeight - height) / 2) + stagger)}px`;
          const state = this.#loadState(id) ?? /** @type {{ left: string; top: string; width?: string; height?: string; open?: boolean }} */ ({});
          this.#saveState(id, w.style.left, w.style.top, state.width, state.height, state.open);
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
