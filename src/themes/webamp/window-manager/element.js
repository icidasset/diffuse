import { DiffuseElement } from "@common/element.js";
import { signal } from "@common/signal.js";
import { debounceMicrotask } from "@vicary/debounce-microtask";

import WindowElement from "../window/element.js"

/**
 * @import {RenderArg} from "@common/element.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class WindowManager extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });

    this.focusOnWindow = this.focusOnWindow.bind(this)
    this.windowMoveStart = this.windowMoveStart.bind(this)
  }

  // SIGNALS

  $activeWindow = signal(/** @type {string | null} */ (null));
  #lastZindex = 1000;

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();

    // Events
    this.root().addEventListener("mousedown", this.focusOnWindow);
    this.root().addEventListener("dtw-window-start-move", this.windowMoveStart);

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

    this.root().removeEventListener("mousedown", this.focusOnWindow);
    this.root().removeEventListener("dtw-window-start-move", this.windowMoveStart);

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
    this.activateWindow(activeId)
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
    };

    document.addEventListener("mousemove", moveFn);
    document.addEventListener("mouseup", stopMove);
    document.addEventListener("mouseleave", stopMove);
  }

  // ACTIONS

  /**
   * @param {string} id
   */
  activateWindow(id) {
    this.querySelectorAll("dtw-window").forEach(w => {
      if (w instanceof WindowElement === false) return

      if (activeId && w.id === activeId) {
        w.activate();
      } else {
        w.deactivate();
      }
    })
  }

  /**
   * @param {string} id
   */
  toggleWindow(id) {
    const w = this.root().querySelector(`dtw-window#${id}`)
    if (w instanceof WindowElement === false) return

    w.toggleAttribute("open")

    if (w.hasAttribute("open")) {
      this.activateWindow(id)
      this.#lastZindex++;
      w.style.zIndex = this.#lastZindex.toString();
    }
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    return html`
      <link rel="stylesheet" href="../../styles/vendor/98.css" />

      <style>
      :host {
        user-select: none;
      }

      dtw-window {
        left: 12px;
        position: absolute;
        top: 12px;
        z-index: 999;

        /* Waiting on https://developer.mozilla.org/en-US/docs/Web/CSS/sibling-index#browser_compatibility */
        &:nth-child(1) {
          left: 24px;
          top: 24px;
        }

        &:nth-child(2) {
          left: 36px;
          top: 36px;
        }

        &:nth-child(3) {
          left: 48px;
          top: 48px;
        }

        &:nth-child(4) {
          left: 60px;
          top: 60px;
        }

        &:nth-child(5) {
          left: 72px;
          top: 72px;
        }

        &:nth-child(6) {
          left: 84px;
          top: 84px;
        }

        &:nth-child(7) {
          left: 96px;
          top: 96px;
        }

        &:nth-child(8) {
          left: 108px;
          top: 108px;
        }

        &:nth-child(9) {
          left: 120px;
          top: 120px;
        }
      }
      </style>

      <!-- INPUT -->
      <dtw-window id="input-window">
        <span slot="title-icon"><img src="../../images/icons/windows_98/cd_audio_cd_a-0.png" height="14" /></span>
        <span slot="title">Manage audio inputs</span>
        <p>👀</p>
      </dtw-window>

      <!-- OUTPUT -->
      <dtw-window id="output-window">
        <span slot="title-icon"><img src="../../images/icons/windows_98/computer_user_pencil-0.png" height="14" /></span>
        <span slot="title">Manage user data</span>

        <form>
          <p>Where do you want to keep your data?</p>
          <div class="field-row">
            <input id="idb-json" type="radio" checked />
            <label for="idb-json">Local only</label>
          </div>
        </form>
      </dtw-window>

      <!-- BROWSER -->
      <dtw-window id="browser-window" open>
        <span slot="title-icon"><img src="../../images/icons/windows_98/directory_explorer-4.png" height="14" /></span>
        <span slot="title">Browse collection</span>
        <dtw-browser
          input-selector="#input"
          output-selector="#output"
          queue-engine-selector="de-queue"
        ></dtw-browser>
      </dtw-window>
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
