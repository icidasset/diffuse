import DiffuseElement from "@common/element.js";
import { effect, signal } from "@common/signals.js";

/**
 * @import {RenderArg} from "@common/element.d.ts"
 * @import {Signal} from "@common/signals.d.ts"
 * @import {Track} from "@elements/core/types.d.ts"
 */

/**
 * @typedef {{ tracks: Signal<Track[]> }} State
 */

/**
 * @type {Track[]}
 */
const TRACKS = [];

////////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////////

class List extends DiffuseElement {
  #teardown = () => {};

  /**
   * @type {Signal<Track[]>}
   */
  tracks = signal(TRACKS);

  // STATE

  /**
   * @override
   * @returns {State}
   */
  get state() {
    return {
      tracks: this.tracks,
    };
  }

  // LIFECYCLE

  connectedCallback() {
    this.#teardown = effect(() => {
      this.innerHTML = this.render({ html: this.html, state: this.state });
    });
  }

  disconnectedCallback() {
    this.#teardown();
  }

  // RENDER

  /**
   * @override
   * @param {RenderArg<State>} _
   */
  render({ html, state }) {
    console.log("Rendering", state.tracks);

    const list = (state.tracks() || []).map((t) =>
      html`
        <div id="track-${t.id}">${t}</div>
      `
    );

    return html`
      <style>
      :host {
        color: blue;
      }
      </style>
      <section>${list.join("")}</section>
    `;
  }
}

export { List as ConstituentBlurBrowserList };
export default List;

////////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////////

customElements.define("constituent-blur-browser-list", List);
