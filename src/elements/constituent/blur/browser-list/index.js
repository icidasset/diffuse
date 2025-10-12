import DiffuseElement from "@common/element.js";
import { signal } from "@common/signals.js";

/**
 * @import {RenderArg} from "@common/element.d.ts"
 * @import {Track} from "@elements/core/types.d.ts"
 *
 * @import {State} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class BrowserList extends DiffuseElement {
  tracks = signal(/** @type {Track[]} */ ([]));

  // STATE

  get state() {
    return {
      tracks: this.tracks,
    };
  }

  // LIFECYCLE

  /**
   * @override
   *
   * TODO: Remove, just an example.
   */
  connectedCallback() {
    super.connectedCallback();
    this.effect(() => {});
  }

  // RENDER

  /**
   * @param {RenderArg<State>} _
   */
  render({ html, state }) {
    console.log("Rendering", state.tracks());

    const list = state.tracks().map((t, idx) =>
      html`
        <div id="track-${idx}">${t}</div>
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

export { BrowserList as ConstituentBlurBrowserList };
export default BrowserList;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

customElements.define("constituent-blur-browser-list", BrowserList);
