import { DiffuseElement } from "@common/element.js";
import { signal } from "@common/signal.js";

/**
 * @import {RenderArg} from "@common/element.d.ts"
 * @import {State} from "./types.d.ts"
 * @import {Track} from "@component/core/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class BrowserList extends DiffuseElement {
  tracks = signal(/** @type {Track[]} */ ([]));

  // STATE

  get state() {
    return {
      tracks: this.tracks.get,
    };
  }

  // RENDER

  /**
   * @param {RenderArg<State>} _
   */
  render({ html, state }) {
    console.log("Rendering", state.tracks());

    const list = state.tracks().map(
      /**
       * @param {Track} t
       * @param {number} idx
       */
      (t, idx) =>
        html`
          <div id="track-${idx}">${t}</div>
        `,
    ).join("");

    return html`
      <style>
      :host {
        color: blue;
      }
      </style>
      <section>${list}</section>
    `;
  }
}

export default BrowserList;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

customElements.define("dcb-browser-list", BrowserList);
