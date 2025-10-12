import DiffuseElement from "@common/element.js";
import { signal } from "@common/signals.js";

/**
 * @import {RenderArg} from "@common/element.d.ts"
 * @import {Track} from "@elements/core/types.d.ts"
 *
 * @import {State} from "./browser-list.d.ts"
 */

////////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////////

class List extends DiffuseElement {
  tracks = signal(/** @type {Track[]} */ ([]));

  // STATE

  get state() {
    return {
      tracks: this.tracks,
    };
  }

  // RENDER

  /**
   * @param {RenderArg<State>} _
   */
  render({ html, state }) {
    console.log("Rendering", state.tracks());

    const list = state.tracks().map((t) =>
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
