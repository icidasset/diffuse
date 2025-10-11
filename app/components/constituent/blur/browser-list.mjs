import BaseElement from "@enhance/base-element";
import ElementMixin from "@enhance/shadow-element-mixin";
import EventHandlerMixin from "@enhance/event-handler-mixin";
import MorphdomMixin from "@enhance/morphdom-mixin";
import TemplateMixin from "@enhance/template-mixin";

import { effect, signal } from "../../../common/signals/index.mjs";

/**
 * @import {Signal} from "../../../common/signals/types.d.ts"
 * @import {Track} from "../../../common/core/types.d.ts"
 */

class List extends MorphdomMixin(EventHandlerMixin(TemplateMixin(BaseElement))) {
  #teardown = () => {};

  /**
   * @type {Signal<Track[]>}
   */
  tracks = signal(["a"]);

  // LIFECYCLE

  connectedCallback() {
    this.#teardown = effect(() => {
      const state = { ...this.state, tracks: this.tracks() };
      this.innerHTML = this.render({ html: this.html, state });
    });
  }

  disconnectedCallback() {
    this.#teardown();
  }

  // RENDER

  render({ html, state }) {
    console.log("Rendering", state.tracks);

    const list = (state.tracks || []).map((t) => html`<div instanceID="${t.id}">${t}</div>`);

    console.log(list);

    return html`
      <style>
        :host {
          color: blue;
        }
      </style>
      <section>${list.join("")}<br /></section>
    `;
  }
}

customElements.define("constituent-blur-browser-list", List);

export { List as ConstituentBlurBrowserList };
export default List;
