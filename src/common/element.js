import morphdom from "morphdom/dist/morphdom.js";
import { effect } from "@common/signals.js";

/**
 * @import {HtmlTagFunction} from "./element.d.ts"
 */

export default class DiffuseElement extends HTMLElement {
  #teardown = () => {};

  constructor() {
    super();
    this.process = this.process.bind(this);
  }

  process() {
    if (!("render" in this && typeof this.render === "function")) return;
    if (!("state" in this)) return;

    const tmp = this.render({
      html: this.html,
      state: this.state,
    });

    const updated = document.createElement("div");
    updated.innerHTML = tmp.trim();
    const root = this.shadowRoot ? this.shadowRoot : this;

    /** @type {Node} */
    const result = morphdom(
      root,
      updated,
      {
        childrenOnly: true,
      },
    );

    return result;
  }

  /**
   * @param {string} _name
   * @param {string} oldValue
   * @param {string} newValue
   */
  attributeChangedCallback(_name, oldValue, newValue) {
    if (oldValue !== newValue) this.process();
  }

  /**
   * @type {HtmlTagFunction}
   */
  html(strings, ...values) {
    return String.raw({ raw: strings }, ...values);
  }

  // LIFECYCLE

  connectedCallback() {
    if (!("render" in this && typeof this.render === "function")) return;

    this.#teardown = effect(() => {
      if (!("render" in this && typeof this.render === "function")) return;
      if (!("state" in this)) return;

      this.innerHTML = this.render({ html: this.html, state: this.state });
    });
  }

  disconnectedCallback() {
    this.#teardown();
  }
}
