import morphdom from "morphdom/dist/morphdom.js";
import { effect } from "@common/signals.js";

/**
 * @import {HtmlTagFunction} from "./element.d.ts"
 */

export default class DiffuseElement extends HTMLElement {
  #disposables = /** @type {Array<() => void>} */ ([]);

  #teardown() {
    this.#disposables.forEach((fn) => fn());
  }

  constructor() {
    super();
    this.morphedRender = this.morphedRender.bind(this);
  }

  morphedRender() {
    if (!("render" in this && typeof this.render === "function")) return;
    if (!("state" in this)) return;

    const tmp = this.render({
      html: this.html,
      state: this.state,
    });

    const updated = document.createElement("div");
    updated.innerHTML = tmp.trim();
    const root = this.shadowRoot ? this.shadowRoot : this;

    morphdom(
      root,
      updated,
      {
        childrenOnly: true,
      },
    );
  }

  /**
   * @param {string} _name
   * @param {string} oldValue
   * @param {string} newValue
   */
  attributeChangedCallback(_name, oldValue, newValue) {
    if (oldValue !== newValue) this.morphedRender();
  }

  /**
   * @param {() => void} fn
   */
  effect(fn) {
    this.#disposables.push(effect(fn));
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

    this.effect(() => {
      if (!("render" in this && typeof this.render === "function")) return;
      if (!("state" in this)) return;

      this.morphedRender();
    });
  }

  disconnectedCallback() {
    this.#teardown();
  }
}
