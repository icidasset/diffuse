import morphdom from "morphdom/dist/morphdom.js";
import { effect } from "@common/signal.js";

/**
 * @import {HtmlTagFunction, MorphOptions} from "./element.d.ts"
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

  /**
   * @param {string} _name
   * @param {string} oldValue
   * @param {string} newValue
   */
  attributeChangedCallback(_name, oldValue, newValue) {
    if (oldValue !== newValue) this.morphedRender();
  }

  /**
   * Effect helper that automatically disposes it
   * when this element is removed from the DOM.
   *
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

  /**
   * Avoid replacing the whole subtree,
   * morph the existing DOM into the new given tree.
   */
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
        ...this.morphOptions,
        childrenOnly: true,
      },
    );
  }

  // MORPH STUFF

  /** @type {MorphOptions} */
  morphOptions = {};

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
