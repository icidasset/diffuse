import morphdom from "morphdom";

/**
 * @import {HtmlTagFunction, RenderArg} from "./element.d.ts"
 */

/**
 * @template [State={}]
 */
export default class DiffuseElement extends HTMLElement {
  constructor() {
    super();
    this.process = this.process.bind(this);
  }

  process() {
    const tmp = this.render({
      html: this.html,
      state: this.state,
    });

    const updated = document.createElement("div");
    updated.innerHTML = tmp.trim();
    const root = this.shadowRoot ? this.shadowRoot : this;

    /* @ts-ignore */
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
    if (oldValue !== newValue) this.process();
  }

  /**
   * @type {HtmlTagFunction}
   */
  html(strings, ...values) {
    return String.raw({ raw: strings }, ...values);
  }

  // TO OVERRIDE

  /**
   * @param {RenderArg<State>} _arg
   */
  render(_arg) {
    return "";
  }

  /**
   * @returns {State}
   */
  get state() {
    /* @ts-ignore */
    return {};
  }
}
