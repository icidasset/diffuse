import { DiffuseElement } from "@common/element.js";

import "@components/configurator/input/element.js";
import "@components/input/opensubsonic/element.js";
import "@components/input/s3/element.js";

/**
 * @import {RenderArg} from "@common/element.d.ts"
 * @import {InputActions, InputElement} from "@components/input/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class InputOrchestrator extends DiffuseElement {
  static NAME = "diffuse/orchestrator/input";
  static WORKER_URL = "components/configurator/input/worker.js";

  /**
   * @returns {InputElement}
   */
  get input() {
    /** @type {InputElement | null} */
    const input = this.querySelector("dc-input");

    if (!input) throw new Error("Input orchestrator did not render yet.");
    return input;
  }

  // PROXY INPUT ACTIONS

  consult = /** @type {InputActions["consult"]} */ (...args) =>
    this.input.consult(...args);

  contextualize = /** @type {InputActions["contextualize"]} */ (...args) =>
    this.input.contextualize(...args);

  groupConsult = /** @type {InputActions["groupConsult"]} */ (...args) =>
    this.input.groupConsult(...args);

  list = /** @type {InputActions["list"]} */ (...args) =>
    this.input.list(...args);

  resolve = /** @type {InputActions["resolve"]} */ (...args) =>
    this.input.resolve(...args);

  // PROXY OTHER FUNCTIONS

  /** @override */
  dependencies() {
    return this.input.dependencies();
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    return html`
      <dc-input>
        <di-opensubsonic></di-opensubsonic>
        <di-s3></di-s3>
      </dc-input>
    `;
  }
}

export default InputOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = InputOrchestrator;
export const NAME = "do-input";

customElements.define(NAME, CLASS);
