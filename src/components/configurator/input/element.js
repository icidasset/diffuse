import { DiffuseElement, whenElementsDefined } from "@common/element.js";

/**
 * @import {ProxiedActions, Tunnel} from "@common/worker.d.ts"
 * @import {InputActions, InputElement} from "@components/input/types.d.ts"
 */

/**
 * @typedef {{ element: InputElement, tunnel: Tunnel, worker: Worker | SharedWorker }} Input
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<InputActions>}
 */
class InputConfigurator extends DiffuseElement {
  static NAME = "diffuse/configurator/input";
  static WORKER_URL = "components/configurator/input/worker.js";

  constructor() {
    super();

    /** @type {ProxiedActions<InputActions>} */
    const proxy = this.workerProxy();

    this.consult = proxy.consult;
    this.contextualize = proxy.contextualize;
    this.groupConsult = proxy.groupConsult;
    this.list = proxy.list;
    this.resolve = proxy.resolve;
  }

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();
    await whenElementsDefined(this.inputs());
  }

  // WORKERS

  /**
   * @override
   */
  dependencies() {
    return this.inputs();
  }

  inputs() {
    return Object.fromEntries(
      Array.from(this.children).map((element) => {
        const input = /** @type {InputElement} */ (element);
        return [input.SCHEME, input];
      }),
    );
  }
}

export default InputConfigurator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = InputConfigurator;
export const NAME = "dc-input";

customElements.define(NAME, CLASS);
