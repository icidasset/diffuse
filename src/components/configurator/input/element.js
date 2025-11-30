import { DiffuseElement, workerProxy } from "@common/element.js";
import { transfer, workerLink, workerTunnel } from "@common/worker.js";

/**
 * @import {ProxiedActions, Tunnel} from "@common/worker.d.ts"
 * @import {InputActions, InputElement} from "@components/input/types.d.ts"
 * @import {AdditionalActions} from "./types.d.ts"
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
    const proxy = workerProxy(this.workerLink);

    this.consult = proxy.consult;
    this.contextualize = proxy.contextualize;
    this.groupConsult = proxy.groupConsult;
    this.list = proxy.list;
    this.resolve = proxy.resolve;
  }

  // WORKER

  /**
   * @override
   */
  createWorker() {
    const worker = super.createWorker();

    // Wait for child elements to be rendered
    setTimeout(() => this.configureWorker(worker), 0);

    return worker;
  }

  // 🛠️

  /**
   * @param {Worker | SharedWorker} worker
   */
  async configureWorker(worker) {
    const inputs = await this.inputTunnels();

    // Check if any inputs are present
    if (inputs.length === 0) return;

    // Configure worker with input ports
    const args = transfer({
      ports: Object.fromEntries(inputs.map((input) => {
        return [input.element.SCHEME, input.tunnel.port];
      })),
    }, inputs.map((i) => i.tunnel.port));

    /** @type {ProxiedActions<AdditionalActions>} */
    const proxy = workerProxy(() => workerLink(worker));
    proxy.configure(args);
  }

  async inputTunnels() {
    const inputElements = this.children;
    const inputs = await Array.from(inputElements).reduce(
      /**
       * @param {Promise<Array<Input>>} acc
       * @param {Element} el
       */
      async (acc, el) => {
        const rec = await acc;
        await customElements.whenDefined(el.localName);

        const element = /** @type {InputElement} */ (el);
        const worker = element.worker();
        const tunnel = workerTunnel(worker);

        const item = {
          element,
          tunnel,
          worker,
        };

        return [...rec, item];
      },
      Promise.resolve([]),
    );

    return inputs;
  }
}

export default InputConfigurator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = InputConfigurator;
export const NAME = "dc-input";

customElements.define(NAME, CLASS);
