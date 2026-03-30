import { DiffuseElement } from "~/common/element.js";

/**
 * @import {ProxiedActions, Tunnel} from "~/common/worker.d.ts"
 * @import {InputElement} from "~/components/input/types.d.ts"
 * @import {Actions} from "./types.d.ts"
 */

/**
 * @typedef {{ element: InputElement, tunnel: Tunnel, worker: Worker | SharedWorker }} Input
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<Actions>}
 */
class InputConfigurator extends DiffuseElement {
  static NAME = "diffuse/configurator/input";
  static WORKER_URL = "components/configurator/input/worker.js";

  constructor() {
    super();

    /** @type {ProxiedActions<Actions>} */
    const proxy = this.workerProxy();

    this.artwork = proxy.artwork;
    this.consult = proxy.consult;
    this.detach = proxy.detach;
    this.groupConsult = proxy.groupConsult;
    this.list = proxy.list;
    this.resolve = proxy.resolve;

    this.cache = proxy.cache;
    this.cacheBlob = proxy.cacheBlob;
    this.listCached = proxy.listCached;
    this.removeFromCache = proxy.removeFromCache;
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
