import { DiffuseElement } from "@common/element.js";
import { portProvider, proxyProvider } from "@common/worker.js";

/**
 * @import {InputActions} from "@common/types.d.ts"
 * @import {PortProviderMethod, ProxiedActions, ProxyProvider, ProxyProviderMethod} from "@common/worker.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<InputActions>}
 * @implements {PortProviderMethod}
 * @implements {ProxyProviderMethod<InputActions>}
 */
class OpensubsonicInput extends DiffuseElement {
  constructor() {
    super();

    // Setup worker
    const worker = this.worker(this.group);

    /** @type {ProxyProvider<InputActions>} */
    this.proxy = proxyProvider([
      "consult",
      "contextualize",
      "groupConsult",
      "list",
      "resolve",
    ]);

    // Worker proxy
    const w = this.proxy(worker);

    this.consult = w.consult;
    this.contextualize = w.contextualize;
    this.groupConsult = w.groupConsult;
    this.list = w.list;
    this.resolve = w.resolve;

    // Provide a channel to the worker
    this.port = portProvider(worker);
  }

  /**
   * @param {string} [group]
   */
  worker(group) {
    const name = `diffuse/input/opensubsonic/${group || crypto.randomUUID()}`;
    const url = import.meta.resolve(
      "./components/input/opensubsonic/worker.js",
    );

    return new Worker(url, { name, type: "module" });
  }
}

export default OpensubsonicInput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = OpensubsonicInput;
export const NAME = "di-opensubsonic";

customElements.define(NAME, OpensubsonicInput);
