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
class S3Input extends DiffuseElement {
  constructor() {
    super();

    // Setup worker
    const worker = this.worker(this.group);

    /** @type {ProxyProvider<InputActions & { demo: () => Promise<void> }>} */
    this.proxy = proxyProvider([
      "consult",
      "contextualize",
      "groupConsult",
      "list",
      "resolve",

      "demo",
    ]);

    // Worker proxy
    const w = this.proxy(worker);

    this.consult = w.consult;
    this.contextualize = w.contextualize;
    this.groupConsult = w.groupConsult;
    this.list = w.list;
    this.resolve = w.resolve;

    this.demo = w.demo;

    // Provide a channel to the worker
    this.port = portProvider(worker);
  }

  /**
   * @param {string} [group]
   */
  worker(group) {
    const name = `diffuse/input/s3/${group || crypto.randomUUID()}`;
    const url = import.meta.resolve(
      "./components/input/s3/worker.js",
    );

    return new Worker(url, { name, type: "module" });
  }
}

export default S3Input;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = S3Input;
export const NAME = "di-s3";

customElements.define(NAME, CLASS);
