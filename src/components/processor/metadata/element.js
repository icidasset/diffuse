import { DiffuseElement } from "@common/element.js";
import { portProvider, proxyProvider } from "@common/worker.js";

/**
 * @import {PortProviderMethod, ProxiedActions, ProxyProvider, ProxyProviderMethod, WorkerProviderMethod} from "@common/worker.d.ts"
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<Actions>}
 * @implements {WorkerProviderMethod}
 * @implements {ProxyProviderMethod<Actions>}
 */
class MetadataProcessor extends DiffuseElement {
  constructor() {
    super();

    // Setup worker
    const worker = this.worker(this.group);

    /** @type {ProxyProvider<Actions>} */
    this.proxy = proxyProvider(["supply"]);

    // Worker proxy
    this.supply = this.proxy(worker).supply;
  }

  /**
   * @param {string} [group]
   */
  worker(group) {
    const name = `diffuse/processor/metadata/${group || crypto.randomUUID()}`;
    const url = "/components/processor/metadata/worker.js";
    return new Worker(url, { name, type: "module" });
  }
}

export default MetadataProcessor;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = MetadataProcessor;
export const NAME = "dp-metadata";

customElements.define(NAME, MetadataProcessor);
