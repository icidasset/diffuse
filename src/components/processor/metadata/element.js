import { DiffuseElement } from "@common/element.js";
import { portProvider, proxyProvider } from "@common/worker.js";

/**
 * @import {PortProviderMethod, ProxiedActions, ProxyProvider, ProxyProviderMethod} from "@common/worker.d.ts"
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<Actions>}
 * @implements {PortProviderMethod}
 * @implements {ProxyProviderMethod<Actions>}
 */
class MetadataProcessor extends DiffuseElement {
  constructor() {
    super();

    // Setup worker
    const name = `diffuse/processor/metadata/${this.group}`;
    const url = "/components/processor/metadata/worker.js";
    const worker = new Worker(url, { name, type: "module" });

    /** @type {ProxyProvider<Actions>} */
    this.proxy = proxyProvider(["supply"]);

    // Worker proxy
    this.supply = this.proxy(worker).supply;

    // Provide a channel to the worker
    this.port = portProvider(worker);
  }
}

export default MetadataProcessor;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = MetadataProcessor;
export const NAME = "dp-metadata";

customElements.define(NAME, MetadataProcessor);
