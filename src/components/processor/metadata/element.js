import { DiffuseElement } from "@common/element.js";
import { workerProxy } from "@common/worker.js";

/**
 * @import {PortProviderMethod, ProxiedActions, ProxyProvider, ProxyProviderMethod, WorkerProviderMethod} from "@common/worker.d.ts"
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<Actions>}
 */
class MetadataProcessor extends DiffuseElement {
  static NAME = "diffuse/processor/metadata";
  static WORKER_URL = "components/processor/metadata/worker.js";

  constructor() {
    super();

    /** @type {ProxiedActions<Actions>} */
    const p = workerProxy(this.workerLink);

    // Worker proxy
    this.supply = p.supply;
  }
}

export default MetadataProcessor;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = MetadataProcessor;
export const NAME = "dp-metadata";

customElements.define(NAME, MetadataProcessor);
