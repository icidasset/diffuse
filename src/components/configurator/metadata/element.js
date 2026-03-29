import { DiffuseElement } from "~/common/element.js";

/**
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 * @import {MetadataElement} from "~/components/metadata/types.d.ts"
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<Actions>}
 */
class MetadataConfigurator extends DiffuseElement {
  static NAME = "diffuse/configurator/metadata";
  static WORKER_URL = "components/configurator/metadata/worker.js";

  constructor() {
    super();

    /** @type {ProxiedActions<Actions>} */
    const proxy = this.workerProxy();

    this.patch = proxy.patch;
  }

  // WORKERS

  /**
   * @override
   */
  dependencies() {
    return Object.fromEntries(
      Array.from(this.children).map((element) => {
        const metadata = /** @type {MetadataElement} */ (element);
        return [metadata.localName, metadata];
      }),
    );
  }
}

export default MetadataConfigurator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = MetadataConfigurator;
export const NAME = "dc-metadata";

customElements.define(NAME, MetadataConfigurator);
