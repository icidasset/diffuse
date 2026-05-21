import { defineElement, DiffuseElement } from "~/common/element.js";

/**
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 * @import {ArtworkElement} from "@specs/components/artwork/types.d.ts"
 * @import {Actions} from "@specs/components/configurator/artwork/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<Actions>}
 */
class ArtworkConfigurator extends DiffuseElement {
  static NAME = "diffuse/configurator/artwork";
  static WORKER_URL = "components/configurator/artwork/worker.js";

  constructor() {
    super();

    /** @type {ProxiedActions<Actions>} */
    const proxy = this.workerProxy();

    this.get = proxy.get;
  }

  // WORKERS

  /**
   * @override
   */
  dependencies() {
    return Object.fromEntries(
      Array.from(this.children).map((element) => {
        const artwork = /** @type {ArtworkElement} */ (element);
        return [artwork.localName, artwork];
      }),
    );
  }
}

export default ArtworkConfigurator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ArtworkConfigurator;
export const NAME = "dc-artwork";

defineElement(NAME, ArtworkConfigurator);
