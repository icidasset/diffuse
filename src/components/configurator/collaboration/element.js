import { defineElement, DiffuseElement } from "~/common/element.js";

/**
 * @import {CollaborationConfiguratorActions} from "@specs/components/configurator/collaboration/types.d.ts"
 * @import {CollaborationSourceElement} from "@specs/components/collaboration/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Holds the available collaboration sources. Append source elements as
 * children; each source declares the hint prefix it handles through `PREFIX`.
 *
 * @implements {CollaborationConfiguratorActions}
 */
class CollaborationConfigurator extends DiffuseElement {
  static NAME = "diffuse/configurator/collaboration";

  // SOURCES

  sources() {
    /** @type {Record<string, CollaborationSourceElement>} */
    const sources = {};

    for (const element of this.children) {
      const source = /** @type {CollaborationSourceElement} */ (element);
      if (typeof source.PREFIX !== "string" || source.PREFIX.length === 0) {
        continue;
      }

      sources[source.PREFIX] = source;
    }

    return sources;
  }

  /**
   * @param {string | undefined} hint
   */
  sourceFor(hint) {
    if (typeof hint !== "string") return undefined;

    const prefix = hint.split("/")[0];
    return prefix ? this.sources()[prefix] : undefined;
  }

  // WORKERS

  /**
   * @override
   */
  dependencies() {
    return this.sources();
  }
}

export default CollaborationConfigurator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = CollaborationConfigurator;
export const NAME = "dc-collaboration";

defineElement(NAME, CLASS);
