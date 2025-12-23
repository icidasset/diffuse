import { DiffuseElement } from "@common/element.js";
import { outputManager } from "../../common.js";

/**
 * @import {ProxiedActions} from "@common/worker.d.ts"
 * @import {OutputManager, OutputWorkerActions} from "../../types.d.ts"
 * @import {SupportedDataTypes} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {OutputManager<any>}
 */
class IndexedDBOutput extends DiffuseElement {
  static NAME = "diffuse/output/polymorphic/indexed-db";
  static WORKER_URL = "components/output/polymorphic/indexed-db/worker.js";

  constructor() {
    super();

    /** @type {ProxiedActions<OutputWorkerActions<SupportedDataTypes>>} */
    const p = this.workerProxy();

    /** @type {OutputManager<SupportedDataTypes>} */
    const manager = outputManager({
      init: this.whenConnected.bind(this),
      tracks: {
        empty: () => undefined,
        get: () => p.get({ name: this.#cat("tracks") }),
        put: (data) => p.put({ name: this.#cat("tracks"), data }),
      },
    });

    this.tracks = manager.tracks;
  }

  // 🛠️

  /** @param {string} name */
  #cat(name) {
    const namespace = this.hasAttribute("namespace")
      ? this.getAttribute("namespace") + "/"
      : "";
    return `${namespace}${name}`;
  }
}

export default IndexedDBOutput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = IndexedDBOutput;
export const NAME = "dop-indexed-db";

customElements.define(NAME, IndexedDBOutput);
