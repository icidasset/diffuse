import { DiffuseElement } from "@common/element.js";
import { use, workerProxy } from "@common/worker.js";
import { outputManager } from "../../common.js";

/**
 * @import {ProxiedActions, ProxyProvider} from "@common/worker.d.ts"
 * @import {OutputManager, OutputWorkerActions} from "../../types.d.ts"
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

    /** @type {ProxiedActions<OutputWorkerActions>} */
    const p = workerProxy(this.workerLink);

    // Manager
    const manager = outputManager({
      tracks: {
        empty: () => [],
        get: p.getTracks,
        put: p.putTracks,
      },
    });

    this.tracks = manager.tracks;
  }
}

export default IndexedDBOutput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = IndexedDBOutput;
export const NAME = "dop-indexed-db";

customElements.define(NAME, IndexedDBOutput);
