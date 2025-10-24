import { DiffuseElement } from "@common/element.js";
import { use } from "@common/worker.js";

/**
 * @import {OutputActions} from "@component/core/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {OutputActions}
 */
class IndexedDBOutput extends DiffuseElement {
  constructor() {
    super();

    // Setup worker
    const name = `diffuse/output/indexed-db/${this.group}`;
    const url = "/component/output/indexed-db/worker.js";
    const worker = new Worker(url, { name, type: "module" });

    // Worker proxy
    this.getTracks = use("getTracks", worker);
    this.putTracks = use("putTracks", worker);
  }
}

export default IndexedDBOutput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = IndexedDBOutput;
export const NAME = "do-indexed-db";

customElements.define(NAME, IndexedDBOutput);
