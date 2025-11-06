import { DiffuseElement } from "@common/element.js";
import { use } from "@common/worker.js";
import { outputManager } from "../../common.js";

/**
 * @import {OutputManager} from "../../types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {OutputManager<any>}
 */
class IndexedDBOutput extends DiffuseElement {
  constructor() {
    super();

    // Setup worker
    const name = `diffuse/output/polymorphic/indexed-db/${this.group}`;
    const url = "/components/output/polymorphic/indexed-db/worker.js";
    const worker = new Worker(url, { name, type: "module" });

    // Manager
    const manager = outputManager({
      tracks: {
        empty: () => [],
        get: use("getTracks", worker),
        put: use("putTracks", worker),
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
