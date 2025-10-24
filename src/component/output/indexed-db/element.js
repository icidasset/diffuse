import { DiffuseElement } from "@common/element.js";
import { use } from "@common/worker.js";
import { outputManager } from "../common.js";

/**
 * @import {OutputActions, OutputManager, Track} from "@component/core/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {OutputManager}
 */
class IndexedDBOutput extends DiffuseElement {
  constructor() {
    super();

    // Setup worker
    const name = `diffuse/output/indexed-db/${this.group}`;
    const url = "/component/output/indexed-db/worker.js";
    const worker = new Worker(url, { name, type: "module" });

    // Manager
    const manager = outputManager({
      tracks: {
        get: () => {
          const fn = use("getTracks", worker);
          console.log("Call", fn, worker);
          return fn();
        },
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
export const NAME = "do-indexed-db";

customElements.define(NAME, IndexedDBOutput);
