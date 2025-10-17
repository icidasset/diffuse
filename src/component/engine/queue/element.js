import DiffuseElement from "@common/element.js";
import { signal } from "@common/signal.js";
import { listen, use } from "@common/worker.js";

/**
 * @import {Actions, Item, Signals} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {Actions}
 * @implements {Signals}
 */
class QueueEngine extends DiffuseElement {
  constructor() {
    super();

    // Setup shared worker
    const worker = new SharedWorker(new URL("./worker.js", import.meta.url), {
      type: "module",
    });

    const port = worker.port;
    port.start();

    // Sync data with worker
    listen("future", this.future, port);
    listen("now", this.now, port);
    listen("past", this.past, port);

    // Worker proxy
    this.add = use("add", port);
  }

  // SIGNALS

  future = signal(/** @type {Array<Item>} */ ([]));
  now = signal(/** @type {Item | null} */ (null));
  past = signal(/** @type {Array<Item>} */ ([]));
}

export default QueueEngine;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const NAME = "de-queue";
customElements.define(NAME, QueueEngine);
