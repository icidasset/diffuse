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

    // Setup worker
    const group = this.getAttribute("group") || crypto.randomUUID();
    const isShared = this.hasAttribute("shared");
    const name = `diffuse/engine/queue/${group}`;
    const url = new URL("./worker.js", import.meta.url);

    let port;

    if (isShared) {
      const worker = new SharedWorker(url, { name, type: "module" });
      port = worker.port;
      port.start();
    } else {
      const worker = new Worker(url, { name, type: "module" });
      port = worker;
    }

    // Sync data with worker
    listen("future", this.future, port);
    listen("now", this.now, port);
    listen("past", this.past, port);

    // Worker proxy
    this.add = use("add", port);
    this.pool = use("pool", port);
    this.shift = use("shift", port);
    this.unshift = use("unshift", port);
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
