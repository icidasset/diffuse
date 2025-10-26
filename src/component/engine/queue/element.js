import { DiffuseElement } from "@common/element.js";
import { signal } from "@common/signal.js";
import { listen, use } from "@common/worker.js";

/**
 * @import {ActionsProxied, Item} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ActionsProxied}
 */
class QueueEngine extends DiffuseElement {
  constructor() {
    super();

    // Is shared?
    const isShared = this.hasAttribute("group");

    // Setup worker
    const name = `diffuse/engine/queue/${this.group}`;
    const url = "/component/engine/queue/worker.js";

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
    listen("future", this.#future.set, port);
    listen("now", this.#now.set, port);
    listen("past", this.#past.set, port);

    /** @type {ActionsProxied['add']} */
    this.add = use("add", port);

    /** @type {ActionsProxied['pool']} */
    this.pool = use("pool", port);

    /** @type {ActionsProxied['shift']} */
    this.shift = use("shift", port);

    /** @type {ActionsProxied['unshift']} */
    this.unshift = use("unshift", port);
  }

  // SIGNALS

  #future = signal(/** @type {Array<Item>} */ ([]));
  #now = signal(/** @type {Item | null} */ (null));
  #past = signal(/** @type {Array<Item>} */ ([]));

  // STATE

  future = this.#future.get;
  now = this.#now.get;
  past = this.#past.get;
}

export default QueueEngine;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = QueueEngine;
export const NAME = "de-queue";

customElements.define(NAME, QueueEngine);
