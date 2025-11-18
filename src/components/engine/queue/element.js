import QS from "query-string";

import { DiffuseElement } from "@common/element.js";
import { signal } from "@common/signal.js";
import { listen, proxyProvider, use } from "@common/worker.js";
import { hash } from "@common/index.js";

/**
 * @import {ProxiedActions, ProxyProvider} from "@common/worker.d.ts";
 * @import {Actions, Item} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<Actions>}
 */
class QueueEngine extends DiffuseElement {
  constructor() {
    super();

    // Query
    const query = QS.stringify({
      "fill": this.getAttribute("fill"),
    });

    // Setup worker
    const name = `diffuse/engine/queue/${this.group}`;
    const url = `/components/engine/queue/worker.js?${query}`;

    let port;

    if (this.hasAttribute("group")) {
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
    listen("poolHash", this.#poolHash.set, port);

    use("future", port)().then(this.#future.set);
    use("now", port)().then(this.#now.set);
    use("past", port)().then(this.#past.set);
    use("poolHash", port)().then(this.#poolHash.set);

    /** @type {ProxyProvider<Actions>} */
    const proxy = proxyProvider(["add", "fill", "pool", "shift", "unshift"]);

    // Worker proxy
    const w = proxy(port);

    this.add = w.add;
    this.fill = w.fill;
    this.pool = w.pool;
    this.shift = w.shift;
    this.unshift = w.unshift;
  }

  // SIGNALS

  #future = signal(/** @type {Array<Item>} */ ([]));
  #now = signal(/** @type {Item | null} */ (null));
  #past = signal(/** @type {Array<Item>} */ ([]));
  #poolHash = signal(hash([]));

  // STATE

  future = this.#future.get;
  now = this.#now.get;
  past = this.#past.get;
  poolHash = this.#poolHash.get;
}

export default QueueEngine;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = QueueEngine;
export const NAME = "de-queue";

customElements.define(NAME, QueueEngine);
