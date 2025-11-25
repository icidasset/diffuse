import { DiffuseElement } from "@common/element.js";
import { signal } from "@common/signal.js";
import { listen, use, workerProxy } from "@common/worker.js";
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
  static NAME = "diffuse/engine/queue";
  static WORKER_URL = "components/engine/queue/worker.js";

  constructor() {
    super();

    /** @type {ProxiedActions<Actions>} */
    const p = workerProxy(this.workerLink);

    this.add = p.add;
    this.fill = p.fill;
    this.pool = p.pool;
    this.shift = p.shift;
    this.unshift = p.unshift;
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

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    // Sync data with worker
    const link = this.workerLink();

    // Listen for remote data changes
    listen("future", this.#future.set, link);
    listen("now", this.#now.set, link);
    listen("past", this.#past.set, link);
    listen("poolHash", this.#poolHash.set, link);

    // TODO: Fetch current data state
    // use("future", link)().then(this.#future.set);
    // use("now", link)().then(this.#now.set);
    // use("past", link)().then(this.#past.set);
    // use("poolHash", link)().then(this.#poolHash.set);
  }
}

export default QueueEngine;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = QueueEngine;
export const NAME = "de-queue";

customElements.define(NAME, QueueEngine);
