import { DiffuseElement } from "@common/element.js";
import { signal } from "@common/signal.js";
import { listen } from "@common/worker.js";

/**
 * @import {ProxiedActions} from "@common/worker.d.ts";
 * @import {Actions, Item, State} from "./types.d.ts"
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

    /** @type {ProxiedActions<Actions & State>} */
    this.proxy = this.workerProxy();

    this.add = this.proxy.add;
    this.fill = this.proxy.fill;
    this.shift = this.proxy.shift;
    this.supply = this.proxy.supply;
    this.unshift = this.proxy.unshift;
  }

  // SIGNALS

  #future = signal(/** @type {Array<Item>} */ ([]));
  #now = signal(/** @type {Item | null} */ (null));
  #past = signal(/** @type {Array<Item>} */ ([]));
  #supplyFingerprint = signal(/** @type {string | undefined} */ (undefined));

  // STATE

  future = this.#future.get;
  now = this.#now.get;
  past = this.#past.get;
  supplyFingerprint = this.#supplyFingerprint.get;

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
    listen("supplyFingerprint", this.#supplyFingerprint.set, link);

    // Fetch current data state
    this.proxy.future().then(this.#future.set);
    this.proxy.now().then(this.#now.set);
    this.proxy.past().then(this.#past.set);
    this.proxy.supplyFingerprint().then(this.#supplyFingerprint.set);
  }
}

export default QueueEngine;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = QueueEngine;
export const NAME = "de-queue";

customElements.define(NAME, QueueEngine);
