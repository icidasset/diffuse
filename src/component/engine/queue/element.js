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

    // TODO:
    // const worker = new SharedWorker(new URL("./worker.js", import.meta.url), {
    //   type: "module",
    // });
    //
    // const port = worker.port;

    const worker = new Worker(new URL("./worker.js", import.meta.url), {
      type: "module",
    });

    const port = worker;

    listen("future", this.future, port);
    listen("now", this.now, port);
    listen("past", this.past, port);

    this.add = use("add", port);

    this.load(port);
  }

  /**
   * @param {Worker} port
   */
  async load(port) {
    const f = await use("future", port)();
    const n = await use("now", port)();
    const p = await use("past", port)();

    this.future(f);
    this.now(n);
    this.past(p);
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
