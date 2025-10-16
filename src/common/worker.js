import { defineWorkerFn, useWorkerFn } from "@mys/worker-fn";
import { getTransferables } from "@okikio/transferables";

import { xxh32 } from "xxh32";

/**
 * @import {NodeWorkerOrNodeMessagePort} from "@mys/m-rpc";
 * @import {Announcement} from "./worker.d.ts"
 */

export const define = defineWorkerFn;
export const use = useWorkerFn;

/**
 * @template T
 * @param {string} name
 * @param {T} args
 */
export function announce(name, args) {
  const transferables = getTransferables(args);
  globalThis.postMessage(constructMsg(name, args), { transfer: transferables });
}

/**
 * @template T
 * @param {string} name
 * @param {(args: T) => void} fn
 * @param {Worker | NodeWorkerOrNodeMessagePort} context
 */
export function listen(
  name,
  fn,
  context = globalThis,
) {
  context.addEventListener(
    "message",
    /** @param {MessageEvent} event */ (event) => {
      const announcement = /** @type {Announcement<T>} */ (event.data);
      const { ns, type } = announcement;

      if (announcement.name !== name) return;
      if (ns !== ANNOUNCEMENT || type !== ANNOUNCEMENT) return;

      fn(announcement.args);
    },
  );
}

////////////////////////////////////////////
// PRIVATE
////////////////////////////////////////////

const ANNOUNCEMENT = "announcement";

/**
 * @template T
 * @param {string} name
 * @param {T} args
 * @returns {Announcement<T>}
 */
function constructMsg(name, args) {
  return {
    ns: ANNOUNCEMENT,
    name,
    key: xxh32(crypto.randomUUID()),

    type: ANNOUNCEMENT,
    args,
  };
}
