import { defineWorkerFn, useWorkerFn } from "@mys/worker-fn";
import { getTransferables } from "@okikio/transferables";
import { xxh32 } from "xxh32";

/**
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
 */
export function listen(name, fn) {
  globalThis.addEventListener("message", (event) => {
    const announcement =
      /** @type {Announcement<T>} */ (/** @type {unknown} */ (event));
    const { ns, type } = announcement;

    if (announcement.name !== name) return;
    if (ns !== ANNOUNCEMENT || type !== ANNOUNCEMENT) return;

    fn(announcement.args);
  });
}

// PRIVATE

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
