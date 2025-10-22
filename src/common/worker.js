import { defineWorkerFn, useWorkerFn } from "@mys/worker-fn";
import { getTransferables } from "@okikio/transferables";

import { xxh32 } from "xxh32";

/**
 * @import {MRpcCallOptions, WorkerGlobalScope} from "@mys/m-rpc";
 * @import {Announcement} from "./worker.d.ts"
 */

////////////////////////////////////////////
// MISC
////////////////////////////////////////////

/**
 * Manage incoming connections for a shared worker.
 * If a regular worker is used instead, it'll just execute the callback immediately.
 *
 * @template {MessagePort | Worker | WorkerGlobalScope} T
 * @param {(context: MessagePort | T) => void} callback
 * @param {T} [context] Uses `globalThis` by default.
 */
export function ostiary(
  callback,
  context = /** @type {T} */ (/** @type {unknown} */ (globalThis)),
) {
  if (/** @type {any} */ (context).onmessage === null) {
    return callback(context);
  }

  context.addEventListener(
    "connect",
    /**
     * @param {any} event
     */
    (event) => {
      /** @type {MessagePort} */
      const port = event.ports[0];
      port.start();

      // Initiate setup
      callback(port);
    },
  );
}

////////////////////////////////////////////
// RAW
////////////////////////////////////////////

/**
 * @template T
 * @param {string} name
 * @param {T} args
 * @param {MessagePort | Worker | WorkerGlobalScope} [context] Uses `globalThis` by default.
 */
export function announce(
  name,
  args,
  context = /** @type {WorkerGlobalScope} */ (globalThis),
) {
  const transferables = getTransferables(args);
  context.postMessage(constructMsg(name, args), { transfer: transferables });
}

/**
 * @template T
 * @param {string} name
 * @param {(args: T) => void} fn
 * @param {MessagePort | Worker | WorkerGlobalScope} [context]
 */
export function listen(
  name,
  fn,
  context = /** @type {WorkerGlobalScope} */ (globalThis),
) {
  context.addEventListener(
    "message",
    /** @param {any} event */ (event) => {
      const announcement = /** @type {Announcement<T>} */ (event.data);
      const { ns, type } = announcement;

      if (announcement.name !== name) return;
      if (ns !== ANNOUNCEMENT || type !== ANNOUNCEMENT) return;

      fn(announcement.args);
    },
  );
}

////////////////////////////////////////////
// RPC
////////////////////////////////////////////

/**
 * @template {(...args: any[]) => any} Fn
 * @param {string} name
 * @param {Fn} fn
 * @param {MessagePort | Worker | WorkerGlobalScope} [context] Uses `globalThis` by default.
 */
export function define(
  name,
  fn,
  context = /** @type {WorkerGlobalScope} */ (globalThis),
) {
  return defineWorkerFn(name, fn, {
    port: /** @type {any} */ (context),
  });
}

/**
 * @param {string} name
 * @param {MessagePort | Worker | WorkerGlobalScope} [context] Uses `globalThis` by default.
 * @param {MRpcCallOptions} [options]
 */
export function use(
  name,
  context = /** @type {WorkerGlobalScope} */ (globalThis),
  options,
) {
  return useWorkerFn(name, /** @type {any} */ (context), {
    timeout: 60000,
    ...(options || {}),
  });
}

////////////////////////////////////////////
// ⛔️
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
