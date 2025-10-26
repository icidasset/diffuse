import Queue from "@mary/ds-queue";
import { defineWorkerFn, useWorkerFn } from "@mys/worker-fn";
import { getTransferables } from "@okikio/transferables";
import { debounceMicrotask } from "@vicary/debounce-microtask";
import { xxh32 } from "xxh32";
import { batch } from "./signal.js";

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
  context,
) {
  outgoing.enqueue(announcement(name, args));
  flushOutgoingAnnouncements(context);
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
  if (!context.incoming) {
    context.addEventListener("message", incomingAnnouncementsHandler(context));
    context.incoming = {};
  }

  context.incoming[name] = debounceMicrotask(fn, { updateArguments: true });
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
function announcement(name, args) {
  return {
    ns: ANNOUNCEMENT,
    name,
    key: xxh32(crypto.randomUUID()),

    type: ANNOUNCEMENT,
    args,
  };
}

/**
 * Process incoming announcements.
 */
const flushIncomingAnnouncements = debounceMicrotask(
  /**
   * @param {MessagePort | Worker | WorkerGlobalScope} [context] Uses `globalThis` by default.
   */
  (context = /** @type {WorkerGlobalScope} */ (globalThis)) => {
    /** @type {Announcement<any>[]} */
    const arr = [];

    for (const a of incoming.drain()) {
      arr.push(a);
    }

    batch(() => {
      arr.forEach((announcement) => {
        context.incoming[announcement.name]?.(announcement.args);
      });
    });
  },
);

/**
 * Process outgoing announcements.
 */
const flushOutgoingAnnouncements = debounceMicrotask(
  /**
   * @param {MessagePort | Worker | WorkerGlobalScope} [context] Uses `globalThis` by default.
   */
  (context = /** @type {WorkerGlobalScope} */ (globalThis)) => {
    /** @type {Announcement<any>[]} */
    const arr = [];

    for (const a of outgoing.drain()) {
      arr.push(a);
    }

    const transferables = getTransferables(arr);
    context.postMessage(arr, { transfer: transferables });
  },
);

/**
 * @type {Queue<Announcement<any>>}
 */
const incoming = new Queue();

/**
 * @param {MessagePort | Worker | WorkerGlobalScope} context
 */
function incomingAnnouncementsHandler(context) {
  /** @param {any} event */
  return (event) => {
    const arr = /** @type {Announcement<any>[]} */ (event.data);

    if (Array.isArray(arr)) {
      arr.forEach((announcement) => {
        const { ns, type } = announcement;
        if (ns !== ANNOUNCEMENT || type !== ANNOUNCEMENT) return;
        incoming.enqueue(announcement);
        flushIncomingAnnouncements(context);
      });
    }
  };
}

/**
 * @type {Queue<Announcement<any>>}
 */
const outgoing = new Queue();
