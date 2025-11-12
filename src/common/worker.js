import Queue from "@mary/ds-queue";

import { MRpc } from "@mys/m-rpc";
import { getTransferables } from "@okikio/transferables";
import { debounceMicrotask } from "@vicary/debounce-microtask";
import { xxh32 } from "xxh32";

import { batch } from "./signal.js";

export { getTransferables } from "@okikio/transferables";

/**
 * @import {MRpcCallOptions, WorkerGlobalScope} from "@mys/m-rpc";
 *
 * @import {IncompleteArray} from "./types.d.ts"
 * @import {Announcement, ProxiedActions, ProxyProvider} from "./worker.d.ts"
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

/**
 * @param {MessagePort | Worker} workerOrPort
 */
export function portProvider(workerOrPort) {
  return () => {
    const channel = new MessageChannel();

    channel.port1.addEventListener("message", (event) => {
      console.log("SEND", event.data);
      workerOrPort.postMessage(event.data);
    });

    /**
     * @param {Event} event
     */
    const workerListener = (event) => {
      const msgEvent = /** @type {MessageEvent} */ (event);
      // const data = { ...msgEvent.data, ns };
      console.log(msgEvent.data);
      channel.port1.postMessage(msgEvent.data);
    };

    workerOrPort.addEventListener("message", workerListener);

    channel.port1.start();
    channel.port2.start();

    return {
      disconnect: () => {
        workerOrPort.removeEventListener("message", workerListener);
        channel.port1.close();
        channel.port2.close();
      },
      port: channel.port2,
    };
  };
}

/**
 * @template {Record<string, (...args: any[]) => any>} Actions
 * @template {keyof Actions} T
 * @template {T[]} U
 * @param {U & ([T] extends [U[number]] ? unknown : IncompleteArray<T>[])} actions
 */
export function proxyProvider(actions) {
  /**
   * @type {ProxyProvider<Actions>}
   */
  return (workerOrPort) => {
    /** @type {Record<string | number | symbol, (...args: any[]) => any>} */
    const proxy = {};

    actions.forEach((action) => {
      proxy[action] = use(action.toString(), workerOrPort);
    });

    return /** @type {ProxiedActions<Actions>} */ (proxy);
  };
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
  const rpc = MRpc.ensureMRpc(context);
  return rpc.defineLocalFn(name, fn);
}

/**
 * @template {(...args: I[]) => O} Fn
 * @template I
 * @template O
 * @param {string} name
 * @param {MessagePort | Worker | WorkerGlobalScope} [context] Uses `globalThis` by default.
 * @param {MRpcCallOptions} [options]
 */
export function use(
  name,
  context = /** @type {WorkerGlobalScope} */ (globalThis),
  options,
) {
  const rpc = MRpc.ensureMRpc(context);
  const _fn = rpc.useRemoteFn(name, { timeout: 60000, ...(options || {}) });

  const fn = /** @type {Fn} */ (async (...args) => {
    try {
      return await _fn(...args);
    } catch (err) {
      if (
        err instanceof Error &&
        err.message ===
          `The remote threw an error when calling the function "${name}".`
      ) {
        err.message = `The worker function "${name}" throws an error.`;
      }
      throw err;
    }
  });

  return fn;
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
