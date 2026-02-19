import { RPCChannel, transfer } from "@kunkun/kkrpc";
import { getTransferables } from "@okikio/transferables";
import { debounceMicrotask } from "@vicary/debounce-microtask";
import { xxh32 } from "xxh32";

import { BrowserPostMessageIo } from "./worker/rpc.js";

export { getTransferables } from "@okikio/transferables";
export { transfer } from "@kunkun/kkrpc";

/**
 * @import {Track} from "@definitions/types.d.ts"
 * @import {Announcement, MessengerRealm, ProxiedActions, Tunnel} from "./worker.d.ts"
 */

////////////////////////////////////////////
// MISC
////////////////////////////////////////////

/**
 * Manage incoming connections for a shared worker.
 * If a regular worker is used instead, it'll just execute the callback immediately.
 *
 * @template {MessagePort | Worker | MessengerRealm} T
 * @param {(context: MessagePort | T, firstConnection: boolean, connectionId: string) => void} callback
 * @param {T} [context] Uses `globalThis` by default.
 */
export function ostiary(
  callback,
  context = /** @type {T} */ (/** @type {unknown} */ (globalThis)),
) {
  if (/** @type {any} */ (context).onmessage === null) {
    return callback(context, true, crypto.randomUUID());
  }

  const c = /** @type {any} */ (context);
  c.__id ??= crypto.randomUUID();

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
      callback(port, !(c.__initiated ?? false), c.__id);
      c.__initiated = true;
    },
  );
}

/**
 * @param {Uint8Array} data
 * @returns {Track[]}
 */
export function tracksIn(data) {
  return JSON.parse(new TextDecoder().decode(data));
}

/**
 * @param {Track[]} tracks
 */
export function tracksOut(tracks) {
  const buffer = new TextEncoder().encode(JSON.stringify(tracks))
  return transfer(buffer, [buffer]);
}

/**
 * @param {Worker | SharedWorker} worker
 */
export function workerLink(worker) {
  if (worker instanceof SharedWorker) {
    worker.port.start();
    return worker.port;
  } else {
    return worker;
  }
}

/**
 * @template {Record<string, (...args: any[]) => any>} Actions
 * @param {() => MessagePort | Worker} workerLinkCreator
 * @returns {ProxiedActions<Actions>}
 */
export function workerProxy(workerLinkCreator) {
  /** @type {ProxiedActions<Actions> | undefined} */
  let int_api;

  /** @returns {ProxiedActions<Actions>} */
  function ensureAPI() {
    if (!int_api) {
      const io = new BrowserPostMessageIo(workerLinkCreator);

      /** @type {undefined | RPCChannel<{}, ProxiedActions<Actions>>} */
      const rpc = new RPCChannel(io, { enableTransfer: false });

      int_api = rpc.getAPI();
    }

    return int_api;
  }

  // Create proxy that creates RPC API when needed
  const proxy = new Proxy(() => {}, {
    get: (_target, prop) => {
      /** @param {Parameters<Actions[any]>} args */
      return (...args) => {
        const api = ensureAPI();
        return api[prop.toString()](...args);
      };
    },
  });

  return /** @type {ProxiedActions<Actions>} */ (/** @type {any} */ (proxy));
}

/**
 * @param {MessagePort | Worker | SharedWorker} workerOrLink
 * @param {{ fromWorker?: (message: any) => Promise<{ data: any, transfer?: Transferable[] }>; toWorker?: (message: any) => Promise<{ data: any, transfer?: Transferable[] }> }} [hooks]
 * @returns {Tunnel}
 */
export function workerTunnel(workerOrLink, hooks = {}) {
  const link = workerOrLink instanceof SharedWorker
    ? workerLink(workerOrLink)
    : workerOrLink;
  const channel = new MessageChannel();

  channel.port1.addEventListener("message", async (event) => {
    // Send to worker
    const { data, transfer } = await hooks?.toWorker?.(event.data) ??
      { data: event.data };
    link.postMessage(data, { transfer });
  });

  /**
   * @param {Event} event
   */
  const workerListener = async (event) => {
    // Receive from worker
    const msgEvent = /** @type {MessageEvent} */ (event);
    const { data, transfer } = await hooks?.fromWorker?.(msgEvent.data) ??
      { data: msgEvent.data };
    channel.port1.postMessage(data, { transfer });
  };

  link.addEventListener("message", workerListener);

  channel.port1.start();
  channel.port2.start();

  return {
    disconnect: () => {
      link.removeEventListener("message", workerListener);
      channel.port1.close();
      channel.port2.close();
    },
    port: channel.port2,
  };
}

////////////////////////////////////////////
// RAW
////////////////////////////////////////////

/**
 * @template T
 * @param {string} name
 * @param {T} args
 * @param {MessagePort | Worker | MessengerRealm} [context] Uses `globalThis` by default.
 */
export function announce(
  name,
  args,
  context,
) {
  const a = announcement(name, args);
  const transferables = getTransferables(a);
  (context ?? globalThis).postMessage(a, { transfer: transferables });
}

/**
 * @template T
 * @param {string} name
 * @param {(args: T) => void} fn
 * @param {MessagePort | Worker | MessengerRealm} [context]
 */
export function listen(
  name,
  fn,
  context = /** @type {MessengerRealm} */ (globalThis),
) {
  const c = /** @type {any} */ (context);

  if (!c.__incoming) {
    context.addEventListener("message", incomingAnnouncementsHandler(context));
    c.__incoming = {};
  }

  c.__incoming[name] = debounceMicrotask(fn, { updateArguments: true });
}

////////////////////////////////////////////
// RPC
////////////////////////////////////////////

/**
 * @template {Record<string, (...args: any[]) => any>} Actions
 * @param {MessagePort | Worker | MessengerRealm} context
 * @param {Actions} actions
 */
export function rpc(context, actions) {
  const io = new BrowserPostMessageIo(() => context);

  /** @type {undefined | RPCChannel<Actions, {}>} */
  return new RPCChannel(io, { enableTransfer: false, expose: actions });
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
 * @param {MessagePort | Worker | MessengerRealm} context
 */
function incomingAnnouncementsHandler(context) {
  /** @param {any} event */
  return (event) => {
    const { ns, type } = event.data;
    if (ns !== ANNOUNCEMENT || type !== ANNOUNCEMENT) return;
    const announcement = /** @type {Announcement<any>} */ (event.data);
    const c = /** @type {any} */ (context);
    c.__incoming[announcement.name]?.(announcement.args);
  };
}
