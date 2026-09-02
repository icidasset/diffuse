import { getTransferables } from "@okikio/transferables";
import { debounceMicrotask } from "@vicary/debounce-microtask";
import { xxh32 } from "xxh32";

import { RpcChannel } from "./worker/rpc-channel.js";

export { getTransferables } from "@okikio/transferables";

/**
 * @import {Announcement, MessengerRealm, ProxiedActions, Tunnel} from "./worker.d.ts"
 */

// Early message buffer for regular Workers.
//
// If a Worker module (or its dependencies) contains a top-level `await`, the
// browser can deliver queued incoming messages to `globalThis` while the module
// evaluation is paused — before `ostiary`/`rpc()` has had a chance to register
// a handler. Those messages would otherwise be silently dropped.
//
// This buffer captures such messages the moment this module is imported (which
// happens before any top-level `await` pause) and replays them once `ostiary`
// sets up the real handler.
//
// Detection: regular Workers are instances of DedicatedWorkerGlobalScope.
// Previously we checked `globalThis.onmessage === null`, but Safari initialises
// that property as `undefined` rather than `null`, causing the check to fail.

/**
 * Detects whether we're running inside a dedicated worker's global scope.
 * Uses the constructor name rather than the `DedicatedWorkerGlobalScope`
 * global, so this module doesn't need the `webworker` TS lib (JSR disallows
 * `/// <reference lib="..." />` in published files).
 *
 * @param {unknown} context
 * @returns {boolean}
 */
function isDedicatedWorkerScope(context) {
  return /** @type {any} */ (context)?.constructor?.name ===
    "DedicatedWorkerGlobalScope";
}

/** @type {MessageEvent[]} */
const _earlyMessages = [];

/** @type {null | (() => void)} */
let _flushEarlyMessages = null;

if (isDedicatedWorkerScope(globalThis)) {
  const handler = /** @type {EventListener} */ ((event) => {
    _earlyMessages.push(/** @type {MessageEvent} */ (event));
  });

  globalThis.addEventListener("message", handler);

  _flushEarlyMessages = () => {
    globalThis.removeEventListener("message", handler);
  };
}

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
  if (isDedicatedWorkerScope(context)) {
    callback(context, true, crypto.randomUUID());

    // Replay any messages that arrived before the handler was registered.
    if (_flushEarlyMessages) {
      _flushEarlyMessages();
      _flushEarlyMessages = null;
      const ctx = /** @type {EventTarget} */ (/** @type {unknown} */ (context));
      _earlyMessages.splice(0).forEach((e) => {
        ctx.dispatchEvent(
          new MessageEvent("message", { data: e.data, ports: [...e.ports] }),
        );
      });
    }

    return;
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
  /** @type {RpcChannel<{}, Actions> | undefined} */
  let channel;

  const proxy = new Proxy(/** @type {any} */ ({}), {
    get: (_target, /** @type {string} */ prop) => {
      /** @param {Parameters<Actions[any]>} args */
      return (...args) => {
        channel ??= new RpcChannel(workerLinkCreator());
        return channel.callMethod(prop, args);
      };
    },
  });

  return /** @type {ProxiedActions<Actions>} */ (proxy);
}

/**
 * @param {() => MessagePort | Worker | SharedWorker} workerCreator
 * @param {{ fromWorker?: (message: any) => Promise<{ data: any, transfer?: Transferable[] }>; toWorker?: (message: any) => Promise<{ data: any, transfer?: Transferable[] }> }} [hooks]
 * @returns {Tunnel}
 */
export function workerTunnel(workerCreator, hooks = {}) {
  /** @type {MessagePort | Worker | undefined} */
  let link;

  const channel = new MessageChannel();

  function ensureLink() {
    if (link) return link;

    const workerOrLink = workerCreator();

    link = workerOrLink instanceof SharedWorker
      ? workerLink(workerOrLink)
      : workerOrLink;

    link.addEventListener("message", workerListener);

    return link;
  }

  channel.port1.addEventListener("message", async (event) => {
    // Send to worker
    const { data, transfer } = await hooks?.toWorker?.(event.data) ??
      { data: event.data };
    ensureLink().postMessage(data, { transfer });
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

  channel.port1.start();
  channel.port2.start();

  return {
    disconnect: () => {
      link?.removeEventListener("message", workerListener);
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
  /** @type {MessengerRealm} */ (/** @type {unknown} */ (context ?? globalThis))
    .postMessage(a, { transfer: transferables });
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
 * @template {Record<string, (...args: any[]) => any>} LocalAPI
 * @template {Record<string, (...args: any[]) => any>} RemoteAPI
 * @param {MessagePort | Worker | MessengerRealm} context
 * @param {RemoteAPI} actions
 * @returns {RpcChannel<{}, RemoteAPI>}
 */
export function rpc(context, actions) {
  /** @type {RpcChannel<{}, RemoteAPI>} */
  const channel = new RpcChannel(context, { expose: actions });
  return channel;
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
