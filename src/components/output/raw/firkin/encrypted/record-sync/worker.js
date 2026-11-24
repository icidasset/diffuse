import "./dom-shim.js";

import { effect } from "~/common/signal.js";
import { ostiary, rpc } from "~/common/worker.js";

import { COLLECTIONS } from "./constants.js";

/**
 * SharedWorker host for the Record Sync output.
 *
 * The worker is created per `identifier` (element name + group + namespace),
 * so one instance owns the engine for every tab using that output. The element
 * itself runs here unchanged on top of `dom-shim.js`; this file only adapts
 * element lifecycle and collections to the worker RPC.
 */

/** @type {Set<any>} */
const ports = new Set();

/** @type {any} */
let element;

/** @type {Promise<any> | undefined} */
let classPromise;

/** @returns {Promise<any>} */
function loadClass() {
  return classPromise ??= import("./element.js").then((module) => module.CLASS);
}

/** @type {{ namespace: string; keep: number } | undefined} */
let config;

/**
 * @param {Record<string, unknown>} event
 */
function broadcast(event) {
  for (const port of ports) {
    try {
      port.postMessage({ ns: "record-sync", ...event });
    } catch {
      // A closed port; the next `open` will re-add it.
      ports.delete(port);
    }
  }
}

/**
 * @param {any} port
 */
function sendSnapshot(port) {
  if (!element) return;

  for (const name of COLLECTIONS) {
    const collection = element[name].collection();
    if (collection.state !== "loaded") continue;

    port.postMessage({
      ns: "record-sync",
      type: "items",
      collection: name,
      items: collection.data,
    });
  }

  port.postMessage({
    ns: "record-sync",
    type: "status",
    ready: element.ready(),
  });
}

/**
 * @param {{ namespace: string; keep: number }} next
 */
async function ensureElement(next) {
  if (element) return element;

  const CLASS = await loadClass();

  config = next;
  element = new CLASS();
  element.localName = "dob-record-sync";
  element.isConnected = true;
  element.setAttribute("namespace", config.namespace);
  element.setAttribute("keep", String(config.keep));

  effect(() => {
    if (!element) return;

    const ready = element.ready();

    for (const name of COLLECTIONS) {
      const collection = element[name].collection();
      if (collection.state !== "loaded") continue;

      broadcast({
        type: "items",
        collection: name,
        items: collection.data,
      });
    }

    broadcast({ type: "status", ready });
  });

  element.connectedCallback();

  return element;
}

/**
 * @param {string} name
 */
function collection(name) {
  if (!element) throw new Error("The record sync worker is not open");
  if (!COLLECTIONS.includes(/** @type {any} */ (name))) {
    throw new TypeError(`Unknown collection '${name}'`);
  }

  return element[name];
}

ostiary((context) => {
  rpc(context, {
    /** @param {{ namespace: string; keep?: number }} options */
    async open({ namespace, keep }) {
      if (config && config.namespace !== namespace) {
        throw new Error(
          `This worker already serves '${config.namespace}', not '${namespace}'`,
        );
      }

      // A detached element can reconnect on the same port after `close`.
      // Registering here (rather than on connect) keeps both cases working.
      ports.add(context);
      await ensureElement({ namespace, keep: keep ?? 64 });
      sendSnapshot(context);
    },

    /**
     * @param {string} name
     * @param {any[]} items
     */
    save(name, items) {
      return collection(name).save(items);
    },

    /** @param {string} name */
    reload(name) {
      return collection(name).reload();
    },

    /** @param {boolean} online */
    online(online) {
      globalThis.dispatchEvent(new Event(online ? "online" : "offline"));
    },

    visible() {
      const document = /** @type {any} */ (globalThis).document;
      document.visibilityState = "visible";
      document.dispatchEvent(new Event("visibilitychange"));
    },

    hidden() {
      const document = /** @type {any} */ (globalThis).document;
      document.visibilityState = "hidden";
      document.dispatchEvent(new Event("visibilitychange"));
    },

    close() {
      ports.delete(context);
    },
  });
});
