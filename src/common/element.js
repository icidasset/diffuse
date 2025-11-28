import QS from "query-string";
import { RPCChannel } from "@kunkun/kkrpc";
import { html, render } from "lit-html";

import { effect, signal } from "@common/signal.js";
import { rpc, transfer, workerLink, workerTunnel } from "./worker.js";
import { BrowserPostMessageIo } from "./worker/rpc.js";

// RE-EXPORT

export { workerLink, workerProxy, workerTunnel } from "./worker.js";

/**
 * @import {BroadcastingStatus, ProvisionedWorker, ProvisionedWorkers} from "./element.d.ts"
 * @import {ProxiedActions, Tunnel} from "./worker.d.ts";
 * @import {Signal} from "./signal.d.ts"
 */

/**
 * Base for custom elements, provides some utility functionality
 * around rendering and managing signals.
 */
export class DiffuseElement extends HTMLElement {
  #disposables = /** @type {Array<() => void>} */ ([]);

  #teardown() {
    this.#disposables.forEach((fn) => fn());
  }

  constructor() {
    super();

    this.group = this.getAttribute("group") ?? "default";

    this.worker = this.worker.bind(this);
    this.workerLink = this.workerLink.bind(this);
  }

  /**
   * @param {string} _name
   * @param {string} oldValue
   * @param {string} newValue
   */
  attributeChangedCallback(_name, oldValue, newValue) {
    if (oldValue !== newValue) this.#render();
  }

  /**
   * Effect helper that automatically disposes it
   * when this element is removed from the DOM.
   *
   * @param {() => void} fn
   */
  effect(fn) {
    this.#disposables.push(effect(fn));
  }

  /**
   * Avoid replacing the whole subtree,
   * morph the existing DOM into the new given tree.
   */
  #render() {
    if (!("render" in this && typeof this.render === "function")) return;

    const tmp = this.render({
      html: html,
      state: "state" in this ? this.state : undefined,
    });

    const root = this.shadowRoot ? this.shadowRoot : this;
    render(tmp, root);
  }

  /** */
  forceRender() {
    return this.#render();
  }

  // LIFECYCLE

  connectedCallback() {
    if (!("render" in this && typeof this.render === "function")) return;

    this.effect(() => {
      if (!("render" in this && typeof this.render === "function")) return;
      this.#render();
    });
  }

  disconnectedCallback() {
    this.#teardown();
  }

  // WORKER

  /** @type {undefined | Worker | SharedWorker} */
  #worker;

  createWorker() {
    const NAME = this.constructor.prototype.constructor.NAME;
    const WORKER_URL = this.constructor.prototype.constructor.WORKER_URL;

    if (!NAME) throw new Error("Missing `NAME` static property");
    if (!WORKER_URL) throw new Error("Missing `WORKER_URL` static property");

    // Query
    const query = QS.stringify(
      "workerQuery" in this && typeof this.workerQuery === "function"
        ? this.workerQuery()
        : {},
    );

    // Setup worker
    const name = `${NAME}/${this.group}`;
    const url = import.meta.resolve("./" + WORKER_URL) + `?${query}`;

    let worker;

    if (this.hasAttribute("group")) {
      worker = new SharedWorker(url, { name, type: "module" });
    } else {
      worker = new Worker(url, { name, type: "module" });
    }

    return worker;
  }

  worker() {
    this.#worker ??= this.createWorker();
    return this.#worker;
  }

  workerLink() {
    const worker = this.worker();
    return workerLink(worker);
  }
}

/**
 * Broadcastable version of the base class.
 *
 * Share the state of an element across multiple tabs
 * of the same origin and have one instance be the leader.
 */
export class BroadcastableDiffuseElement extends DiffuseElement {
  broadcasted = false;

  #broadcastingStatus;
  broadcastingStatus;

  /** @type {PromiseWithResolvers<void>} */
  #lock = Promise.withResolvers();

  /** @type {PromiseWithResolvers<BroadcastingStatus>} */
  #status = Promise.withResolvers();

  constructor() {
    super();

    this.broadcast = this.broadcast.bind(this);

    /** @type {Signal<Promise<BroadcastingStatus>>} */
    this.#broadcastingStatus = signal(this.#status.promise, { eager: true });
    this.broadcastingStatus = this.#broadcastingStatus.get;
  }

  /**
   * @template {Record<string, { strategy: "leaderOnly" | "replicate", fn: (...args: any[]) => any }>} ActionsWithStrategy
   * @template {{ [K in keyof ActionsWithStrategy]: ActionsWithStrategy[K]["fn"] }} Actions
   * @param {string} name
   * @param {ActionsWithStrategy} actionsWithStrategy
   */
  broadcast(name, actionsWithStrategy) {
    if (this.broadcasted) return;

    const channel = new BroadcastChannel(name);
    const msg = new MessageChannel();

    /**
     * @typedef {{ [K in keyof ActionsWithStrategy]: ActionsWithStrategy[K]["fn"] }} A
     */

    this.broadcasted = true;
    this.name = name;

    const _rpc = rpc(
      msg.port2,
      Object.fromEntries(
        Object.entries(actionsWithStrategy).map(([k, v]) => {
          return [k, v.fn.bind(this)];
        }),
      ),
    );

    channel.addEventListener(
      "message",
      async (event) => {
        if (event.data?.includes('"method":"leader:')) {
          const status = await this.#status.promise;
          if (status.leader) {
            const json = event.data.replace('"method":"leader:', '"method":"');
            msg.port1.postMessage(json);
          }
        } else {
          msg.port1.postMessage(event.data);
        }
      },
    );

    msg.port1.addEventListener(
      "message",
      (event) => channel.postMessage(event.data),
    );

    msg.port1.start();
    msg.port2.start();

    async function anyoneWaiting() {
      const state = await navigator.locks.query();
      return !!state.pending?.length;
    }

    const io = new BrowserPostMessageIo(() => msg.port2);

    /** @type {undefined | RPCChannel<{}, ProxiedActions<Actions>>} */
    const proxyChannel = new RPCChannel(io, { enableTransfer: true });

    /** @type {ProxiedActions<Actions>} */
    const proxy = proxyChannel.getAPI();

    /** @type {any} */
    const actions = {};

    Object.entries(actionsWithStrategy).forEach(
      ([action, { fn, strategy }]) => {
        const ogFn = fn.bind(this);
        let wrapFn = ogFn;

        switch (strategy) {
          case "leaderOnly":
            /** @param {Parameters<Actions[action]>} args */
            wrapFn = async (...args) => {
              const status = await this.#status.promise;
              return status.leader
                ? ogFn(...args)
                : proxyChannel.callMethod(`leader:${action}`, args);
            };
            break;

          case "replicate":
            /** @param {Parameters<Actions[action]>} args */
            wrapFn = async (...args) => {
              anyoneWaiting().then((bool) => {
                if (bool) proxy[action](...args);
              });
              return ogFn(...args);
            };
            break;
        }

        actions[action] = wrapFn;
      },
    );

    return /** @type {ProxiedActions<Actions>} */ (actions);
  }

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    if (!this.broadcasted) return;

    // Grab a lock if it isn't acquired yet,
    // and hold it until `this.lock.promise` resolves.
    navigator.locks.request(
      `${this.name}/lock`,
      { ifAvailable: true },
      (lock) => {
        this.#status.resolve(
          lock ? { leader: true, initialLeader: true } : { leader: false },
        );
        if (lock) return this.#lock.promise;
      },
    );

    // When the lock status is initially determined, log its status.
    // Additionally, wait for lock if needed.
    this.#status.promise.then((status) => {
      if (status.leader) {
        console.log(`🧙 Elected leader for: ${this.name}`);
      } else {
        console.log(`🔮 Watching leader: ${this.name}`);
      }

      // Wait for leadership
      if (status.leader === false) {
        navigator.locks.request(
          `${this.name}/lock`,
          () => {
            this.#status = Promise.withResolvers();
            this.#status.resolve({ leader: true, initialLeader: false });

            this.#broadcastingStatus.value = this.#status.promise;

            return this.#lock.promise;
          },
        );
      }
    });
  }

  /**
   * @override
   */
  disconnectedCallback() {
    super.disconnectedCallback();
    this.#lock.resolve();
  }
}

/**
 * @template {string} A
 * @template {ProvisionedWorkers<A>} B
 * @template {Record<string, any>} C
 * @template R
 * @param {Promise<B> | undefined} provisions
 * @param {(args: C & { ports: { [K in keyof B]: MessagePort } }) => R} fn
 * @param {C} fnArgs
 * @returns {Promise<R>}
 */
export async function callWorkerWithProvisions(provisions, fn, fnArgs) {
  const workers = await provisions;
  if (!workers) throw new Error("Workers not defined");

  /** @type {Array<[keyof B, Tunnel]>} */
  const tunnels = Object.keys(workers).map(
    (value) => {
      const key = /** @type {keyof B} */ (value);
      const worker = workers[key];
      return [key, workerTunnel(worker)];
    },
  );

  const ports = /** @type {{ [K in keyof B]: MessagePort }} */ (
    Object.fromEntries(
      tunnels.map(([key, tunnel]) => {
        return [key, tunnel.port];
      }),
    )
  );

  const args = {
    ...fnArgs,
    ports,
  };

  const result = await fn(transfer(
    args,
    tunnels.map(([_key, tunnel]) => {
      return tunnel.port;
    }),
  ));

  tunnels.forEach(([_key, tunnel]) => {
    tunnel.disconnect();
  });

  return result;
}

/**
 * Component DOM selector.
 *
 * Basically `document.querySelector` but returns the element
 * with the correct type based on the element module given.
 *
 * ```
 * import * as QueryEngine from "@components/engine/query/element.js"
 *
 * const instance = component(QueryEngine)
 * ```
 *
 * @template {abstract new (...args: any[]) => any} C
 * @param {{ CLASS: C; NAME: string }} elementModule
 * @param {string} [id] Optional id to select
 */
export function component(elementModule, id) {
  const el = document.querySelector(
    id ? `${elementModule.NAME}#${id}` : elementModule.NAME,
  );
  if (!el) {
    throw new Error(`Element for selector '${elementModule.NAME}' not found.`);
  }
  return /** @type {InstanceType<C>} */ (el);
}

/**
 * @template {HTMLElement} T
 * @param {DiffuseElement} parent
 * @param {string} attribute
 * @returns {T}
 */
export function query(parent, attribute) {
  const selector = parent.getAttribute(attribute);

  if (!selector) {
    throw new Error(`Missing required '${attribute}' attribute`);
  }

  /** @type {T | null} */
  const element = document.querySelector(selector);
  if (!element) throw new Error(`Missing required '${selector}' element`);

  return element;
}

/**
 * @template {Record<string, DiffuseElement>} T
 * @param {T} elements
 */
export async function provisionWorkers(elements) {
  await whenElementsDefined(elements);

  /** @type {Record<string, ProvisionedWorker>} */
  const provisions = {};

  Object.entries(elements).forEach(([key, element]) => {
    const worker = element.createWorker();
    provisions[key] = worker;
  });

  const casted =
    /** @type {{ [K in keyof T]: ProvisionedWorker}} */ (provisions);

  return casted;
}

/**
 * @param {ProvisionedWorkers<any> | undefined} workers
 */
export function terminateProvisions(workers) {
  if (!workers) return;

  Object.values(workers).forEach((worker) => {
    if (worker instanceof Worker) worker.terminate();
  });
}

/**
 * @template {Record<string, DiffuseElement>} T
 * @param {T} elements
 */
export async function whenElementsDefined(elements) {
  await Promise.all(
    Object.values(elements).map((element) =>
      customElements.whenDefined(element.localName)
    ),
  );
}
