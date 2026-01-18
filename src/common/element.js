import QS from "query-string";
import { decodeMessage, encodeMessage, RPCChannel } from "@kunkun/kkrpc";
import { html, render } from "lit-html";

import { effect, signal } from "@common/signal.js";
import { rpc, workerLink, workerProxy, workerTunnel } from "./worker.js";
import { BrowserPostMessageIo } from "./worker/rpc.js";

/**
 * @import {BroadcastingStatus, WorkerOpts} from "./element.d.ts"
 * @import {ProxiedActions, Tunnel} from "./worker.d.ts";
 * @import {Signal} from "./signal.d.ts"
 */

export { nothing } from "lit-html";
export const DEFAULT_GROUP = "default";

/**
 * Base for custom elements, provides some utility functionality
 * around rendering and managing signals.
 */
export class DiffuseElement extends HTMLElement {
  $connected = signal(false);

  #connected = Promise.withResolvers();
  #disposables = /** @type {Array<() => void>} */ ([]);

  /** */
  constructor() {
    super();

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
   * Effect helper that automatically is disposes
   * when this element is removed from the DOM.
   *
   * @param {() => void} fn
   */
  effect(fn) {
    this.#disposables.push(effect(fn));
  }

  /** */
  forceRender() {
    return this.#render();
  }

  /** */
  get group() {
    return this.getAttribute("group") ?? DEFAULT_GROUP;
  }

  /** */
  get label() {
    return this.getAttribute("label") ?? this.id ?? this.localName;
  }

  /** */
  get nameWithGroup() {
    return `${this.constructor.prototype.constructor.NAME}/${this.group}`;
  }

  /** */
  root() {
    return (this.shadowRoot ?? this);
  }

  /** */
  get selector() {
    return this.id.length ? `#${this.id}` : this.localName;
  }

  /** */
  whenConnected() {
    return this.#connected.promise;
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

    render(tmp, this.root());
  }

  // LIFECYCLE

  connectedCallback() {
    this.$connected.value = true;
    this.#connected.resolve(null);

    if (!("render" in this && typeof this.render === "function")) return;

    this.effect(() => {
      if (!("render" in this && typeof this.render === "function")) return;
      this.#render();
    });
  }

  disconnectedCallback() {
    this.$connected.value = false;
    this.#teardown();
  }

  #teardown() {
    this.#disposables.forEach((fn) => fn());
  }

  // WORKERS

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
    const name = this.nameWithGroup;
    const url = import.meta.resolve("./" + WORKER_URL) + `?${query}`;

    let worker;

    if (this.hasAttribute("group")) {
      worker = new SharedWorker(url, { name, type: "module" });
    } else {
      worker = new Worker(url, { name, type: "module" });
    }

    return worker;
  }

  /** */
  dependencies() {
    return Object.fromEntries(
      Array.from(this.children).flatMap((element) => {
        if ("nameWithGroup" in element === false) {
          return [];
        }

        const d = /** @type {DiffuseElement} */ (element);
        return [[d.localName, d]];
      }),
    );
  }

  worker() {
    this.#worker ??= this.createWorker();
    return this.#worker;
  }

  workerLink() {
    const worker = this.worker();
    return workerLink(worker);
  }

  /**
   * @template {Record<string, (...args: any[]) => any>} Actions
   * @param {WorkerOpts} [opts]
   * @returns {ProxiedActions<Actions>}
   */
  workerProxy(opts) {
    return workerProxy(
      () => this.workerTunnel(opts).port,
    );
  }

  /**
   * @param {WorkerOpts} [opts]
   */
  workerTunnel({ forceNew } = {}) {
    // Creates a MessagePort that is connected to the worker.
    // All the dependencies are added automatically.
    const worker = forceNew === true ||
        (typeof forceNew === "object" && forceNew.self === true)
      ? this.createWorker()
      : this.worker();
    const deps = this.dependencies();

    let toWorker;

    if (Object.keys(deps).length) {
      toWorker =
        /**
         * @param {any} msg
         */
        async (msg) => {
          /** @type {Array<[string, Tunnel]>} */
          const ports = Object.entries(deps).map(
            /** @param {[string, DiffuseElement]} _ */
            ([k, v]) => {
              const n = typeof forceNew === "object"
                ? forceNew.dependencies?.[k] ?? false
                : false;
              return [k, v.workerTunnel({ forceNew: n })];
            },
          );

          const decoded = await decodeMessage(msg);
          const data = {
            data: Array.isArray(decoded.args) ? decoded.args[0] : decoded.args,
            ports: Object.fromEntries(ports.map(([k, v]) => {
              return [k, v.port];
            })),
          };

          const encoded = encodeMessage(
            {
              ...decoded,
              args: Array.isArray(decoded.args)
                ? [data, ...decoded.args.slice(1)]
                : decoded.args,
            },
            {},
            true,
            ports.map(([_k, v]) => v.port),
          );

          this.#disposables.push(() => {
            ports.forEach(([_k, v]) => v.disconnect());
          });

          return {
            data: encoded,
            transfer: ports.map(([_k, v]) => v.port),
          };
        };
    }

    const tunnel = workerTunnel(worker, { toWorker });
    return tunnel;
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

  /** @type {{ assumeLeadership?: boolean }} */
  #broadcastingOptions = {};

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
   * @param {string} channelName
   * @param {ActionsWithStrategy} actionsWithStrategy
   * @param {{ assumeLeadership?: boolean }} [options]
   */
  broadcast(channelName, actionsWithStrategy, options) {
    if (this.broadcasted) return;
    if (options) this.#broadcastingOptions = options;

    const channel = new BroadcastChannel(channelName);
    const msg = new MessageChannel();

    /**
     * @typedef {{ [K in keyof ActionsWithStrategy]: ActionsWithStrategy[K]["fn"] }} A
     */

    this.broadcasted = true;
    this.channelName = channelName;

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

  async isLeader() {
    if (this.broadcasted) {
      const status = await this.broadcastingStatus();
      return status.leader;
    } else {
      return true;
    }
  }

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    if (!this.broadcasted) return;

    // Grab a lock if it isn't acquired yet and if needed,
    // and hold it until `this.lock.promise` resolves.
    const assumeLeadership = this.#broadcastingOptions?.assumeLeadership;

    if (assumeLeadership === undefined || assumeLeadership === true) {
      navigator.locks.request(
        `${this.channelName}/lock`,
        assumeLeadership === true ? { steal: true } : { ifAvailable: true },
        (lock) => {
          this.#status.resolve(
            lock ? { leader: true, initialLeader: true } : { leader: false },
          );
          if (lock) return this.#lock.promise;
        },
      );
    } else {
      this.#status.resolve(
        { leader: false },
      );
    }

    // When the lock status is initially determined, log its status.
    // Additionally, wait for lock if needed.
    this.#status.promise.then((status) => {
      if (status.leader) {
        console.log(`🧙 Elected leader for: ${this.channelName}`);
      } else {
        console.log(`🔮 Watching leader: ${this.channelName}`);
      }

      // Wait for leadership
      if (status.leader === false) {
        navigator.locks.request(
          `${this.channelName}/lock`,
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
 * @param {Record<string, Worker | SharedWorker>} workers
 */
export function terminateWorkers(workers) {
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
