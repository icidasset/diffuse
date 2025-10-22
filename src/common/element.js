import morphdom from "morphdom";

import { effect, signal } from "@common/signal.js";
import { define, use } from "@common/worker.js";

/**
 * @import {BroadcastingStatus, FnParams, FnReturn, HtmlTagFunction, MorphOptions} from "./element.d.ts"
 * @import {Signal, SignalReader} from "./signal.d.ts"
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
    this.morphedRender = this.morphedRender.bind(this);
  }

  /**
   * @param {string} _name
   * @param {string} oldValue
   * @param {string} newValue
   */
  attributeChangedCallback(_name, oldValue, newValue) {
    if (oldValue !== newValue) this.morphedRender();
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
   * @type {HtmlTagFunction}
   */
  html(strings, ...values) {
    return String.raw({ raw: strings }, ...values);
  }

  /**
   * Avoid replacing the whole subtree,
   * morph the existing DOM into the new given tree.
   */
  morphedRender() {
    if (!("render" in this && typeof this.render === "function")) return;
    if (!("state" in this)) return;

    const tmp = this.render({
      html: this.html,
      state: this.state,
    });

    const updated = document.createElement("div");
    updated.innerHTML = tmp.trim();
    const root = this.shadowRoot ? this.shadowRoot : this;

    morphdom(
      root,
      updated,
      {
        ...this.morphOptions,
        childrenOnly: true,
      },
    );
  }

  // MORPH STUFF

  /** @type {MorphOptions} */
  morphOptions = {};

  // LIFECYCLE

  connectedCallback() {
    if (!("render" in this && typeof this.render === "function")) return;

    this.effect(() => {
      if (!("render" in this && typeof this.render === "function")) return;
      if (!("state" in this)) return;

      this.morphedRender();
    });
  }

  disconnectedCallback() {
    this.#teardown();
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
    this.#broadcastingStatus = signal(this.#status.promise, { unbiased: true });
    this.broadcastingStatus = this.#broadcastingStatus.get;
  }

  /**
   * @param {string} name
   */
  broadcast(name) {
    const channel = new BroadcastChannel(name);
    const msg = new MessageChannel();

    this.broadcasted = true;
    this.name = name;

    channel.addEventListener(
      "message",
      async (event) => {
        const name = event.data.name?.split(":");

        if (name[0] === "leader") {
          const status = await this.#status.promise;
          if (status.leader) {
            msg.port1.postMessage({
              ...event.data,
              name: name.splice(1).join(":"),
            });
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

    /**
     * @template I
     * @template O
     * @template {(...args: I[]) => O} Fn
     * @param {string} method
     * @param {Fn} fn
     */
    return (method, fn) => {
      define(method, fn.bind(this), msg.port2);

      /**
       * @typedef {FnParams<typeof fn>} P
       * @typedef {FnReturn<typeof fn>} R
       */

      /** @param {P} args */
      const leaderOnly = async (...args) => {
        const status = await this.#status.promise;
        return status.leader
          ? /** @type {R} */ (fn.call(this, ...args))
          : /** @type {Promise<R>} */ (use(`leader:${method}`, msg.port2)(
            ...args,
          ));
      };

      /**
       * @param {P} args
       * @returns {R}
       */
      const replicate = (...args) => {
        anyoneWaiting().then((bool) => {
          if (bool) use(method, msg.port2)(...args);
        });
        return /** @type {R} */ (fn.call(this, ...args));
      };

      return {
        leaderOnly,
        replicate,
      };
    };
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
