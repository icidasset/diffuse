/**
 * @import {MessengerRealm, ProxiedActions} from "../worker.d.ts"
 */

/**
 * Lightweight RPC channel over postMessage using structured clone.
 *
 * Protocol:
 * - Request:  { __rpc: true, id, method, args, type: "request" }
 * - Response: { __rpc: true, id, type: "response", result } or { ..., error }
 *
 * @template {Record<string, (...args: any[]) => any>} LocalAPI
 * @template {Record<string, (...args: any[]) => any>} RemoteAPI
 */
export class RpcChannel {
  /** @type {LocalAPI | undefined} */
  #actions;

  /** @type {Record<string, { resolve: (v: any) => void, reject: (e: any) => void }>} */
  #pending = {};

  #port;

  /** @type {(event: MessageEvent) => void} */
  #listener;

  /**
   * @param {MessagePort | Worker | MessengerRealm} port
   * @param {{ expose?: LocalAPI }} [options]
   */
  constructor(port, options) {
    this.#port = port;
    this.#actions = options?.expose;

    this.#listener = (/** @type {MessageEvent} */ event) => {
      const msg = event.data;
      if (!msg || msg.__rpc !== true) return;

      if (msg.type === "request") {
        this.#handleRequest(msg);
      } else if (msg.type === "response") {
        this.#handleResponse(msg);
      }
    };

    port.addEventListener(
      "message",
      /** @type {EventListener} */ (this.#listener),
    );
  }

  /**
   * @param {LocalAPI} actions
   */
  expose(actions) {
    this.#actions = actions;
  }

  /**
   * @template {keyof RemoteAPI & string} M
   * @param {M} method
   * @param {any[]} args
   * @returns {Promise<any>}
   */
  callMethod(method, args) {
    return new Promise((resolve, reject) => {
      const id = crypto.randomUUID();
      this.#pending[id] = { resolve, reject };
      this.#port.postMessage({
        __rpc: true,
        id,
        method,
        args,
        type: "request",
      });
    });
  }

  /**
   * @returns {ProxiedActions<RemoteAPI>}
   */
  getAPI() {
    return /** @type {ProxiedActions<RemoteAPI>} */ (
      new Proxy(/** @type {any} */ ({}), {
        get: (_target, /** @type {string} */ prop) => {
          return (/** @type {any[]} */ ...args) => this.callMethod(prop, args);
        },
      })
    );
  }

  destroy() {
    this.#port.removeEventListener(
      "message",
      /** @type {EventListener} */ (this.#listener),
    );

    if ("close" in this.#port && typeof this.#port.close === "function") {
      this.#port.close();
    }

    if (
      "terminate" in this.#port && typeof this.#port.terminate === "function"
    ) {
      /** @type {any} */ (this.#port).terminate();
    }

    // Reject all pending requests
    for (const [id, { reject }] of Object.entries(this.#pending)) {
      reject(new Error("RPC channel destroyed"));
      delete this.#pending[id];
    }
  }

  /**
   * @param {{ id: string, method: string, args: any[] }} msg
   */
  #handleRequest(msg) {
    const { id, method, args } = msg;
    const fn = this.#actions?.[/** @type {keyof LocalAPI} */ (method)];

    if (typeof fn !== "function") {
      this.#port.postMessage({
        __rpc: true,
        id,
        type: "response",
        error: `Method "${method}" not found`,
      });
      return;
    }

    try {
      Promise.resolve(fn(...args)).then(
        (result) => {
          this.#port.postMessage({ __rpc: true, id, type: "response", result });
        },
        (err) => {
          this.#port.postMessage({
            __rpc: true,
            id,
            type: "response",
            error: err?.message ?? String(err),
          });
        },
      );
    } catch (err) {
      this.#port.postMessage({
        __rpc: true,
        id,
        type: "response",
        error: /** @type {Error} */ (err)?.message ?? String(err),
      });
    }
  }

  /**
   * @param {{ id: string, result?: any, error?: string }} msg
   */
  #handleResponse(msg) {
    const pending = this.#pending[msg.id];
    if (!pending) return;
    delete this.#pending[msg.id];

    if (msg.error !== undefined) {
      pending.reject(new Error(msg.error));
    } else {
      pending.resolve(msg.result);
    }
  }
}
