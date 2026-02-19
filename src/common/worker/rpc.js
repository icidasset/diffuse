/**
 * @import { IoCapabilities, IoInterface, IoMessage, WireEnvelope } from "@kunkun/kkrpc"
 *
 * @import { MessengerRealm } from "../worker.d.ts"
 */

const DESTROY_SIGNAL = "__DESTROY__";

/**
 * @implements {IoInterface}
 */
export class BrowserPostMessageIo {
  name = "browser-postmessage-io";

  /** @type {Array<string | IoMessage>} */
  #messageQueue = [];

  /** @type {((value: string | IoMessage | null) => void) | null} */
  #resolveRead = null;

  /** */
  #realm;

  /** @type {IoCapabilities} */
  capabilities = {
    structuredClone: true,
    transfer: true,
  };

  /**
   * @param {() => MessengerRealm} realmCreator
   */
  constructor(realmCreator) {
    /** @type {undefined | MessengerRealm} */
    const realm = realmCreator();
    realm.addEventListener("message", this.#handleMessage.bind(this));

    this.#realm = () => {
      return realm;
    };
  }

  /**
   * @param {MessageEvent} event
   */
  #handleMessage(event) {
    const raw = event.data;
    const message = this.#normalizeIncoming(raw);

    // Handle destroy signal
    if (message === DESTROY_SIGNAL) {
      this.destroy();
      return;
    }

    if (this.#resolveRead) {
      this.#resolveRead(message);
      this.#resolveRead = null;
    } else {
      this.#messageQueue.push(message);
    }
  }

  /**
   * @param {any} message
   * @returns {string | IoMessage}
   */
  #normalizeIncoming(message) {
    if (typeof message === "string") {
      return message;
    }

    if (message && typeof message === "object" && message.version === 2) {
      const envelope = /** @type {WireEnvelope} */ (message);
      return {
        data: envelope,
        transfers: (/** @type {unknown[] | undefined} */ (envelope
          .__transferredValues)) ?? [],
      };
    }

    return /** @type {string} */ (message);
  }

  /** @returns {Promise<string | IoMessage | null>} */
  read() {
    // If there are queued messages, return the first one
    if (this.#messageQueue.length > 0) {
      return Promise.resolve(this.#messageQueue.shift() ?? null);
    }

    // Otherwise, wait for the next message
    return new Promise((resolve) => {
      this.#resolveRead = resolve;
    });
  }

  /**
   * @param {string | IoMessage} message
   */
  write(message) {
    if (typeof message === "string") {
      this.#realm().postMessage(message);
      return Promise.resolve();
    }

    if (message.transfers && message.transfers.length > 0) {
      const msg = { ...message };

      if (typeof msg.data === "object" && msg.data.payload.args) {
        if (msg.data.payload.args[0] instanceof HTMLElement) {
          msg.data.payload.args[0] = undefined;
        }
      }

      console.log(message)

      this.#realm().postMessage(
        message.data,
        /** @type {Transferable[]} */ (message.transfers),
      );
    } else {
      this.#realm().postMessage(message.data);
    }

    return Promise.resolve();
  }

  destroy() {
    const realm = this.#realm();

    realm.postMessage(DESTROY_SIGNAL);

    if (
      "terminate" in realm && typeof realm.terminate === "function"
    ) {
      realm.terminate();
    }
  }

  signalDestroy() {
    this.#realm().postMessage(DESTROY_SIGNAL);
  }
}
