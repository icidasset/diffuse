import { computed, signal } from "~/common/signal.js";
import { OutputTransformer } from "../../base.js";
import { defineElement } from "~/common/element.js";
import { outputKey } from "~/common/firkin/output-key.js";

import { decryptLog, encryptEntry } from "./crypto.js";

/**
 * @import { SignalReader } from "~/common/signal.d.ts";
 * @import { CollectionName } from "~/common/self-describing.js"
 */

/**
 * @typedef {{ state: "loading" } | { state: "loaded"; data: Uint8Array | undefined } | { state: "error" }} CollectionState
 */

/**
 * Encrypts each value an output stores, transparently.
 *
 * Sits between a consumer (an Automerge transformer, say) and a bytes output:
 * the consumer reads and writes plaintext `Uint8Array`s, the output only ever
 * sees framed ciphertext. Each `save` seals one entry, so appending a delta
 * never re-encrypts history, and a log-shaped output keeps working as-is.
 *
 * Decryption happens in `reload()` — which every consumer calls before reading
 * — because it is asynchronous and a `collection()` read can't be. The effect
 * covers the other case, where the backing output changes on its own.
 *
 * By default the key is the shared Firkin output key (`outputKey()`), read
 * reactively: wiring only has to load this element, and a key derived by a
 * later sign-in — on this page or another tab — is picked up on its own.
 * `setKey` overrides it for manual wirings (tests), keeping those setups free
 * of any passkey or account knowledge.
 *
 * @extends {OutputTransformer<Uint8Array>}
 */
class EncryptBytesOutputTransformer extends OutputTransformer {
  /** Manual override (`setKey`), for tests and custom wirings. */
  #manualKey = signal(/** @type {CryptoKey | undefined} */ (undefined));

  /**
   * The AES-GCM key used for this output: the manual override when one was
   * set, otherwise the shared Firkin output key.
   */
  #key = computed(() => this.#manualKey.get() ?? outputKey());

  constructor() {
    super();

    this.ready = computed(() => this.#key() !== undefined);

    this.collaborations = this.#entry("collaborations");
    this.facets = this.#entry("facets");
    this.playlistItems = this.#entry("playlistItems");
    this.settings = this.#entry("settings");
    this.tracks = this.#entry("tracks");
  }

  /**
   * Override the key for this element; `undefined` returns to the shared
   * Firkin output key. Production wiring needs nothing — the element reads
   * the shared key itself.
   *
   * @param {CryptoKey | undefined} key
   */
  setKey(key) {
    this.#manualKey.set(key);
  }

  /**
   * @param {CollectionName} name
   */
  #entry(name) {
    const base = this.base()[name];

    /** @type {import("~/common/signal.d.ts").Signal<CollectionState>} */
    const state = signal(
      /** @type {CollectionState} */ ({ state: "loading" }),
    );

    /** @type {Uint8Array | undefined} */
    let decrypted;
    /** @type {Promise<void> | undefined} */
    let pending;

    const refresh = () => {
      pending ??= (async () => {
        const key = this.#key();
        const source = base.collection();

        if (source.state !== "loaded") {
          state.set(source);
          return;
        }

        // The base returns the same buffer until it changes; decrypt it once.
        if (source.data === decrypted) return;
        decrypted = source.data;

        if (!key || !(source.data instanceof Uint8Array)) {
          state.set({ state: "loaded", data: undefined });
          return;
        }

        try {
          state.set({
            state: "loaded",
            data: await decryptLog(key, source.data),
          });
        } catch (err) {
          console.warn(`Could not decrypt the '${name}' output:`, err);
          state.set({ state: "error" });
        }
      })().finally(() => {
        pending = undefined;
      });

      return pending;
    };

    this.effect(() => {
      this.#key();
      base.collection();

      void refresh();
    });

    return {
      collection: state.get,
      reload: async () => {
        await base.reload();
        await refresh();
      },
      save: async (/** @type {Uint8Array | undefined} */ bytes) => {
        const key = this.#key();
        if (!key) throw new Error("The output encryption key is not loaded");
        if (!bytes || bytes.length === 0) return;

        await base.save(await encryptEntry(key, bytes));
      },
    };
  }
}

export default EncryptBytesOutputTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = EncryptBytesOutputTransformer;
export const NAME = "dtob-encrypt";

defineElement(NAME, CLASS);
