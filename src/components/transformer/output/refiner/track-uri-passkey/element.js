import { computed, signal } from "~/common/signal.js";
import { OutputTransformer } from "../../base.js";

import {
  adoptPasskeyPrfResult,
  createPasskey,
  decryptUri,
  deriveCipherKey,
  encryptUri,
  isEncryptedUri,
  loadStoredCipherKey,
  removeStoredPasskey,
  storeCipherKey,
} from "./passkey.js";

/**
 * @import { Track } from "~/definitions/types.d.ts"
 * @import { OutputManager } from "~/components/output/types.d.ts"
 */

/**
 * Output transformer that encrypts track URIs using a passkey-derived key.
 *
 * Sits in front of any output element. On read (`tracks.collection`),
 * decrypts `encrypted://` URIs transparently. On write (`tracks.save`),
 * re-encrypts all URIs before passing them downstream.
 *
 * Tracks whose URIs cannot be decrypted (no key in memory) are held
 * in `lockedTracks` and excluded from the visible collection.
 *
 * @extends {OutputTransformer}
 */
class TrackUriPasskeyTransformer extends OutputTransformer {
  static NAME = "diffuse/transformer/output/refiner/track-uri-passkey";

  #tracks;

  constructor() {
    super();

    const base = this.base();

    const encryptionKey = this.#encryptionKey;
    const lockedTracks = this.#lockedTracks;

    this.facets = base.facets;
    this.playlistItems = base.playlistItems;
    this.themes = base.themes;
    this.ready = this.#keyReady.get;

    // Tracks
    this.#tracks = () => {
      const col = base.tracks.collection();

      if (col?.state !== "loaded") {
        return { state: "loading", locked: [], unlocked: [] };
      }

      const key = encryptionKey.get();

      /** @type {Track[]} */
      const unlocked = [];

      /** @type {Track[]} */
      const locked = [];

      for (const track of col.data) {
        if (!isEncryptedUri(track.uri)) {
          unlocked.push(track);
        } else if (key) {
          try {
            unlocked.push({ ...track, uri: decryptUri(key, track.uri) });
          } catch {
            locked.push(track);
          }
        } else {
          locked.push(track);
        }
      }

      return { state: "loaded", locked, unlocked };
    };

    /** @type {OutputManager["tracks"]} */
    this.tracks = {
      ...base.tracks,

      collection: computed(() => {
        const result = this.#tracks();
        if (result.state === "loading") return { state: "loading" };
        lockedTracks.set(result.locked);
        return { state: "loaded", data: result.unlocked };
      }),

      save: async (/** @type {Track[]} */ newTracks) => {
        const key = encryptionKey.get();

        if (key) {
          newTracks = newTracks.map((track) => ({
            ...track,
            uri: encryptUri(key, track.uri),
          }));

          // Re-append still-locked tracks so they are not lost
          newTracks = newTracks.concat(lockedTracks.value);
        }

        await base.tracks.save(newTracks);
      },
    };
  }

  // SIGNALS

  #encryptionKey = signal(/** @type {Uint8Array | null} */ (null));
  #keyReady = signal(false);
  #lockedTracks = signal(/** @type {Track[]} */ ([]));

  passkeyActive = computed(() => this.#encryptionKey.get() !== null);
  lockedTracks = this.#lockedTracks.get;

  // LIFECYCLE

  /** @override */
  connectedCallback() {
    if (this.hasAttribute("group")) {
      const channelName = this.namespace?.length
        ? `${TrackUriPasskeyTransformer.NAME}/${this.namespace}/${this.group}`
        : `${TrackUriPasskeyTransformer.NAME}/${this.group}`;

      const actions = this.broadcast(channelName, {
        getLockedTracks: {
          strategy: "leaderOnly",
          fn: this.#lockedTracks.get,
        },
        setLockedTracks: {
          strategy: "replicate",
          fn: this.#lockedTracks.set,
        },
      });

      if (actions) {
        this.#lockedTracks.set = actions.setLockedTracks;

        actions.getLockedTracks().then((locked) => {
          this.#lockedTracks.value = locked;
        });
      }
    }

    super.connectedCallback();

    loadStoredCipherKey(this.namespace ?? "").then((key) => {
      if (key) {
        this.#encryptionKey.value = key;
      }

      this.#keyReady.value = true;
    });
  }

  // PASSKEY

  /**
   * Register a new passkey for track URI encryption.
   * Throws if the authenticator does not support the PRF extension.
   */
  async setupPasskey() {
    const namespace = this.namespace ?? "";
    const result = await createPasskey(namespace);

    if (!result.supported) {
      throw new Error(result.reason);
    }

    const key = await deriveCipherKey(result.prfSecond);
    await storeCipherKey(namespace, key);
    this.#encryptionKey.value = key;

    let saved = false;

    const stop = this.effect(() => {
      if (saved) {
        stop();
        return;
      }

      const col = this.tracks.collection();
      if (col.state === "loading") return;

      saved = true;
      this.tracks.save(col.data);
    });
  }

  /**
   * Adopt an existing passkey from another device via discoverable-credential
   * lookup. Stores the credential ID locally and derives the cipher key.
   */
  async adoptPasskey() {
    const namespace = this.namespace ?? "";
    const result = await adoptPasskeyPrfResult(namespace);

    if (!result.supported) {
      throw new Error(result.reason);
    }

    const key = await deriveCipherKey(result.prfSecond);
    await storeCipherKey(namespace, key);
    this.#encryptionKey.value = key;

    let saved = false;

    const stop = this.effect(() => {
      if (saved) {
        stop();
        return;
      }

      const col = this.tracks.collection();
      if (col.state !== "loaded") return;

      saved = true;
      this.tracks.save(col.data);
    });
  }

  /**
   * Remove the stored passkey credential and clear in-memory key material.
   */
  async removePasskey() {
    const namespace = this.namespace ?? "";
    await removeStoredPasskey(namespace);

    let removed = false;

    const stop = this.effect(() => {
      if (removed) {
        stop();
        return;
      }

      const col = this.tracks.collection();
      if (col.state !== "loaded") return;

      removed = true;

      this.#encryptionKey.value = null;
      this.tracks.save(col.data);
    });
  }
}

export default TrackUriPasskeyTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = TrackUriPasskeyTransformer;
export const NAME = "dtor-track-uri-passkey";

customElements.define(NAME, CLASS);
