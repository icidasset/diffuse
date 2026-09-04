import { computed, signal } from "~/common/signal.js";
import { OutputTransformer } from "../../base.js";
import { defineElement } from "~/common/element.js";

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
 * @import { Setting, Track } from "~/definitions/types.d.ts"
 * @import { OutputManager } from "@specs/components/output/types.d.ts"
 */

/**
 * Output transformer that encrypts track URIs and setting values using a
 * passkey-derived key.
 *
 * On read, decrypts `encrypted://` URIs in tracks and `encrypted://`-encoded
 * JSON in setting values transparently. On write, re-encrypts before passing
 * downstream.
 *
 * Tracks/settings that cannot be decrypted (no key in memory) are held in
 * `lockedTracks`/`lockedSettings` and excluded from the visible collection.
 *
 * @extends {OutputTransformer}
 */
class PasskeyEncryptionTransformer extends OutputTransformer {
  static NAME = "diffuse/transformer/output/refiner/passkey-encryption";

  #tracks;

  constructor() {
    super();

    const base = this.base();

    const encryptionKey = this.#encryptionKey;
    const lockedSettings = this.#lockedSettings;
    const lockedTracks = this.#lockedTracks;

    this.facets = base.facets;
    this.playlistItems = base.playlistItems;
    this.ready = this.#keyReady.get;

    // Settings
    /** @type {OutputManager["settings"]} */
    this.settings = {
      ...base.settings,

      collection: computed(() => {
        const col = base.settings.collection();
        if (col?.state !== "loaded") {
          return col?.state === "error" ? { state: "error" } : { state: "loading" };
        }

        const key = encryptionKey.get();

        /** @type {Setting[]} */
        const unlocked = [];

        /** @type {Setting[]} */
        const locked = [];

        for (const setting of col.data) {
          const value = setting.value;
          if (typeof value === "string" && isEncryptedUri(value)) {
            if (key) {
              try {
                unlocked.push({
                  ...setting,
                  value: decryptUri(key, value),
                });
              } catch {
                locked.push(setting);
              }
            } else {
              locked.push(setting);
            }
          } else {
            unlocked.push(setting);
          }
        }

        lockedSettings.set(locked);
        return { state: "loaded", data: unlocked };
      }),

      save: async (/** @type {Setting[]} */ newSettings) => {
        const key = encryptionKey.get();

        if (key) {
          newSettings = newSettings.map((setting) => ({
            ...setting,
            value: encryptUri(key, setting.value),
          }));

          // Re-append still-locked settings so they are not lost
          newSettings = newSettings.concat(lockedSettings.value);
        }

        await base.settings.save(newSettings);
      },
    };

    // Tracks
    this.#tracks = () => {
      const col = base.tracks.collection();

      if (col?.state !== "loaded") {
        return col?.state === "error"
          ? { state: "error", locked: [], unlocked: [] }
          : { state: "loading", locked: [], unlocked: [] };
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
  #lockedSettings = signal(/** @type {Setting[]} */ ([]));
  #lockedTracks = signal(/** @type {Track[]} */ ([]));

  passkeyActive = computed(() => this.#encryptionKey.get() !== null);
  lockedSettings = this.#lockedSettings.get;
  lockedTracks = this.#lockedTracks.get;

  // LIFECYCLE

  /** @override */
  connectedCallback() {
    if (this.hasAttribute("group")) {
      const channelName = this.namespace?.length
        ? `${PasskeyEncryptionTransformer.NAME}/${this.namespace}/${this.group}`
        : `${PasskeyEncryptionTransformer.NAME}/${this.group}`;

      const actions = this.broadcast(channelName, {
        getLockedSettings: {
          strategy: "leaderOnly",
          fn: this.#lockedSettings.get,
        },
        setLockedSettings: {
          strategy: "replicate",
          fn: this.#lockedSettings.set,
        },
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
        this.#lockedSettings.set = actions.setLockedSettings;
        this.#lockedTracks.set = actions.setLockedTracks;

        actions.getLockedSettings().then((locked) => {
          this.#lockedSettings.value = locked;
        });

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
   * Replace the active cipher key with `newKey` and re-encrypt existing data
   * under it. The stored settings/tracks must be read while the current key is
   * still in memory so they decrypt to plaintext; once captured, the key is
   * switched to `newKey` and that plaintext is saved, which re-encrypts it with
   * the new key.
   *
   * Both collections are captured in the same reactive snapshot so a single
   * save is consistent, matching how `removePasskey` reads before clearing.
   *
   * @param {string} namespace
   * @param {Uint8Array} newKey
   */
  async #rekey(namespace, newKey) {
    await storeCipherKey(namespace, newKey);

    /** @type {Setting[]} */ let settingsData = [];
    /** @type {Track[]} */ let tracksData = [];

    await new Promise((resolve) => {
      let captured = false;

      const stop = this.effect(() => {
        const settingsCol = this.settings.collection();
        const tracksCol = this.tracks.collection();

        if (settingsCol.state !== "loaded" || tracksCol.state !== "loaded") return;
        if (captured) return;

        captured = true;
        settingsData = settingsCol.data;
        tracksData = tracksCol.data;

        stop();

        this.#encryptionKey.value = newKey;

        void this.settings.save(settingsData);
        void this.tracks.save(tracksData);

        resolve(undefined);
      });
    });
  }

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

    const newKey = await deriveCipherKey(result.prfSecond);
    await this.#rekey(namespace, newKey);
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

    const newKey = await deriveCipherKey(result.prfSecond);
    await this.#rekey(namespace, newKey);
  }

  /**
   * Remove the stored passkey credential and clear in-memory key material.
   */
  async removePasskey() {
    const namespace = this.namespace ?? "";
    await removeStoredPasskey(namespace);

    // Both collections must be captured in the same reactive snapshot before
    // clearing the key. If the key were cleared between the two reads, the
    // second collection would evaluate with key=null and show encrypted items
    // as locked (invisible), causing them to be silently dropped on save.
    let removed = false;

    const stop = this.effect(() => {
      if (removed) { stop(); return; }

      const settingsCol = this.settings.collection();
      const tracksCol = this.tracks.collection();

      if (settingsCol.state !== "loaded" || tracksCol.state !== "loaded") return;

      removed = true;

      this.#encryptionKey.value = null;

      this.settings.save(settingsCol.data);
      this.tracks.save(tracksCol.data);
    });
  }
}

export default PasskeyEncryptionTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = PasskeyEncryptionTransformer;
export const NAME = "dtor-passkey-encryption";

defineElement(NAME, CLASS);
