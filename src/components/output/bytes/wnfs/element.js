import * as IDB from "idb-keyval";
import { CID } from "multiformats/cid";
import { base64url } from "iso-base/rfc4648";
import { FileSystem, Path } from "@wnfs-wg/nest";

import { computed, signal } from "~/common/signal.js";
import { BroadcastedOutputElement, outputManager } from "../../common.js";
import { defineElement } from "~/common/element.js";
import { byField, byUriScheme } from "~/common/bundles.js";
import {
  decodeJsonCollection,
  encodeJsonCollection,
} from "~/common/lens.js";

import { FirkinBlockstore } from "./blockstore.js";
import { readCollection, writeCollection } from "./files.js";
import {
  WnfsFirkinRootTree,
} from "./root-tree.js";
import {
  decryptPrivateKey,
  deriveStorageKey,
  encryptPrivateKey,
  generateExchangeKeyPair,
  importExchangePrivateKey,
} from "./passkey.js";
import { readLocalCollection } from "./local.js";
import {
  clientForSession,
  deleteSession,
  readSession,
} from "~/common/firkin/session.js";
import {
  passkeyDid as derivePasskeyDid,
  publishSessionDelegation,
  signInAccount,
  UnregisteredAccountError,
} from "~/common/firkin/account.js";
import {
  EXCHANGE_KEY_NAMESPACE,
  IDB_LOCAL_PREFIX,
  IDB_LOCAL_ROOT,
  IDB_STORAGE,
  REGISTRY_NAMESPACE,
  REGISTRY_PRIVATE_KEY,
  REGISTRY_ROOT,
  RESTORE_TIMEOUT_MS,
  UPLOAD_DEBOUNCE_MS,
  WNFS_OUTPUT_DIR,
} from "./constants.js";

/**
 * @import { OutputElement, OutputManager } from "@specs/components/output/types.d.ts"
 * @import { WnfsFirkinOutputElement } from "@specs/components/output/bytes/wnfs/types.d.ts"
 * @import { BundleSelector } from "~/common/bundles.d.ts"
 * @import { CollectionName } from "~/common/self-describing.js"
 * @import { Firkin } from "@tokono.ma/firkin"
 * @import { Session } from "~/common/firkin/session.js"
 * @import { Track } from "~/definitions/types.d.ts"
 *
 * @typedef {Object} Storage
 * @property {string} apiBase
 * @property {string} did
 * @property {string} root
 * @property {CryptoKey} privateKey
 * @property {Uint8Array} publicKey
 * @property {string} shareId
 *
 * @typedef {Object} EncryptedKeyPayload
 * @property {number} v
 * @property {string} shareId
 * @property {JsonWebKey} privateKey
 * @property {string} publicKey
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Collections copied from the local output when their WNFS file is missing.
 *
 * @type {readonly CollectionName[]}
 */
const SEED_COLLECTIONS = [
  "collaborations",
  "facets",
  "playlistItems",
  "settings",
  "tracks",
];

/**
 * Stores user-data in a private WNFS (via `@wnfs-wg/nest`) with all blocks
 * held by a Firkin instance.
 *
 * The account is a passkey: its PRF "signing" output derives the account's
 * ed25519 key. That key immediately delegates every `/firkin` capability to a
 * session key kept in IndexedDB. The WNFS data root is stored in the account
 * registry, and the WNFS exchange private key is stored there too, encrypted
 * with a symmetric key derived from the passkey's PRF "encryption" output.
 *
 * Playlist items are written as one WNFS file per playlist; tracks as one file
 * per URI scheme (or per input source when a composer sets `tracksBy`), so a
 * save only rewrites the bundles that changed. The other collections stay one
 * file each.
 *
 * @implements {OutputElement}
 * @implements {WnfsFirkinOutputElement}
 */
class WnfsFirkinOutput extends BroadcastedOutputElement {
  static NAME = "diffuse/output/bytes/wnfs";

  /**
   * Optional grouping rule for tracks, supplied by the composition layer.
   * When absent, tracks are grouped by their URI scheme.
   *
   * @type {BundleSelector<Track> | undefined}
   */
  tracksBy = undefined;

  /** @type {OutputManager} */
  #manager;

  // SIGNALS

  #isOnline = signal(navigator.onLine);
  #ready = signal(false);
  #session = signal(/** @type {Session | undefined} */ (undefined));
  #storage = signal(/** @type {Storage | undefined} */ (undefined));
  #lastPasskeyDid = signal(/** @type {string | undefined} */ (undefined));

  // STATE

  /** @type {FileSystem | undefined} */
  #fs;

  /** @type {FirkinBlockstore | undefined} */
  #blockstore;

  /** @type {Firkin | undefined} */
  #rb;

  /** @type {Promise<void>} */
  #restorePromise = Promise.resolve();

  /** @type {Promise<unknown>} */
  #writeQueue = Promise.resolve();

  /** @type {Promise<void> | undefined} */
  #mountPromise;

  /** Whether the local collection cache matches the current WNFS root. */
  #localFresh = false;

  /** @type {Map<CollectionName, Promise<void>>} */
  #refreshing = new Map();

  /** @type {{ root: string; resolve: () => void; reject: (err: unknown) => void }[]} */
  #uploadPending = [];

  /** @type {ReturnType<typeof setTimeout> | undefined} */
  #uploadTimer;

  #uploading = false;

  /** @type {Set<CollectionName>} */
  #accessedNames = new Set();

  constructor() {
    super();

    this.#manager = /** @type {OutputManager} */ (
      /** @type {unknown} */ (outputManager({
      collaborations: {
        empty: () => [],
        get: () => this.#get("collaborations"),
        put: (data) => this.#put("collaborations", data),
      },
      facets: {
        empty: () => [],
        get: () => this.#get("facets"),
        put: (data) => this.#put("facets", data),
      },
      init: () => this.whenConnected(),
      playlistItems: {
        empty: () => [],
        get: () => this.#get("playlistItems"),
        put: (data) => this.#put("playlistItems", data),
      },
      settings: {
        empty: () => [],
        get: () => this.#get("settings"),
        put: (data) => this.#put("settings", data),
      },
      tracks: {
        empty: () => [],
        get: () => this.#get("tracks"),
        put: (data) => this.#put("tracks", data),
      },
      }))
    );

    this.collaborations = this.#manager.collaborations;
    this.facets = this.#manager.facets;
    this.playlistItems = this.#manager.playlistItems;
    this.settings = this.#manager.settings;
    this.tracks = this.#manager.tracks;

    this.ready = computed(() => this.#ready.value && this.#isOnline.value);
  }

  // SIGNALS

  did = computed(() => this.#session.value?.did);
  apiBase = computed(() =>
    this.#session.value?.apiBase ?? this.#storage.value?.apiBase
  );
  dataRoot = computed(() => this.#storage.value?.root);
  sessionExpiresAt = computed(() => this.#session.value?.expiresAt);

  /**
   * The DID the most recent passkey flow derived. Set even when that flow then
   * failed — a freshly created passkey whose account the instance has not
   * registered yet is exactly when the UI needs it.
   */
  lastPasskeyDid = computed(() => this.#lastPasskeyDid.value);

  // LIFECYCLE

  /** @override */
  connectedCallback() {
    this.replicateSavedData(this.#manager);

    super.connectedCallback();

    globalThis.addEventListener("online", this.#online);
    globalThis.addEventListener("offline", this.#offline);

    this.#restorePromise = this.#restore().catch((err) => {
      console.error("Failed to restore Firkin + WNFS:", err);
    });
  }

  /** @override */
  disconnectedCallback() {
    globalThis.removeEventListener("online", this.#online);
    globalThis.removeEventListener("offline", this.#offline);
    super.disconnectedCallback();
  }

  #offline = () => this.#isOnline.set(false);
  #online = () => {
    this.#isOnline.set(true);

    // A restore attempted while offline (or before the instance was
    // reachable) should be retried once connectivity returns.
    if (!this.#ready.value) {
      this.#restorePromise = this.#restore().catch((err) => {
        console.error("Failed to restore Firkin + WNFS:", err);
      });
    }
  };

  /**
   * Wait for the initial restore, but never indefinitely: a hanging registry
   * request should not keep the dashboard on its loading spinner.
   */
  async #waitForRestore() {
    await Promise.race([
      this.#restorePromise,
      new Promise((resolve) => setTimeout(resolve, RESTORE_TIMEOUT_MS)),
    ]);
  }

  /** Resolves after the initial restore attempt. */
  whenRestored() {
    return this.#restorePromise;
  }

  // ACTIONS

  /**
   * Sign in with a passkey, then make sure the WNFS exists and is mounted.
   *
   * @param {{
   *   apiBase?: string
   *   create?: boolean
   * }} [options]
   */
  async signIn(options = {}) {
    await this.whenConnected();
    await this.#restorePromise;
    await this.#writeQueue.catch(() => {});
    await this.#flushUploads();

    // Reset first: only a DID derived by *this* attempt may be shown, so a
    // cancelled prompt never leaves a stale one in the dialog.
    this.#lastPasskeyDid.value = undefined;

    const account = await signInAccount({
      apiBase: options.apiBase,
      create: options.create,
    }).catch((err) => {
      if (err instanceof UnregisteredAccountError) {
        this.#lastPasskeyDid.value = err.did;
      }
      throw err;
    });

    const { apiBase, did: accountDid, prf, session } = account;

    this.#lastPasskeyDid.value = accountDid;
    this.#session.value = session;
    this.#rb = this.#clientFor(session);

    const storageKey = await deriveStorageKey(prf.prfSecond);
    const { root: rootCid, key: keyCid } = await this.#readRegistryPointers();

    /** @type {Storage} */
    let storage;

    if (rootCid && keyCid) {
      storage = await this.#loadExistingStorage(
        rootCid,
        keyCid,
        storageKey,
        apiBase,
        accountDid,
      );
    } else if (rootCid && !keyCid) {
      const cached = await IDB.get(IDB_STORAGE);
      if (
        cached &&
        cached.apiBase === apiBase &&
        cached.did === accountDid &&
        cached.privateKey instanceof CryptoKey
      ) {
        storage = cached;
        await this.#mountStorage(storage, rootCid);
      } else {
        throw new Error(
          "Firkin + WNFS exists, but its private key is missing from the registry and this browser.",
        );
      }
    } else {
      storage = await this.#createStorage(storageKey, apiBase, accountDid);
    }

    // A new WNFS starts without collection files. Copy the local output's
    // collections over before selecting it: the local output has the user's
    // current data (including the preludes that register this output), and the
    // output configurator mirrors the selected output back into the local
    // fallback — mirroring an empty, freshly created output would erase it.
    await this.#seedFromLocal(storage);
    await this.#setLocalFreshness(
      storage.root,
      rootCid ? storage.root : undefined,
    );

    await IDB.set(IDB_STORAGE, storage);
    this.#storage.value = storage;
    this.#ready.value = true;

    return {
      did: accountDid,
      root: storage.root,
      created: !rootCid,
    };
  }

  /**
   * Create a passkey (or authenticate with an existing one) and return the DID
   * it derives. Purely local: it touches neither the session nor the WNFS, and
   * makes no request, which is what the first-time setup needs — a new
   * account's DID has to be registered with the instance before it can be
   * used for anything.
   *
   * @param {{ create?: boolean }} [options]
   * @returns {Promise<string>}
   */
  async passkeyDid(options = {}) {
    await this.whenConnected();

    const did = await derivePasskeyDid(options);

    this.#lastPasskeyDid.value = did;

    return did;
  }

  /**
   * Forget the local session and storage pointer.
   *
   * This does not revoke the server-side delegation; it expires after a year.
   */
  async signOut() {
    await this.#writeQueue.catch(() => {});
    await this.#flushUploads();
    await Promise.all([deleteSession(), IDB.del(IDB_STORAGE)]);

    this.#session.value = undefined;
    this.#storage.value = undefined;
    this.#lastPasskeyDid.value = undefined;
    this.#fs = undefined;
    this.#blockstore = undefined;
    this.#rb = undefined;
    this.#mountPromise = undefined;
    this.#localFresh = false;
    this.#refreshing.clear();
    this.#ready.value = false;
  }

  // RESTORE

  async #restore() {
    const session = await readSession();
    if (!session) return;

    await publishSessionDelegation(session);

    this.#session.value = session;
    this.#rb = this.#clientFor(session);

    /** @type {Storage | undefined} */
    const storage = await IDB.get(IDB_STORAGE);
    if (!storage) return;
    if (!(storage.privateKey instanceof CryptoKey)) return;
    if (storage.apiBase !== session.apiBase || storage.did !== session.did) {
      return;
    }

    const rootCid = await this.#rb.registryGet(
      REGISTRY_NAMESPACE,
      REGISTRY_ROOT,
    );
    if (!rootCid) return;

    // Lazy mount: reads are served from the local collection cache when it
    // matches this root. The WNFS is only mounted for writes or when the cache
    // is stale/missing. This keeps dashboard startup off the private forest.
    this.#storage.value = { ...storage, root: rootCid };
    this.#ready.value = true;
    await this.#setLocalFreshness(rootCid, storage.root);

    if (rootCid !== storage.root) {
      await IDB.set(IDB_STORAGE, this.#storage.value);
    }
  }

  /**
   * Read both WNFS pointers from the account registry in one request.
   *
   * Firkin's registry is namespaced and `registryNames` returns every name in
   * a namespace, so the root and key pointers share one authenticated read.
   *
   * @returns {Promise<{ root: string | null; key: string | null }>}
   */
  async #readRegistryPointers() {
    const rb = this.#rb;
    if (!rb) throw new Error("Firkin client is not ready");

    const names = await rb.registryNames(REGISTRY_NAMESPACE);

    return {
      root: names?.[REGISTRY_ROOT] ?? null,
      key: names?.[REGISTRY_PRIVATE_KEY] ?? null,
    };
  }

  /**
   * @param {Storage} storage
   * @param {string} rootCid
   */
  async #mountStorage(storage, rootCid) {
    if (!this.#rb) throw new Error("Firkin client is not ready");

    const blockstore = new FirkinBlockstore(this.#rb, rootCid);
    // Assign before mounting: the SDK's dagPull uses this blockstore as its
    // block cache, and the cache adapter resolves `this.#blockstore` lazily.
    this.#blockstore = blockstore;

    const fs = await FileSystem.fromCID(CID.parse(rootCid), {
      blockstore,
      rootTreeClass: /** @type {any} */ (WnfsFirkinRootTree),
    });

    await fs.mountPrivateNode({
      path: Path.root(),
      exchangeKeyPair: {
        publicKey: storage.publicKey,
        privateKey: storage.privateKey,
      },
      shareId: storage.shareId || undefined,
    });

    this.#fs = fs;
  }

  // STORAGE

  /**
   * @param {Uint8Array} storageKey
   * @param {string} apiBase
   * @param {string} did
   * @returns {Promise<Storage>}
   */
  async #createStorage(storageKey, apiBase, did) {
    if (!this.#rb) throw new Error("Firkin client is not ready");

    const blockstore = new FirkinBlockstore(this.#rb);
    // Assign early: the SDK's push/pull cache adapter resolves
    // `this.#blockstore` lazily.
    this.#blockstore = blockstore;

    const fs = await FileSystem.create({
      blockstore,
      rootTreeClass: /** @type {any} */ (WnfsFirkinRootTree),
    });

    await fs.assignIdentifier(did);

    const keyPair = await generateExchangeKeyPair();
    const privateJwk = await crypto.subtle.exportKey("jwk", keyPair.privateKey);
    const publicJwk = await crypto.subtle.exportKey("jwk", keyPair.publicKey);

    const publicKey = base64url.decode(
      /** @type {string} */ (publicJwk.n),
    );

    await fs.registerExchangeKey(EXCHANGE_KEY_NAMESPACE, publicKey);

    const { shareId } = await fs.createPrivateNode({
      path: Path.root(),
      exchangeKeyPair: {
        publicKey,
        privateKey: keyPair.privateKey,
      },
    });

    await fs.ensureDirectory(Path.priv(WNFS_OUTPUT_DIR));

    const root = (await fs.calculateDataRoot()).toString();
    blockstore.setRoot(root);

    // Upload the WNFS blocks first, then the encrypted private key, and only
    // then point the registry at the root. A partial failure leaves the old
    // (or no) root, never a root with missing blocks.
    await blockstore.flush(root);

    /** @type {EncryptedKeyPayload} */
    const payload = {
      v: 1,
      shareId,
      privateKey: privateJwk,
      publicKey: base64url.encode(publicKey),
    };

    const encrypted = encryptPrivateKey(storageKey, payload);
    const { cid: keyCid } = await this.#rb.upload(encrypted);

    await this.#rb.registrySet(
      REGISTRY_NAMESPACE,
      REGISTRY_PRIVATE_KEY,
      keyCid,
    );
    await this.#rb.registrySet(REGISTRY_NAMESPACE, REGISTRY_ROOT, root);

    // Keep a non-extractable handle for everyday use.
    const privateKey = await importExchangePrivateKey(privateJwk);

    this.#fs = fs;

    return { apiBase, did, root, privateKey, publicKey, shareId };
  }

  /**
   * @param {string} rootCid
   * @param {string} keyCid
   * @param {Uint8Array} storageKey
   * @param {string} apiBase
   * @param {string} did
   * @returns {Promise<Storage>}
   */
  async #loadExistingStorage(rootCid, keyCid, storageKey, apiBase, did) {
    if (!this.#rb) throw new Error("Firkin client is not ready");

    const encrypted = await this.#rb.get(keyCid);

    /** @type {EncryptedKeyPayload} */
    let payload;
    try {
      payload = /** @type {EncryptedKeyPayload} */ (
        decryptPrivateKey(storageKey, encrypted)
      );
    } catch {
      throw new Error(
        "Could not decrypt the Firkin + WNFS key. Sign in with the passkey that created this storage.",
      );
    }

    if (payload.v !== 1) {
      throw new Error(`Unsupported Firkin + WNFS key version: ${payload.v}`);
    }

    const publicKey = base64url.decode(payload.publicKey);
    const privateKey = await importExchangePrivateKey(payload.privateKey);

    /** @type {Storage} */
    const storage = {
      apiBase,
      did,
      root: rootCid,
      privateKey,
      publicKey,
      shareId: payload.shareId ?? "",
    };

    await this.#mountStorage(storage, rootCid);
    return storage;
  }

  /**
   * Seed missing collection files from the local (default) output.
   *
   * A new WNFS starts without collection files, while the local output still
   * has the user's current data. Copy it over before the output is selected:
   * the output configurator mirrors the selected output back into the local
   * fallback, so mirroring an empty new output would erase the user's data.
   *
   * Collections that already have a file are left untouched: the file existing
   * means the WNFS has been written to (possibly deliberately emptied).
   *
   * @param {Storage} storage
   */
  async #seedFromLocal(storage) {
    const fs = this.#fs;
    const blockstore = this.#blockstore;
    const rb = this.#rb;
    if (!fs || !blockstore || !rb) return;

    const dir = Path.priv(WNFS_OUTPUT_DIR);
    if (!(await fs.exists(dir))) {
      await fs.ensureDirectory(dir);
    }

    let seeded = false;

    for (const name of SEED_COLLECTIONS) {
      const path = Path.priv(WNFS_OUTPUT_DIR, name);
      if (await fs.exists(path)) continue;

      const data = await readLocalCollection(name);
      if (data.length === 0) continue;

      await writeCollection(fs, name, data, this.#selectorFor(name));
      await this.#writeLocalCache(
        name,
        /** @type {Uint8Array} */ (encodeJsonCollection(data, name, true)),
      );
      seeded = true;
    }

    if (!seeded) return;

    const root = (await fs.calculateDataRoot()).toString();
    await blockstore.flush(root);
    blockstore.setRoot(root);

    await rb.registrySet(REGISTRY_NAMESPACE, REGISTRY_ROOT, root);
    storage.root = root;

    await IDB.set(IDB_LOCAL_ROOT, root);
    this.#localFresh = true;
  }

  /** @param {Session} session */
  #clientFor(session) {
    return clientForSession(session, {
      cache: {
        /** @param {string} cid */
        get: (cid) => this.#blockstore?.peek(cid),
        /** @param {string} cid; @param {Uint8Array} bytes */
        set: (cid, bytes) => {
          this.#blockstore?.seed(cid, bytes);
        },
        keys: () => this.#blockstore?.keys() ?? [],
      },
    });
  }

  // COLLECTIONS

  /**
   * @param {string} root
   * @param {string | undefined} knownRoot - Cached storage root, used as a
   *   legacy hint when `IDB_LOCAL_ROOT` has not been written yet.
   */
  async #setLocalFreshness(root, knownRoot) {
    const cached = await IDB.get(IDB_LOCAL_ROOT);
    this.#localFresh = cached === undefined
      ? knownRoot === root
      : cached === root;

    if (this.#localFresh && cached === undefined) {
      await IDB.set(IDB_LOCAL_ROOT, root);
    }
  }

  /** Mount the WNFS on demand (reads can be served from the local cache). */
  async #ensureMounted() {
    if (this.#fs) return;
    if (this.#mountPromise) return await this.#mountPromise;

    const storage = this.#storage.value;
    if (!storage) throw new Error("Firkin + WNFS is not configured");

    this.#mountPromise = this.#mountStorage(storage, storage.root).finally(
      () => {
        this.#mountPromise = undefined;
      },
    );

    await this.#mountPromise;
  }

  /** @param {CollectionName} name */
  #scheduleRefresh(name) {
    const run = () => {
      void this.#refreshCollection(name);
    };

    if (typeof requestIdleCallback === "function") {
      requestIdleCallback(run, { timeout: 2_000 });
    } else {
      setTimeout(run, 0);
    }
  }

  /**
   * The grouping rule for a collection, or `undefined` when it is stored as a
   * single file.
   *
   * Playlist items are one file per playlist. Tracks are one file per input
   * source (an S3 bucket, a Subsonic server, a local folder, …), falling back
   * to their URI scheme when no input can name the source.
   *
   * @param {CollectionName} name
   */
  #selectorFor(name) {
    if (name === "playlistItems") return byField("playlist");
    if (name !== "tracks") return undefined;

    const fallback = byUriScheme();
    const custom = this.tracksBy;

    if (!custom) return fallback;

    /** @type {BundleSelector<Track>} */
    const selector = (track) => custom(track) ?? fallback(track);
    return selector;
  }

  /** @param {CollectionName} name */
  #refreshCollection(name) {
    const existing = this.#refreshing.get(name);
    if (existing) return existing;

    const pending = (async () => {
      try {
        await this.#ensureMounted();
        const fs = this.#fs;
        if (!fs) return;

        const data = await readCollection(fs, name);
        await this.#writeLocalCache(
          name,
          /** @type {Uint8Array} */ (encodeJsonCollection(data, name, true)),
        );
        this.#manager.signals[name].value = data;
      } catch (err) {
        console.error(`Failed to refresh Firkin + WNFS '${name}':`, err);
      } finally {
        this.#refreshing.delete(name);
      }
    })();

    this.#refreshing.set(name, pending);
    return pending;
  }

  // GET & PUT

  /**
   * @param {CollectionName} name
   * @returns {Promise<any[]>}
   */
  #get = async (name) => {
    this.#accessedNames.add(name);

    await this.whenConnected();
    await this.#writeQueue;

    // Cache-first: serve the local collection immediately. The WNFS is only
    // mounted when the cache is stale/absent or on a write. This avoids walking
    // the (potentially large) private forest on every dashboard load.
    const local = await this.#readLocalCache(name);

    if (local.length > 0) {
      if (this.#ready.value && !this.#localFresh) {
        this.#scheduleRefresh(name);
      } else if (!this.#ready.value) {
        // Restore is still in flight. Refresh after it settles (and after the
        // caller has applied the local snapshot) if the root changed.
        this.#restorePromise.then(() => {
          setTimeout(() => {
            if (this.#ready.value && !this.#localFresh) {
              this.#scheduleRefresh(name);
            }
          }, 0);
        });
      }
      return local;
    }

    await this.#waitForRestore();

    if (!this.#ready.value) return [];

    await this.#ensureMounted();

    const fs = this.#fs;
    if (!fs) return [];

    try {
      const data = await readCollection(fs, name);
      if (data.length === 0) return [];

      await this.#writeLocalCache(
        name,
        /** @type {Uint8Array} */ (encodeJsonCollection(data, name, true)),
      );
      return data;
    } catch (err) {
      console.error(`Failed to read Firkin + WNFS '${name}':`, err);
      return await this.#readLocalCache(name);
    }
  };

  /**
   * @param {CollectionName} name
   * @param {any[]} data
   */
  #put = (name, data) => {
    const write = async () => {
      await this.whenConnected();
      await this.#restorePromise;

      const rb = this.#rb;
      const storage = this.#storage.value;

      if (!this.#ready.value || !rb || !storage) {
        throw new Error("Firkin + WNFS is not connected");
      }

      await this.#ensureMounted();

      const fs = this.#fs;
      const blockstore = this.#blockstore;
      if (!fs || !blockstore) {
        throw new Error("Firkin + WNFS could not be mounted");
      }

      const bytes = /** @type {Uint8Array} */ (
        encodeJsonCollection(data, name, true)
      );

      const dir = Path.priv(WNFS_OUTPUT_DIR);
      if (!(await fs.exists(dir))) {
        await fs.ensureDirectory(dir);
      }

      await writeCollection(fs, name, data, this.#selectorFor(name));

      // Keep the local fallback current even if the upload fails.
      await this.#writeLocalCache(name, bytes);

      const root = (await fs.calculateDataRoot()).toString();
      return { root, changed: root !== storage.root };
    };

    // Serialize only the WNFS mutations. The upload is coalesced by the
    // debounced queue, so rapid saves share one `/dag/push`.
    const written = this.#writeQueue.then(write, write);
    this.#writeQueue = written.then(() => {}, () => {});
    return written.then(({ root, changed }) =>
      changed ? this.#enqueueUpload(root) : undefined
    );
  };

  /**
   * Queue an upload for `root`. Multiple saves within the debounce window are
   * batched into one `/dag/push` rooted at the latest root.
   *
   * @param {string} root
   */
  #enqueueUpload(root) {
    return new Promise((resolve, reject) => {
      this.#uploadPending.push({
        root,
        resolve: () => resolve(undefined),
        reject,
      });

      if (this.#uploadTimer !== undefined) clearTimeout(this.#uploadTimer);
      this.#uploadTimer = setTimeout(() => {
        this.#uploadTimer = undefined;
        this.#drainUploads();
      }, UPLOAD_DEBOUNCE_MS);
    });
  }

  async #drainUploads() {
    if (this.#uploading || this.#uploadPending.length === 0) return;
    this.#uploading = true;

    const batch = this.#uploadPending;
    this.#uploadPending = [];
    const root = batch[batch.length - 1].root;

    try {
      const blockstore = this.#blockstore;
      const rb = this.#rb;
      const storage = this.#storage.value;
      if (!blockstore || !rb || !storage) {
        throw new Error("Firkin + WNFS is not connected");
      }

      await blockstore.flush(root);
      await rb.registrySet(REGISTRY_NAMESPACE, REGISTRY_ROOT, root);
      blockstore.setRoot(root);

      const next = { ...storage, root };
      this.#storage.value = next;
      await IDB.set(IDB_STORAGE, next);
      await IDB.set(IDB_LOCAL_ROOT, root);
      this.#localFresh = true;

      for (const item of batch) item.resolve();
    } catch (err) {
      console.error("Failed to upload Firkin + WNFS blocks:", err);
      for (const item of batch) item.reject(err);
    } finally {
      this.#uploading = false;

      if (this.#uploadPending.length > 0) {
        this.#uploadTimer = setTimeout(() => {
          this.#uploadTimer = undefined;
          this.#drainUploads();
        }, UPLOAD_DEBOUNCE_MS);
      }
    }
  }

  /** Flush pending uploads immediately (used when signing out). */
  async #flushUploads() {
    if (this.#uploadTimer !== undefined) {
      clearTimeout(this.#uploadTimer);
      this.#uploadTimer = undefined;
    }

    while (this.#uploading || this.#uploadPending.length > 0) {
      await this.#drainUploads();
      await new Promise((resolve) => setTimeout(resolve, 10));
    }
  }

  // LOCAL CACHE

  /** @param {CollectionName} name */
  #localCacheKey(name) {
    const ns = this.namespace ?? "default";
    return `${IDB_LOCAL_PREFIX}/${ns}/${name}`;
  }

  /**
   * @param {CollectionName} name
   * @returns {Promise<any[]>}
   */
  async #readLocalCache(name) {
    try {
      const cached = await IDB.get(this.#localCacheKey(name));
      if (!(cached instanceof Uint8Array)) return [];
      return decodeJsonCollection(cached, name);
    } catch (err) {
      console.error(
        `Failed to read the local Firkin + WNFS cache for '${name}':`,
        err,
      );
      return [];
    }
  }

  /**
   * @param {CollectionName} name
   * @param {Uint8Array} bytes
   */
  async #writeLocalCache(name, bytes) {
    try {
      await IDB.set(this.#localCacheKey(name), bytes);
    } catch (err) {
      console.error(`Failed to cache Firkin + WNFS file '${name}':`, err);
    }
  }
}

export default WnfsFirkinOutput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = WnfsFirkinOutput;
export const NAME = "dob-wnfs-firkin";

defineElement(NAME, CLASS);
