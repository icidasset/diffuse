import * as IDB from "idb-keyval";
import { CID } from "multiformats/cid";
import { base64url } from "iso-base/rfc4648";
import { FileSystem, Path } from "@wnfs-wg/nest";

import { computed, signal } from "~/common/signal.js";
import { BroadcastedOutputElement, outputManager } from "../../common.js";
import { defineElement } from "~/common/element.js";
import {
  decodeJsonCollection,
  encodeJsonCollection,
} from "~/common/lens.js";

import {
  adoptPasskeyPrfResult,
  createPasskey,
} from "~/components/transformer/output/refiner/passkey-encryption/passkey.js";

import {
  Firkin,
  WebCryptoSigner,
  delegate,
  inlineInvocation,
  invocation,
} from "@tokono.ma/firkin";

import { FirkinBlockstore } from "./blockstore.js";
import {
  WnfsFirkinRootTree,
} from "./root-tree.js";
import {
  accountSignerFromPrf,
  decryptPrivateKey,
  deriveStorageKey,
  encryptPrivateKey,
  generateExchangeKeyPair,
  importExchangePrivateKey,
} from "./passkey.js";
import {
  FILES_MANIFEST,
  FILE_URL_TTL_SECONDS,
  decodeManifest,
  encodeManifest,
} from "./files.js";
import { readLocalCollection } from "./local.js";
import {
  DEFAULT_API_BASE,
  IDB_LOCAL_PREFIX,
  IDB_LOCAL_ROOT,
  IDB_SESSION,
  IDB_STORAGE,
  PASSKEY_NAMESPACE,
  REGISTRY_PRIVATE_KEY,
  REGISTRY_ROOT,
  RESTORE_TIMEOUT_MS,
  SESSION_COMMAND,
  SESSION_TTL_SECONDS,
  UPLOAD_DEBOUNCE_MS,
  WNFS_OUTPUT_DIR,
} from "./constants.js";

/**
 * @import { OutputElement, OutputManager } from "@specs/components/output/types.d.ts"
 * @import { WnfsFirkinOutputElement } from "@specs/components/output/bytes/wnfs/types.d.ts"
 * @import { CollectionName } from "~/common/self-describing.js"
 *
 * @typedef {Object} Session
 * @property {string} apiBase
 * @property {string} did
 * @property {CryptoKey} privateKey
 * @property {Uint8Array} publicKey
 * @property {Uint8Array} proof
 * @property {string} proofCid
 * @property {number} expiresAt
 * @property {boolean} [inlineProofs] - The server rejected delegation publishing
 *   (or it failed), so tokens carry their proof inline instead of by CID.
 * @property {boolean} [published] - The delegation is in the server's store.
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
 * Thrown when a Firkin instance rejects the account: its DID has to be
 * registered before the registry is readable, so the UI surfaces the DID for
 * copying instead of burying it in an error message.
 */
export class UnregisteredAccountError extends Error {
  /** @param {string} did */
  constructor(did) {
    super(
      "This account is not registered with the Firkin instance. Register its DID in the instance's admin console, then sign in.",
    );

    this.name = "UnregisteredAccountError";

    /** The DID that has to be registered. */
    this.did = did;
  }
}

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
 * @implements {OutputElement}
 * @implements {WnfsFirkinOutputElement}
 */
class WnfsFirkinOutput extends BroadcastedOutputElement {
  static NAME = "diffuse/output/bytes/wnfs";

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
    this.replicateSavedData(this.#manager, {
      storageStatus: {
        strategy: "leaderOnly",
        fn: () => this.#filesStatus(),
      },
      storageListFiles: {
        strategy: "leaderOnly",
        fn: () => this.#listFiles(),
      },
      storageRead: {
        strategy: "leaderOnly",
        fn: (name) => this.#readFile(name),
      },
      storageFileURL: {
        strategy: "leaderOnly",
        fn: (name, expireIn) => this.#fileURL(name, expireIn),
      },
      storageUploadFile: {
        strategy: "leaderOnly",
        fn: (name, bytes, mime) => this.#uploadFile(name, bytes, mime),
      },
      storageDeleteFile: {
        strategy: "leaderOnly",
        fn: (name) => this.#deleteFile(name),
      },
    });

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

    const apiBase = (options.apiBase?.trim() || DEFAULT_API_BASE)
      .replace(/\/+$/, "");

    // Reset first: only a DID derived by *this* attempt may be shown, so a
    // cancelled prompt never leaves a stale one in the dialog.
    this.#lastPasskeyDid.value = undefined;

    const prf = await this.#passkey(options.create);

    const accountSigner = await accountSignerFromPrf(prf.prfFirst);
    const accountDid = accountSigner.did();

    this.#lastPasskeyDid.value = accountDid;

    const session = await this.#createSession(
      apiBase,
      accountDid,
      accountSigner,
    );
    await this.#publishSessionDelegation(session);
    this.#session.value = session;
    this.#rb = this.#clientFor(session);

    const storageKey = await deriveStorageKey(prf.prfSecond);

    let rootCid = null;
    let keyCid = null;

    try {
      [rootCid, keyCid] = await Promise.all([
        this.#rb.registryGet(REGISTRY_ROOT),
        this.#rb.registryGet(REGISTRY_PRIVATE_KEY),
      ]);
    } catch (err) {
      const status = /** @type {any} */ (err)?.status;
      if (status === 401 || status === 403) {
        throw new UnregisteredAccountError(accountDid);
      }
      throw err;
    }

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

    const prf = await this.#passkey(options.create ?? false);
    const signer = await accountSignerFromPrf(prf.prfFirst);
    const did = signer.did();

    this.#lastPasskeyDid.value = did;

    return did;
  }

  /** Forget the local session and storage pointer. */
  async signOut() {
    await this.#writeQueue.catch(() => {});
    await this.#flushUploads();
    await Promise.all([IDB.del(IDB_SESSION), IDB.del(IDB_STORAGE)]);

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

  // PASSKEY

  /**
   * Run the passkey flow and return its PRF material.
   *
   * @param {boolean} [create=false]
   * @returns {Promise<{ credentialId: Uint8Array, prfFirst: ArrayBuffer, prfSecond: ArrayBuffer }>}
   */
  async #passkey(create = false) {
    const prf = create
      ? await createPasskey(PASSKEY_NAMESPACE)
      : await adoptPasskeyPrfResult(PASSKEY_NAMESPACE);

    if (!prf.supported) throw new Error(prf.reason);
    if (!prf.prfFirst) {
      throw new Error(
        "This authenticator did not return the PRF signing output",
      );
    }

    return /** @type {{ credentialId: Uint8Array, prfFirst: ArrayBuffer, prfSecond: ArrayBuffer }} */ (
      prf
    );
  }

  // RESTORE

  async #restore() {
    /** @type {Session | undefined} */
    const session = await IDB.get(IDB_SESSION);
    if (!session) return;
    if (session.expiresAt <= Date.now()) return;
    if (!(session.privateKey instanceof CryptoKey)) return;

    await this.#publishSessionDelegation(session);

    this.#session.value = session;
    this.#rb = this.#clientFor(session);

    /** @type {Storage | undefined} */
    const storage = await IDB.get(IDB_STORAGE);
    if (!storage) return;
    if (!(storage.privateKey instanceof CryptoKey)) return;
    if (storage.apiBase !== session.apiBase || storage.did !== session.did) {
      return;
    }

    const rootCid = await this.#rb.registryGet(REGISTRY_ROOT);
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

    await fs.registerExchangeKey(PASSKEY_NAMESPACE, publicKey);

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

    await this.#rb.registrySet(REGISTRY_PRIVATE_KEY, keyCid);
    await this.#rb.registrySet(REGISTRY_ROOT, root);

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

      const bytes = /** @type {Uint8Array} */ (
        encodeJsonCollection(data, name, true)
      );

      await fs.write(path, "bytes", bytes);
      await this.#writeLocalCache(name, bytes);
      seeded = true;
    }

    if (!seeded) return;

    const root = (await fs.calculateDataRoot()).toString();
    await blockstore.flush(root);
    blockstore.setRoot(root);

    await rb.registrySet(REGISTRY_ROOT, root);
    storage.root = root;

    await IDB.set(IDB_LOCAL_ROOT, root);
    this.#localFresh = true;
  }

  // SESSION

  /**
   * Mint a fresh `/firkin` delegation for a browser session key.
   *
   * A previously stored session key is reused when it belongs to the same
   * instance + account, but the delegation is always re-minted. This makes an
   * explicit passkey sign-in recover from a delegation that was revoked or
   * expired elsewhere.
   *
   * @param {string} apiBase
   * @param {string} did
   * @param {any} accountSigner
   * @returns {Promise<Session>}
   */
  async #createSession(apiBase, did, accountSigner) {
    /** @type {Session | undefined} */
    const stored = await IDB.get(IDB_SESSION);

    const sessionSigner =
      stored &&
        stored.apiBase === apiBase &&
        stored.did === did &&
        stored.privateKey instanceof CryptoKey
        ? WebCryptoSigner.fromPersisted(stored.privateKey, stored.publicKey)
        : await WebCryptoSigner.generateNonExtractable();

    const { proof, cid } = await delegate(accountSigner, {
      subject: did,
      audience: sessionSigner.did(),
      command: SESSION_COMMAND,
      expireIn: SESSION_TTL_SECONDS,
    });

    /** @type {Session} */
    const session = {
      apiBase,
      did,
      privateKey: /** @type {CryptoKey} */ (sessionSigner.privateKey),
      publicKey: sessionSigner.pubkeyBytes,
      proof,
      proofCid: cid,
      expiresAt: Date.now() + SESSION_TTL_SECONDS * 1000,
    };

    await IDB.set(IDB_SESSION, session);
    return session;
  }

  /** @param {Session} session */
  #clientFor(session) {
    const sessionSigner = WebCryptoSigner.fromPersisted(
      session.privateKey,
      session.publicKey,
    );

    const blockCache = {
      /** @param {string} cid */
      get: (cid) => this.#blockstore?.peek(cid),
      /** @param {string} cid; @param {Uint8Array} bytes */
      set: (cid, bytes) => {
        this.#blockstore?.seed(cid, bytes);
      },
      keys: () => this.#blockstore?.keys() ?? [],
    };

    return new Firkin(
      session.apiBase,
      async (/** @type {string} */ command, options) => {
        const expireIn = options?.expireIn ?? 600;

        // Reference the delegation by CID when it has been published (the
        // SDK's default; smaller tokens). Servers without a delegation store
        // get the bootstrap shape with the proof inline.
        if (session.inlineProofs) {
          return await inlineInvocation(sessionSigner, {
            command,
            proofs: [{ proof: session.proof, cid: session.proofCid }],
            expireIn,
          });
        }

        return await invocation(sessionSigner, {
          command,
          // The bytes are used locally to discover/narrow the chain; only the
          // CID goes on the wire.
          proofs: [{ proof: session.proof, cid: session.proofCid }],
          subject: session.did,
          expireIn,
        });
      },
      undefined,
      /** @type {any} */ (blockCache),
    );
  }

  /**
   * Publish the session delegation to the server's delegation store so tokens
   * can reference it by CID. Idempotent; publishing is skipped once done.
   *
   * Falls back to inline proofs on servers that have no delegation store
   * (the token still works there, it is just larger).
   *
   * @param {Session} session
   */
  async #publishSessionDelegation(session) {
    if (session.published || session.inlineProofs) return;

    const sessionSigner = WebCryptoSigner.fromPersisted(
      session.privateKey,
      session.publicKey,
    );

    // The explicit token below is used instead of this client's token source,
    // so `inlineProofs` here is irrelevant; it avoids minting a reference
    // proof for a delegation that is not published yet.
    const rb = this.#clientFor({ ...session, inlineProofs: true });

    try {
      // Bootstrap token: the delegation is not stored yet, so its proof has to
      // travel inline for this one call.
      const token = await inlineInvocation(sessionSigner, {
        command: "/firkin/blob/upload",
        proofs: [{ proof: session.proof, cid: session.proofCid }],
        expireIn: 60,
      });

      await rb.publishDelegation(
        { proof: session.proof, cid: session.proofCid },
        { token },
      );
      session.published = true;
      session.inlineProofs = false;
    } catch (err) {
      console.warn(
        "Could not publish the Firkin session delegation; falling back to inline proofs:",
        err,
      );
      session.published = false;
      session.inlineProofs = true;
    }

    await IDB.set(IDB_SESSION, session);
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

  /** @param {CollectionName} name */
  #refreshCollection(name) {
    const existing = this.#refreshing.get(name);
    if (existing) return existing;

    const pending = (async () => {
      try {
        await this.#ensureMounted();
        const fs = this.#fs;
        if (!fs) return;

        const path = Path.priv(WNFS_OUTPUT_DIR, name);
        if (!(await fs.exists(path))) return;

        const bytes = await fs.read(path, "bytes");
        await this.#writeLocalCache(name, bytes);
        this.#manager.signals[name].value = decodeJsonCollection(bytes, name);
      } catch (err) {
        console.error(`Failed to refresh Firkin + WNFS file '${name}':`, err);
      } finally {
        this.#refreshing.delete(name);
      }
    })();

    this.#refreshing.set(name, pending);
    return pending;
  }

  // FILES

  /** @returns {import("./files.js").StorageStatus} */
  #filesStatus() {
    return {
      // Report the session (restored from IndexedDB immediately) rather than
      // `#ready`, so a consult during the initial registry restore stays
      // optimistic. Mounting/writing still wait for `#ready`.
      connected: Boolean(this.#session.value),
      did: this.did(),
      root: this.dataRoot(),
    };
  }

  /** @returns {Promise<FileSystem>} */
  async #filesFs() {
    await this.#waitForRestore();

    if (!this.#ready.value) {
      throw new Error("Firkin + WNFS is not connected");
    }

    await this.#ensureMounted();

    const fs = this.#fs;
    if (!fs) throw new Error("Firkin + WNFS could not be mounted");
    return fs;
  }

  /** @returns {Promise<import("./files.js").StorageFile[]>} */
  async #listFiles() {
    const fs = await this.#filesFs();
    const manifest = await this.#readManifest(fs);

    return Object.entries(manifest.files).map(([name, file]) => ({
      name,
      ...file,
    }));
  }

  /**
   * Read the manifest from the WNFS. A missing or corrupt manifest is treated
   * as empty (there is no migration of pre-pointer file layouts).
   *
   * @param {FileSystem} fs
   * @returns {Promise<import("./files.js").FilesManifest>}
   */
  async #readManifest(fs) {
    const path = Path.priv(FILES_MANIFEST);
    if (!(await fs.exists(path))) return { version: 1, files: {} };

    return decodeManifest(await fs.read(path, "bytes"));
  }

  /**
   * @param {FileSystem} fs
   * @param {import("./files.js").FilesManifest} manifest
   * @returns {Promise<string>} The new data root.
   */
  async #writeManifest(fs, manifest) {
    const result = await fs.write(
      Path.priv(FILES_MANIFEST),
      "bytes",
      encodeManifest(manifest),
    );

    return result.dataRoot.toString();
  }

  /** @returns {Promise<{ fs: FileSystem; rb: Firkin }>} */
  async #filesContext() {
    const fs = await this.#filesFs();
    const rb = this.#rb;

    if (!rb) throw new Error("Firkin + WNFS is not connected");
    return { fs, rb };
  }

  /**
   * Fallback byte read for playback/metadata, resolving the pointer's CID
   * directly against Firkin.
   *
   * @param {string} name
   * @returns {Promise<Uint8Array>}
   */
  async #readFile(name) {
    const { fs, rb } = await this.#filesContext();
    const manifest = await this.#readManifest(fs);
    const entry = manifest.files[name];

    if (!entry) {
      throw new Error(`File not found in Firkin + WNFS: ${name}`);
    }

    return await rb.get(entry.cid);
  }

  /**
   * A presigned, range-capable Firkin URL for a file — what media elements and
   * the metadata workers use.
   *
   * @param {string} name
   * @param {number} [expireIn]
   * @returns {Promise<string>}
   */
  async #fileURL(name, expireIn) {
    const { fs, rb } = await this.#filesContext();
    const manifest = await this.#readManifest(fs);
    const entry = manifest.files[name];

    if (!entry) {
      throw new Error(`File not found in Firkin + WNFS: ${name}`);
    }

    return await rb.readUrl(entry.cid, {
      expireIn: expireIn ?? FILE_URL_TTL_SECONDS,
    });
  }

  /**
   * Upload the bytes as a raw Firkin blob, then record the pointer in the WNFS.
   *
   * @param {string} name
   * @param {Uint8Array} bytes
   * @param {string} mime
   * @returns {Promise<{ cid: string }>}
   */
  #uploadFile(name, bytes, mime) {
    const write = async () => {
      const { fs, rb } = await this.#filesContext();
      const manifest = await this.#readManifest(fs);
      const previous = manifest.files[name];

      // Audio bytes stay in Firkin; only this pointer goes into the WNFS.
      const { cid } = await rb.upload(bytes, { codec: 0x55 });

      manifest.files[name] = {
        cid,
        size: bytes.byteLength,
        mime,
        modified: Date.now(),
      };

      const root = await this.#writeManifest(fs, manifest);

      // Drop the replaced blob when nothing else points at it.
      if (
        previous && previous.cid !== cid &&
        !Object.values(manifest.files).some((file) =>
          file.cid === previous.cid
        )
      ) {
        await rb.delete(previous.cid).catch(() => {});
      }

      return { cid, root };
    };

    const written = this.#writeQueue.then(write, write);
    this.#writeQueue = written.then(() => {}, () => {});

    return written.then(async ({ cid, root }) => {
      await this.#enqueueUpload(root);
      return { cid };
    });
  }

  /**
   * Remove a pointer from the WNFS and detach the blob's account claim when no
   * other pointer references it.
   *
   * @param {string} name
   */
  #deleteFile(name) {
    const remove = async () => {
      const { fs, rb } = await this.#filesContext();
      const manifest = await this.#readManifest(fs);
      const entry = manifest.files[name];

      if (!entry) return null;

      delete manifest.files[name];
      const root = await this.#writeManifest(fs, manifest);

      if (
        !Object.values(manifest.files).some((file) => file.cid === entry.cid)
      ) {
        await rb.delete(entry.cid).catch(() => {});
      }

      return root;
    };

    const removed = this.#writeQueue.then(remove, remove);
    this.#writeQueue = removed.then(() => {}, () => {});

    return removed.then(async (root) => {
      if (root) await this.#enqueueUpload(root);
    });
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

    const path = Path.priv(WNFS_OUTPUT_DIR, name);

    try {
      if (!(await fs.exists(path))) return [];

      const bytes = await fs.read(path, "bytes");
      await this.#writeLocalCache(name, bytes);
      return decodeJsonCollection(bytes, name);
    } catch (err) {
      console.error(`Failed to read Firkin + WNFS file '${name}':`, err);
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

      const result = await fs.write(
        Path.priv(WNFS_OUTPUT_DIR, name),
        "bytes",
        bytes,
      );

      // Keep the local fallback current even if the upload fails.
      await this.#writeLocalCache(name, bytes);
      return result.dataRoot.toString();
    };

    // Serialize only the WNFS mutations. The upload is coalesced by the
    // debounced queue, so rapid saves share one `/dag/push`.
    const written = this.#writeQueue.then(write, write);
    this.#writeQueue = written.then(() => {}, () => {});
    return written.then((root) => this.#enqueueUpload(root));
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
      await rb.registrySet(REGISTRY_ROOT, root);
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
