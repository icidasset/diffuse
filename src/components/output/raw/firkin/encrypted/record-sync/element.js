import { ApiError, bytesToHex, WebCryptoSigner } from "@tokono.ma/firkin";
import * as IDB from "idb-keyval";

import { computed, signal } from "~/common/signal.js";
import { BroadcastedOutputElement } from "../../../../common.js";
import { defineElement } from "~/common/element.js";
import { RpcChannel } from "~/common/worker/rpc-channel.js";
import {
  getSessionClient,
  isUnauthorized,
  sessionScope,
} from "~/common/firkin/session.js";
import { writeIfNewer } from "~/common/firkin/storage.js";
import {
  channelName,
  HINT_DEBOUNCE_MS,
  openHintChannel,
} from "~/common/firkin/channel.js";
import {
  invalidate as invalidateListing,
  listingKey,
  names as sharedNames,
} from "~/common/firkin/listings.js";
import { fieldKey } from "~/common/firkin/output-key.js";
import {
  fieldContext,
  openField,
  sealField,
} from "~/common/record-sync/field-cipher.js";

import {
  COLLECTIONS,
  FIRST_READ_TIMEOUT_MS,
  IDB_DOCS,
  IDB_PACKS,
  logName,
  NAMES_TTL_MS,
  NOT_READY_GRACE_MS,
  PUSH_COALESCE_MS,
  registryNamespace,
  SNAPSHOT_EVERY,
} from "./constants.js";
import {
  appendEntry,
  extendLogSignature,
  logSignature,
  logSignatureCid,
  readEntries,
} from "../incremental-loro/log.js";
import { verifySignature } from "../incremental-loro/verify.js";
import {
  applyPatch,
  decodePatch,
  decodeState,
  deltaPatch,
  emptyState,
  encodePatch,
  encodeState,
  hasState,
  snapshotPatch,
  stateItems,
} from "./codec.js";

/**
 * @import { Session } from "~/common/firkin/session.js";
 * @import { Hint } from "~/common/firkin/channel.js";
 * @import { LogAuth } from "../incremental-loro/log.js";
 * @import { Firkin } from "@tokono.ma/firkin";
 * @import { Patch, State } from "./codec.js";
 * @import { CollectionName } from "~/common/self-describing.js";
 */

/**
 * Which fields are sealed before a record reaches the log.
 *
 * Track URIs can embed credentials (Subsonic tokens, signed URLs), playlist
 * entries repeat them, and setting values hold service keys. Everything else
 * stays readable to the server.
 *
 * @type {Record<string, string[]>}
 */
const SENSITIVE_FIELDS = {
  settings: ["value"],
  tracks: ["uri"],
};

/**
 * The newest version timestamp in a state.
 *
 * Used to detect a persisted state that is older than the applied log: a stale
 * tab can write state without winning the monotonic applied-record race, and a
 * reload must then replay the log instead of trusting the signature.
 *
 * @param {State} state
 */
function stateVersion(state) {
  let version = 0;

  for (const record of Object.values(state.records)) {
    const at = Date.parse(record.at);
    if (Number.isFinite(at) && at > version) version = at;
  }

  for (const tombstone of Object.values(state.tombstones)) {
    const at = Date.parse(tombstone.at);
    if (Number.isFinite(at) && at > version) version = at;
  }

  const order = Date.parse(state.order.at);
  if (Number.isFinite(order) && order > version) version = order;

  return version;
}

/** The client's block cache, shared with the log's inline entries. */
const blockCache = {
  /** @param {string} cid */
  get: (cid) => IDB.get(`${IDB_PACKS}/${cid}`),
  /** @param {string} cid; @param {Uint8Array} bytes */
  set: (cid, bytes) => IDB.set(`${IDB_PACKS}/${cid}`, bytes),
  keys: async () =>
    (await IDB.keys())
      .filter((key) =>
        typeof key === "string" && key.startsWith(`${IDB_PACKS}/`)
      )
      .map((key) => /** @type {string} */ (key).slice(IDB_PACKS.length + 1)),
};

/**
 * The userdata output: **Record Sync on Firkin**.
 *
 * A collection is a map of records with last-writer-wins versions, tombstones
 * for deletes and one LWW order. Saves append a patch (or a full snapshot every
 * `SNAPSHOT_EVERY` pushes) to the same signed Firkin log the Loro output uses;
 * pulls replay the log and merge with LWW. The log stores patches in the clear:
 * only the `$enc` fields are sealed, so metadata and public fields are visible
 * to the server. There is no operation history beyond the retained log window
 * and tombstones.
 *
 * Sensitive fields are sealed with the Record Sync field key (`$enc`) before a
 * record is stored locally or remotely, and opened when the collection is read.
 * An unchanged plaintext keeps its ciphertext, so editing another field does
 * not rewrite the record.
 *
 * The spec is `specs/record-sync/SPEC.md`. This is the reference transport: the
 * manifest signature covers the entry CIDs, which stands in for the per-patch
 * signature in the spec.
 *
 * @extends {BroadcastedOutputElement}
 */
class FirkinRecordSyncOutput extends BroadcastedOutputElement {
  static NAME = "diffuse/output/raw/firkin/encrypted/record-sync";
  static WORKER_URL =
    "components/output/raw/firkin/encrypted/record-sync/worker.js";

  // SIGNALS

  #isOnline = signal(navigator.onLine);
  #connected = signal(false);
  #loaded = signal(false);

  /** @type {Map<string, ReturnType<typeof signal<number>>>} */
  #versions = new Map(COLLECTIONS.map((name) => [name, signal(0)]));

  /** @type {ReturnType<typeof signal<Set<string>>>} */
  #ready = signal(new Set());

  // LOCAL STATE

  /** @type {Map<string, State>} */
  #states = new Map();

  /** Plaintext records per collection, decrypted from the sealed state. */
  /** @type {Map<string, any[]>} */
  #views = new Map();

  /** @type {Map<string, { pushes: number }>} */
  #cursors = new Map();

  /** @type {Map<string, { packs: string; signature?: string; at?: number; stateAt?: number }>} */
  #applied = new Map();

  /**
   * The local patch waiting for a push. A pending push loaded from disk has no
   * payload (it is only a flag), so it is tracked separately and sent as a
   * snapshot instead.
   *
   * @type {Map<string, Patch>}
   */
  #pending = new Map();

  /** @type {Set<string>} */
  #pendingSnapshots = new Set();

  #persisted = new Set();

  /** @type {Promise<void> | undefined} */
  #pulling;

  /** @type {Promise<void> | undefined} */
  #initialising;

  #queue = Promise.resolve();
  /** @type {Promise<void>} */
  #pushQueue = Promise.resolve();

  /** @type {Map<string, ReturnType<typeof setTimeout>>} */
  #pushTimers = new Map();

  // TRANSPORT

  /** @type {Map<string, Uint8Array>} */
  #packs = new Map();

  /** @type {Map<string, import("../incremental-loro/log.js").ManifestState>} */
  #manifests = new Map();

  /** @type {ReturnType<typeof openHintChannel> | undefined} */
  #channel;

  /**
   * Pending hints per collection. `writers` keeps the latest manifest each
   * device announced, so one writer with a CID can skip the registry listing.
   *
   * @type {Map<string, { timer: ReturnType<typeof setTimeout>; writers: Map<string, string | undefined> }>}
   */
  #hints = new Map();

  /** Cache scope for the shared registry listing: apiBase + account + namespace. */
  /** @type {string | undefined} */
  #listingKey;

  /** IDB scope for the signed-in server + account. */
  /** @type {string | undefined} */
  #accountKey;

  /** @type {{ publicKey: Uint8Array; auth: LogAuth } | undefined} */
  #authCache;

  /** The session's public key hex, used as the writer id in versions. */
  /** @type {string | undefined} */
  #deviceId;

  // SHARED WORKER

  /** Whether this element drives the SharedWorker engine instead of a local one. */
  #workerMode = signal(false);

  /** @type {RpcChannel<any, any> | undefined} */
  #workerChannel;

  /** @type {Promise<void> | undefined} Resolves once `open` is served. */
  #workerOpening;

  /** @type {Map<string, any[]>} */
  #workerViews = new Map();

  #workerLoaded = signal(new Set());
  #workerStatus = signal(false);
  #workerListening = false;

  /** Last version timestamp issued by this device. */
  #lastAt = 0;

  /** @type {Promise<void> | undefined} */
  #refreshing;

  #refreshAgain = false;

  /** @type {ReturnType<typeof setTimeout> | undefined} */
  #settle;

  constructor() {
    super();

    for (const name of COLLECTIONS) {
      this.#states.set(name, emptyState());
    }

    this.collaborations = this.#entry("collaborations");
    this.facets = this.#entry("facets");
    this.playlistItems = this.#entry("playlistItems");
    this.settings = this.#entry("settings");
    this.tracks = this.#entry("tracks");

    this.ready = computed(() => {
      if (this.#workerMode.value) return this.#workerStatus.value;

      return this.#loaded.value &&
        this.#connected.value &&
        this.#isOnline.value &&
        fieldKey() !== undefined;
    });

    this.effect(() => {
      if (this.#workerMode.value) return;
      if (fieldKey() === undefined) return;

      void this.#refreshConnected();
      void this.#refreshViews();
    });

    this.effect(() => {
      if (this.#workerMode.value) return;

      if (!this.ready()) {
        this.#scheduleSettle();
        return;
      }

      clearTimeout(this.#settle);
      this.#settle = undefined;

      void this.#pushUnpushed();
      void this.#pull();
    });
  }

  // ENTRIES

  /** @param {string} name */
  #entry(name) {
    return {
      collection: computed(() => {
        this.#versions.get(name)?.get();

        if (this.#workerMode.value) {
          if (!this.#workerLoaded.value.has(name)) return { state: "loading" };

          return { state: "loaded", data: this.#workerViews.get(name) ?? [] };
        }

        if (!this.#ready.value.has(name)) return { state: "loading" };

        return { state: "loaded", data: this.#views.get(name) ?? [] };
      }),
      reload: () =>
        this.#workerMode.value
          ? this.#workerReload(name)
          : this.#reloadLocal(name),
      save: (/** @type {any[]} */ items) =>
        this.#workerMode.value
          ? this.#workerSave(name, items)
          : this.#save(name, items),
    };
  }

  /** @param {string} name */
  #reloadLocal(name) {
    return this.#pullCollection(name).catch((err) => {
      this.#remoteFailure(err, `Could not reload the '${name}' records:`);
      throw err;
    });
  }

  // LIFECYCLE

  /** @override */
  connectedCallback() {
    super.connectedCallback();

    globalThis.addEventListener("online", this.#online);
    globalThis.addEventListener("offline", this.#offline);
    globalThis.addEventListener("pagehide", this.#pageHide);
    document.addEventListener("visibilitychange", this.#visibilityChange);
    globalThis.addEventListener("pageshow", this.#pageShow);

    if (
      this.hasAttribute("shared") &&
      this.hasAttribute("group") &&
      typeof SharedWorker !== "undefined"
    ) {
      this.#workerMode.value = true;
      void this.#connectWorker();
      return;
    }

    void this.#initialise();
    void this.#refreshConnected();
  }

  /** @override */
  disconnectedCallback() {
    globalThis.removeEventListener("online", this.#online);
    globalThis.removeEventListener("offline", this.#offline);
    globalThis.removeEventListener("pagehide", this.#pageHide);
    document.removeEventListener("visibilitychange", this.#visibilityChange);
    globalThis.removeEventListener("pageshow", this.#pageShow);

    if (this.#workerMode.value) {
      void this.#workerCall("close");

      // `close` unregisters the port; a re-mounted element has to `open`
      // again (on the same port) to receive broadcasts.
      this.#workerOpening = undefined;

      super.disconnectedCallback();
      return;
    }

    this.#flushPushes();
    this.#closeChannel();

    super.disconnectedCallback();
  }

  #offline = () => {
    this.#isOnline.set(false);

    if (this.#workerMode.value) {
      void this.#workerCall("online", false);
      return;
    }

    this.#closeChannel();
  };

  #online = () => {
    this.#isOnline.set(true);

    if (this.#workerMode.value) {
      void this.#workerCall("online", true);
      return;
    }

    void this.#refreshConnected();
  };

  #visibilityChange = () => {
    if (document.visibilityState !== "visible") {
      if (this.#workerMode.value) {
        void this.#workerCall("hidden");
        return;
      }

      this.#flushPushes();
      return;
    }

    if (this.#workerMode.value) {
      void this.#workerCall("visible");
      return;
    }

    this.#catchUp();
  };

  /** The page is going away: send anything still coalescing. */
  #pageHide = () => {
    if (this.#workerMode.value) {
      void this.#workerCall("hidden");
      return;
    }

    this.#flushPushes();
  };

  /** A page restored from the back/forward cache skipped its normal load. */
  #pageShow = (/** @type {PageTransitionEvent} */ event) => {
    if (!event.persisted) return;

    if (this.#workerMode.value) {
      void this.#workerCall("visible");
      return;
    }

    this.#catchUp(true);
  };

  /**
   * Re-read after the page was away or the hint connection was
   * re-established.
   *
   * The cached listing is reused unless the caller knows hints may have been
   * missed. A long hidden period is covered by the listing TTL, so this only
   * forces when the socket was down (reconnect) or the page skipped its normal
   * load (bfcache).
   *
   * @param {boolean} [force]
   */
  #catchUp(force = false) {
    if (!this.isConnected) return;

    if (!this.ready()) {
      void this.#refreshConnected();
      return;
    }

    if (force) this.#invalidateListings();

    void this.#pull();
  }

  #refreshConnected() {
    if (this.#refreshing) {
      this.#refreshAgain = true;
      return this.#refreshing;
    }

    this.#refreshing = (async () => {
      do {
        this.#refreshAgain = false;
        await this.#checkConnected();
      } while (this.#refreshAgain);
    })().finally(() => {
      this.#refreshing = undefined;

      if (this.isConnected && !this.ready()) this.#scheduleSettle();
    });

    return this.#refreshing;
  }

  async #checkConnected() {
    try {
      await this.#initialise();

      const client = await getSessionClient({ cache: blockCache });
      if (!client) {
        this.#accountKey = undefined;
        this.#invalidateListings();
        this.#listingKey = undefined;
        this.#ready.value = new Set();
        this.#bumpAll();
        this.#connected.value = false;
        this.#closeChannel();
        return;
      }

      if (this.#accountKey !== sessionScope(client.session)) {
        await this.#loadAccount();
      }

      const nextKey = listingKey(
        client.session,
        registryNamespace(this.#namespace),
      );

      if (this.#listingKey !== nextKey) {
        if (this.#listingKey) invalidateListing(this.#listingKey);
        this.#listingKey = nextKey;
      }

      this.#deviceId = bytesToHex(client.session.publicKey);

      // A stored session is enough to start the local-first path. The first
      // real read or write validates it; an auth failure marks the output
      // unready (see #remoteFailure).
      this.#connected.value = true;
      this.#ensureChannel(client);
    } catch {
      this.#connected.value = false;
    }
  }

  /**
   * Record a failed remote operation.
   *
   * Only an authorization failure means the session is no longer accepted;
   * a transient network or server error must not make the output fall back to
   * the local store.
   *
   * @param {unknown} err
   * @param {string} message
   */
  #remoteFailure(err, message) {
    console.warn(message, err);

    if (!isUnauthorized(err)) return;

    this.#invalidateListings();
    this.#connected.value = false;
    this.#closeChannel();
  }

  // HINTS

  /** @param {{ session: Session; rb: Firkin }} client */
  #ensureChannel(client) {
    if (this.#channel) return;

    const name = channelName(this.#namespace);

    this.#channel = openHintChannel({
      device: this.#auth(client.session).device,
      url: async () => {
        const current = await getSessionClient({ cache: blockCache });
        if (!current) return undefined;

        return await current.rb.channelUrl(name);
      },
      onHint: (hint) => this.#refreshCollection(hint),
      onReconnect: () => this.#catchUp(true),
    });
  }

  #closeChannel() {
    this.#channel?.close();
    this.#channel = undefined;

    for (const hint of this.#hints.values()) clearTimeout(hint.timer);
    this.#hints.clear();
  }

  /**
   * Re-read one collection, coalescing a burst of hints into one read.
   *
   * A hint that names a manifest for a single known writer skips the registry
   * listing and reads that manifest directly. More than one writer, a writer
   * we have not seen yet, or a hint from an older sender without a CID falls
   * back to listing.
   *
   * @param {Hint} hint
   */
  #refreshCollection(hint) {
    const collection = hint.collection;
    if (!COLLECTIONS.includes(/** @type {any} */ (collection))) return;

    const pending = this.#hints.get(collection);
    if (pending !== undefined) clearTimeout(pending.timer);

    const writers = pending?.writers ??
      new Map();

    writers.set(hint.device ?? "", hint.manifestCid);

    this.#hints.set(collection, {
      writers,
      timer: setTimeout(() => {
        this.#hints.delete(collection);

        const name = /** @type {CollectionName} */ (collection);
        const announced = [...writers];

        void (async () => {
          if (
            announced.length === 1 && announced[0][0] && announced[0][1]
          ) {
            try {
              if (
                await this.#applyHinted(name, announced[0][0], announced[0][1])
              ) {
                return;
              }
            } catch (err) {
              this.#remoteFailure(
                err,
                `Could not apply the '${collection}' hint:`,
              );
            }
          }

          this.#invalidateListings();

          try {
            await this.#pullCollection(name);
          } catch (err) {
            this.#remoteFailure(
              err,
              `Could not refresh '${collection}' after a hint:`,
            );
          }
        })();
      }, HINT_DEBOUNCE_MS),
    });
  }

  // CONFIGURATION

  /** Which engine this element is currently driving. */
  get mode() {
    return this.#workerMode.value ? "shared-worker" : "local";
  }

  get #namespace() {
    return this.namespace ?? "default";
  }

  /** @returns {number | undefined} */
  get keep() {
    const raw = this.getAttribute("keep");
    if (!raw) return undefined;

    const parsed = Number(raw);

    return Number.isInteger(parsed) && parsed > 0 ? parsed : undefined;
  }

  // LOCAL

  #keyPrefix() {
    return `${IDB_DOCS}/${this.#accountKey ?? "anonymous"}/${this.#namespace}`;
  }

  /** @param {string} name */
  #stateKey(name) {
    return `${this.#keyPrefix()}/${name}/state`;
  }

  /** @param {string} name */
  #cursorKey(name) {
    return `${this.#keyPrefix()}/${name}/cursor`;
  }

  /** @param {string} name */
  #appliedKey(name) {
    return `${this.#keyPrefix()}/${name}/applied`;
  }

  /** @param {string} name */
  #pendingKey(name) {
    return `${this.#keyPrefix()}/${name}/pending`;
  }

  /**
   * Load every collection from the local cache. Runs once per account.
   */
  #initialise() {
    this.#initialising ??= (async () => {
      try {
        const client = await getSessionClient({ cache: blockCache });
        const scope = client ? sessionScope(client.session) : undefined;
        this.#accountKey = scope;

        if (scope) {
          await Promise.all(
            COLLECTIONS.map((name) => this.#loadCollection(name)),
          );
        }
      } finally {
        this.#loaded.value = true;
        this.#bumpAll();
      }
    })();

    return this.#initialising;
  }

  /**
   * Discard one account's in-memory state and load another's.
   *
   * Runs inside the serialization queue so an in-flight apply finishes against
   * the old scope before the maps are reset. `ready` is cleared first so a pull
   * that has not started applying yet gives up instead.
   */
  async #loadAccount() {
    this.#loaded.value = false;
    this.#ready.value = new Set();
    this.#bumpAll();
    this.#closeChannel();

    await this.#serialize(async () => {
      this.#initialising = undefined;
      this.#resetLocal();
      await this.#initialise();
    });
  }

  #resetLocal() {
    for (const name of COLLECTIONS) this.#states.set(name, emptyState());
    this.#views.clear();
    this.#cursors.clear();
    this.#applied.clear();
    this.#pending.clear();
    this.#pendingSnapshots.clear();
    this.#persisted.clear();
    this.#manifests.clear();
    this.#lastAt = 0;
    this.#ready.value = new Set();
    this.#loaded.value = false;

    for (const timer of this.#pushTimers.values()) clearTimeout(timer);
    this.#pushTimers.clear();

    this.#bumpAll();
  }

  /** @param {string} name */
  async #loadCollection(name) {
    try {
      const [stored, cursor, applied, pending] = await Promise.all([
        IDB.get(this.#stateKey(name)),
        IDB.get(this.#cursorKey(name)),
        IDB.get(this.#appliedKey(name)),
        IDB.get(this.#pendingKey(name)),
      ]);

      const state = stored instanceof Uint8Array
        ? decodeState(stored)
        : emptyState();

      this.#states.set(name, state);

      const stateAt = stateVersion(state);

      if (stored instanceof Uint8Array) this.#persisted.add(name);

      if (cursor && typeof cursor === "object") {
        this.#cursors.set(name, {
          pushes: Number.isInteger(cursor.pushes) ? cursor.pushes : 0,
        });
      }

      // A missing state cache means this device has no idea what the log holds;
      // ignore the applied record so the pull re-reads, and drop a pending push
      // rather than sending a snapshot of an empty state over the remote.
      if (stored instanceof Uint8Array) {
        if (applied && typeof applied === "object") {
          const appliedStateAt = typeof applied.stateAt === "number"
            ? applied.stateAt
            : undefined;

          // A stale tab can persist an older state without winning the
          // monotonic applied-record race. Don't let the applied signature
          // claim more than the state has; the pull will replay the log.
          if (appliedStateAt !== undefined && stateAt < appliedStateAt) {
            await IDB.del(this.#appliedKey(name)).catch(() => {});
          } else {
            this.#applied.set(name, {
              packs: typeof applied.packs === "string" ? applied.packs : "",
              signature: typeof applied.signature === "string"
                ? applied.signature
                : undefined,
              at: typeof applied.at === "number" ? applied.at : undefined,
              stateAt: appliedStateAt,
            });
          }
        }

        if (pending) this.#pendingSnapshots.add(name);
      } else {
        await IDB.del(this.#appliedKey(name)).catch(() => {});
        await IDB.del(this.#pendingKey(name)).catch(() => {});
      }

      this.#observeState(state);
      await this.#refreshView(name);

      if (stored instanceof Uint8Array) this.#markReady(name);
    } catch (err) {
      console.warn(`Could not load the local '${name}' records:`, err);
    }
  }

  /**
   * Decrypt the sealed records into the collection the UI reads.
   *
   * @param {string} name
   */
  async #refreshView(name) {
    const state = this.#states.get(name) ?? emptyState();
    const key = fieldKey();
    const fields = SENSITIVE_FIELDS[name] ?? [];
    /** @type {any[]} */
    const items = [];

    for (const sealed of stateItems(state)) {
      /** @type {any} */
      const record = { ...sealed };
      const source = record.$enc;
      const enc = source && typeof source === "object"
        ? /** @type {Record<string, string>} */ ({ ...source })
        : undefined;
      let locked = false;

      if (enc) {
        for (const field of fields) {
          const value = enc[field];
          if (typeof value !== "string") continue;

          if (!key) {
            locked = true;
            continue;
          }

          try {
            record[field] = await openField(value, {
              key,
              context: fieldContext(name, String(record.id), field),
            });
            delete enc[field];
          } catch (err) {
            locked = true;
            console.warn(`Could not decrypt '${field}' on '${name}':`, err);
          }
        }

        if (Object.keys(enc).length > 0) record.$enc = enc;
        else delete record.$enc;
      }

      if (locked) record.locked = true;

      items.push(record);
    }

    this.#views.set(name, items);
  }

  async #refreshViews() {
    for (const name of COLLECTIONS) {
      await this.#refreshView(name);
      this.#bump(name);
    }
  }

  /**
   * Seal the sensitive fields of one record, reusing the previous ciphertext
   * when the plaintext has not changed.
   *
   * @param {string} name
   * @param {any} item
   * @param {any} previous - The currently stored (sealed) record.
   * @param {CryptoKey} key
   */
  async #sealRecord(name, item, previous, key) {
    /** @type {any} */
    const record = { ...item };
    const fields = SENSITIVE_FIELDS[name] ?? [];
    /** @type {Record<string, string>} */
    const enc = { ...(previous?.$enc ?? {}), ...(record.$enc ?? {}) };

    delete record.$enc;

    for (const field of fields) {
      const value = record[field];
      const prior = enc[field];

      if (typeof value === "string") {
        enc[field] = await sealField(value, {
          key,
          context: fieldContext(name, String(item.id), field),
          previous: prior,
        });
        delete record[field];
      } else if (typeof prior === "string") {
        // Locked (no key when it was edited) or not in the incoming record:
        // keep the ciphertext rather than dropping the value.
        enc[field] = prior;
      } else {
        delete enc[field];
      }
    }

    if (Object.keys(enc).length > 0) record.$enc = enc;

    return record;
  }

  /** @param {string} name */
  async #persistState(name) {
    const state = this.#states.get(name);
    if (!state) return;

    try {
      await IDB.set(this.#stateKey(name), encodeState(state));
      this.#persisted.add(name);
    } catch (err) {
      this.#persisted.delete(name);
      console.warn(`Could not cache the '${name}' records:`, err);
    }
  }

  /** @param {string} name; @param {boolean} pending */
  async #persistPending(name, pending) {
    try {
      if (pending) await IDB.set(this.#pendingKey(name), true);
      else await IDB.del(this.#pendingKey(name));
    } catch (err) {
      console.warn(`Could not record the '${name}' pending push:`, err);
    }
  }

  /** @param {string} name */
  async #cursor(name) {
    const cached = this.#cursors.get(name);
    if (cached) return cached;

    const cursor = { pushes: 0 };

    try {
      const stored = await IDB.get(this.#cursorKey(name));
      if (
        stored && typeof stored === "object" && Number.isInteger(stored.pushes)
      ) {
        cursor.pushes = stored.pushes;
      }
    } catch (err) {
      console.warn(`Could not read the '${name}' push cursor:`, err);
    }

    this.#cursors.set(name, cursor);
    return cursor;
  }

  /** @param {string} name; @param {{ pushes: number }} cursor */
  async #setCursor(name, cursor) {
    this.#cursors.set(name, cursor);

    try {
      const stored = await writeIfNewer(
        this.#cursorKey(name),
        cursor,
        (next, current) =>
          !Number.isInteger(current?.pushes) || next.pushes >= current.pushes,
      );

      if (Number.isInteger(stored?.pushes) && stored.pushes > cursor.pushes) {
        this.#cursors.set(name, { pushes: stored.pushes });
      }
    } catch (err) {
      console.warn(`Could not persist the '${name}' push cursor:`, err);
    }
  }

  /** @param {string} name; @param {{ packs: string; signature?: string; at?: number; stateAt?: number }} applied */
  async #setApplied(name, applied) {
    const state = this.#states.get(name) ?? emptyState();

    /** @type {{ packs: string; signature?: string; at: number; stateAt: number }} */
    const record = {
      ...applied,
      at: Math.max(applied.at ?? 0, this.#lastAt),
      stateAt: stateVersion(state),
    };

    this.#applied.set(name, record);

    try {
      await writeIfNewer(
        this.#appliedKey(name),
        record,
        (next, current) =>
          !Number.isFinite(current?.at) || next.at >= current.at,
      );
    } catch (err) {
      console.warn(`Could not persist the '${name}' applied log:`, err);
    }
  }

  /**
   * Whether an applied record describes a state this element actually has.
   *
   * A local save legitimately advances the state beyond the applied log, so the
   * check is one-directional: the state must be at least as new as the applied
   * record. A newer applied record with an older state means a stale tab lost
   * the write race, and the log must be replayed.
   *
   * @param {string} name
   * @param {{ stateAt?: number }} applied
   */
  #appliedTrustworthy(name, applied) {
    const state = this.#states.get(name);
    if (!state) return false;
    if (applied.stateAt === undefined) return true;

    return stateVersion(state) >= applied.stateAt;
  }

  // WRITING

  /**
   * @param {string} name
   * @param {any[]} items
   */
  async #save(name, items) {
    await this.#initialise();

    // A save can arrive immediately after the output is selected, before the
    // first connection check has finished loading the account. Wait for it
    // rather than rejecting work that is about to be writable.
    if (!this.ready()) await this.#refreshConnected();

    return await this.#serialize(async () => {
      const key = fieldKey();
      const device = this.#deviceId;

      if (!this.ready() || !key || !device) {
        throw new Error("Record Sync is not ready");
      }

      const state = this.#states.get(name) ?? emptyState();
      /** @type {Record<string, any>} */
      const sealed = Object.create(null);

      for (const item of items) {
        const id = String(item?.id ?? "");
        if (!id) throw new TypeError("Every record needs an id");

        sealed[id] = await this.#sealRecord(
          name,
          item,
          state.records[id]?.record,
          key,
        );
      }

      const order = items.map((item) => String(item.id));
      const patch = deltaPatch(state, sealed, order, {
        at: this.#nextAt(),
        device,
      });

      if (!patch) {
        this.#views.set(name, items.map((item) => ({ ...item })));
        this.#bump(name);
        return;
      }

      applyPatch(state, patch);
      await this.#persistState(name);

      this.#pendingSnapshots.delete(name);
      this.#pending.set(name, patch);
      await this.#persistPending(name, true);

      this.#views.set(name, items.map((item) => ({ ...item })));
      this.#bump(name);
      this.#schedulePush(name);
    });
  }

  #nextAt() {
    this.#lastAt = Math.max(Date.now(), this.#lastAt + 1);

    return new Date(this.#lastAt).toISOString();
  }

  /** @param {State} state */
  #observeState(state) {
    for (
      const entry of [
        ...Object.values(state.records),
        ...Object.values(state.tombstones),
        state.order,
      ]
    ) {
      const at = Date.parse(entry.at);
      if (Number.isFinite(at)) this.#lastAt = Math.max(this.#lastAt, at);
    }
  }

  /** @param {string} name */
  #schedulePush(name) {
    const pending = this.#pushTimers.get(name);
    if (pending !== undefined) clearTimeout(pending);

    this.#pushTimers.set(
      name,
      setTimeout(() => {
        this.#pushTimers.delete(name);
        this.#enqueuePush(name);
      }, PUSH_COALESCE_MS),
    );
  }

  /** @param {string} name */
  #enqueuePush(name) {
    const run = () => this.#push(name);

    this.#pushQueue = this.#pushQueue.then(run, run).then(
      () => {},
      () => {},
    );
  }

  #flushPushes() {
    for (const [name, timer] of this.#pushTimers) {
      clearTimeout(timer);
      this.#enqueuePush(name);
    }

    this.#pushTimers.clear();
  }

  async #pushUnpushed() {
    await this.#initialise();

    // Let a pull settle first: it reconciles the applied record and may prove
    // that a cached collection is already what the log holds.
    if (this.#pulling) await this.#pulling.catch(() => {});

    for (const name of COLLECTIONS) {
      if (this.#pending.has(name) || this.#pendingSnapshots.has(name)) {
        this.#enqueuePush(name);
        continue;
      }

      try {
        if (await IDB.get(this.#pendingKey(name))) {
          this.#pendingSnapshots.add(name);
          this.#enqueuePush(name);
        }
      } catch (err) {
        console.warn(`Could not read the '${name}' pending push:`, err);
      }
    }
  }

  /** @param {string} name */
  async #push(name) {
    if (!(await this.isLeader())) return;

    const cursor = await this.#cursor(name);
    const pending = this.#pending.get(name);
    const pendingSnapshot = this.#pendingSnapshots.has(name);
    if (pending === undefined && !pendingSnapshot) return;

    if (!this.ready()) return;

    const device = this.#deviceId;
    if (!device) return;

    const client = await getSessionClient({ cache: blockCache });
    if (!client) return;

    const scope = sessionScope(client.session);
    if (scope !== this.#accountKey) return;

    const current = this.#applied.get(name);
    if (current && !this.#appliedTrustworthy(name, current)) return;

    const state = this.#states.get(name) ?? emptyState();
    const snapshot = pendingSnapshot ||
      (cursor.pushes > 0 && cursor.pushes % SNAPSHOT_EVERY === 0);

    // A pending push without a state (an evicted cache) must not be sent as an
    // empty snapshot: it would delete the remote records.
    if (snapshot && !hasState(state)) {
      this.#pending.delete(name);
      this.#pendingSnapshots.delete(name);
      await this.#persistPending(name, false);
      return;
    }

    const version = { at: this.#nextAt(), device };
    const patch = snapshot ? snapshotPatch(state, version) : pending;
    if (!patch) return;

    const bytes = encodePatch(patch);
    const auth = this.#auth(client.session);
    const cacheKey = `${name}\u0000${auth.device}`;

    try {
      const { cid, appended, manifest } = await appendEntry(
        client.rb,
        this.#namespace,
        name,
        bytes,
        {
          auth,
          keep: this.keep,
          current: this.#manifests.get(cacheKey),
          registry: registryNamespace(this.#namespace),
        },
      );

      if (sessionScope(client.session) !== this.#accountKey) return;

      this.#manifests.set(cacheKey, manifest);
      this.#packs.set(cid, bytes);

      try {
        await IDB.set(`${IDB_PACKS}/${cid}`, bytes);
      } catch {
        // The log has it; the local cache is only an optimisation.
      }

      this.#invalidateListings();
      this.#channel?.send(name, manifest.cid);

      await this.#setCursor(name, {
        pushes: cursor.pushes + (appended ? 1 : 0),
      });

      const applied = this.#applied.get(name);
      const packs = applied?.packs ?? "";
      const known = packs.split(";");

      await this.#setApplied(name, {
        packs: appended && !known.includes(cid)
          ? (packs ? `${packs};${cid}` : cid)
          : packs,
        signature: extendLogSignature(
          applied?.signature,
          logName(name, auth.device),
          manifest.cid,
        ),
      });

      if (this.#pending.get(name) === pending) this.#pending.delete(name);
      if (this.#pendingSnapshots.has(name) === pendingSnapshot) {
        this.#pendingSnapshots.delete(name);
      }

      await this.#persistPending(
        name,
        this.#pending.has(name) || this.#pendingSnapshots.has(name),
      );
    } catch (err) {
      this.#remoteFailure(err, `Could not push the '${name}' records:`);
    }
  }

  // READING

  #pull() {
    this.#pulling ??= (async () => {
      await this.#initialise();

      const settle = setTimeout(() => {
        for (const name of COLLECTIONS) this.#markReady(name);
      }, FIRST_READ_TIMEOUT_MS);

      try {
        await Promise.all(COLLECTIONS.map(async (name) => {
          try {
            await this.#pullCollection(name);
          } catch (err) {
            this.#remoteFailure(err, `Could not pull the '${name}' records:`);
            this.#markReady(name);
          }
        }));
      } finally {
        clearTimeout(settle);
        this.#pulling = undefined;
      }
    })();

    return this.#pulling;
  }

  /** @param {string} name */
  async #pullCollection(name) {
    await this.#initialise();

    const settle = setTimeout(
      () => this.#markReady(name),
      FIRST_READ_TIMEOUT_MS,
    );

    try {
      if (!this.ready()) return;

      const log = await this.#readLog(name);

      await this.#serialize(async () => {
        if (!this.ready()) return;

        await this.#applyLocked(name, log);
      });
    } finally {
      clearTimeout(settle);
      this.#markReady(name);
    }
  }

  /** @param {string} name */
  async #readLog(name) {
    const client = await getSessionClient({ cache: blockCache });
    if (!client) {
      return {
        patches: /** @type {Patch[]} */ ([]),
        packs: "",
        signature: undefined,
        unchanged: false,
        complete: false,
      };
    }

    const names = await this.#names(client.rb);
    const signature = logSignature(names, name);
    const stored = this.#applied.get(name);
    const applied = stored && this.#appliedTrustworthy(name, stored)
      ? stored
      : undefined;

    if (applied?.signature === signature && applied.packs !== undefined) {
      return {
        patches: /** @type {Patch[]} */ ([]),
        packs: applied.packs,
        signature,
        unchanged: true,
        complete: true,
      };
    }

    const entries = await this.#entries(client, name, names);
    const packs = entries.map((entry) => entry.cid).join(";");

    const appliedPacks = applied?.packs ? applied.packs.split(";") : [];
    const incremental = appliedPacks.length > 0 &&
      appliedPacks.every((cid, index) => entries[index]?.cid === cid);

    const pending = entries.slice(incremental ? appliedPacks.length : 0);

    /** @type {Patch[]} */
    const patches = [];
    let complete = true;

    for (const entry of pending) {
      const bytes = entry.bytes ?? await this.#readPack(client.rb, entry.cid);
      if (!bytes) {
        complete = false;
        continue;
      }

      try {
        patches.push(decodePatch(bytes));
      } catch (err) {
        console.warn(`Ignoring an unreadable '${name}' entry:`, err);
      }
    }

    return { patches, packs, signature, unchanged: false, complete };
  }

  /**
   * @param {string} name
   * @param {{ patches: Patch[]; packs: string; signature?: string; unchanged: boolean; complete: boolean }} log
   */
  async #applyLocked(name, log) {
    if (log.unchanged) return;

    const state = this.#states.get(name) ?? emptyState();
    let changed = false;

    for (const patch of log.patches) {
      const at = Date.parse(patch.at);
      if (Number.isFinite(at)) this.#lastAt = Math.max(this.#lastAt, at);

      changed = applyPatch(state, patch) || changed;
    }

    if (changed) {
      await this.#refreshView(name);
      await this.#persistState(name);
      this.#bump(name);
    }

    if (log.complete && log.packs.length > 0) {
      await this.#setApplied(name, {
        packs: log.packs,
        signature: log.signature,
      });
    }
  }

  // TRANSPORT HELPERS

  /**
   * Apply one writer's announced manifest without listing the namespace.
   *
   * Returns `false` when the hint can't stand in for a listing (no applied
   * record yet, a writer we have not seen in a listing, the manifest can't be
   * read, or an entry block is missing), so the caller can fall back. A new
   * writer must be introduced by a listing: that is where its ref was
   * authorized. Duplicate and stale hints are handled here: applying a patch
   * twice is a no-op, and an older manifest is not allowed to roll the stored
   * signature (or the manifest cache) back.
   *
   * @param {CollectionName} name
   * @param {string} device
   * @param {string} manifestCid
   * @returns {Promise<boolean>}
   */
  async #applyHinted(name, device, manifestCid) {
    if (!this.ready()) return false;

    const applied = this.#applied.get(name);
    if (!applied?.signature) return false;
    if (!this.#appliedTrustworthy(name, applied)) return false;

    const ref = logName(name, device);
    const currentCid = logSignatureCid(applied.signature, ref);

    // A writer the registry has not introduced yet has to be discovered by a
    // listing first; hints only shortcut writers we already know.
    if (currentCid === undefined) return false;
    if (currentCid === manifestCid) return true;

    const client = await getSessionClient({ cache: blockCache });
    if (!client) return false;

    const key = `${name}\u0000${device}`;
    const before = this.#manifests.get(key);
    const entries = await this.#entries(client, name, { [ref]: manifestCid });
    if (entries.length === 0) return false;

    const manifest = this.#manifests.get(key);
    if (before && manifest && manifest.payload.seq < before.payload.seq) {
      this.#manifests.set(key, before);
      return true;
    }

    /** @type {Patch[]} */
    const patches = [];

    for (const entry of entries) {
      const bytes = entry.bytes ?? await this.#readPack(client.rb, entry.cid);
      if (!bytes) return false;

      try {
        patches.push(decodePatch(bytes));
      } catch (err) {
        console.warn(`Ignoring an unreadable '${name}' entry:`, err);
      }
    }

    // Nothing that decodes as a Record Sync patch means this is the wrong
    // manifest (or the wrong output's log); a listing is the safe answer.
    if (patches.length === 0) return false;

    const handled = await this.#serialize(async () => {
      if (!this.ready()) return false;

      const current = this.#applied.get(name) ?? { packs: "" };
      const seen = new Set(current.packs.split(";").filter(Boolean));

      // A duplicate or older manifest adds nothing; leave the stored ref alone
      // rather than rolling it back.
      if (entries.every((entry) => seen.has(entry.cid))) return true;

      const state = this.#states.get(name) ?? emptyState();
      let changed = false;

      for (const patch of patches) {
        const at = Date.parse(patch.at);
        if (Number.isFinite(at)) this.#lastAt = Math.max(this.#lastAt, at);

        changed = applyPatch(state, patch) || changed;
      }

      if (changed) {
        await this.#refreshView(name);
        await this.#persistState(name);
        this.#bump(name);
      }

      for (const entry of entries) seen.add(entry.cid);

      await this.#setApplied(name, {
        packs: [...seen].join(";"),
        signature: extendLogSignature(current.signature, ref, manifestCid),
      });

      return true;
    });

    if (handled) this.#invalidateListings();

    return handled;
  }

  /**
   * @param {{ session: Session; rb: Firkin }} client
   * @param {string} name
   * @param {Record<string, string> | null} names
   */
  async #entries(client, name, names) {
    return await readEntries(client.rb, this.#namespace, name, {
      auth: this.#auth(client.session),
      loadNames: async () => names,
      onManifest: (device, manifestCid, payload) => {
        this.#manifests.set(`${name}\u0000${device}`, {
          cid: manifestCid,
          payload,
        });
      },
    });
  }

  /**
   * The namespace listing, shared across collections, output instances and
   * tabs. The shared store owns the TTL, in-flight dedupe and invalidation
   * broadcast.
   *
   * @param {Firkin} rb
   */
  #loadNames(rb) {
    return (/** @type {string} */ namespace) => {
      const key = this.#listingKey;
      return key
        ? sharedNames(rb, key, namespace, NAMES_TTL_MS)
        : rb.registryNames(namespace);
    };
  }

  #invalidateListings() {
    const key = this.#listingKey;
    if (key) invalidateListing(key);
  }

  /** @param {Firkin} rb */
  async #names(rb) {
    return await this.#loadNames(rb)(registryNamespace(this.#namespace));
  }

  /**
   * @param {Firkin} rb
   * @param {string} cid
   */
  async #readPack(rb, cid) {
    const cached = this.#packs.get(cid);
    if (cached) return cached;

    try {
      const stored = await IDB.get(`${IDB_PACKS}/${cid}`);
      if (stored instanceof Uint8Array) {
        this.#packs.set(cid, stored);
        return stored;
      }
    } catch {
      // Fall through to the network.
    }

    try {
      const bytes = await rb.get(cid);
      this.#packs.set(cid, bytes);

      try {
        await IDB.set(`${IDB_PACKS}/${cid}`, bytes);
      } catch {
        // Cache only.
      }

      return bytes;
    } catch (err) {
      if (err instanceof ApiError && err.status === 404) return undefined;
      throw err;
    }
  }

  // AUTH

  /**
   * @param {Session} session
   */
  #auth(session) {
    if (this.#authCache?.publicKey === session.publicKey) {
      return this.#authCache.auth;
    }

    const signer = WebCryptoSigner.fromPersisted(
      session.privateKey,
      session.publicKey,
    );

    const auth = {
      device: bytesToHex(session.publicKey),
      sign: (/** @type {Uint8Array} */ bytes) => signer.sign(bytes),
      verify: verifySignature,
    };

    this.#authCache = { publicKey: session.publicKey, auth };

    return auth;
  }

  // SHARED WORKER

  #connectWorker() {
    this.#workerOpening ??= this.#openWorker();

    return this.#workerOpening;
  }

  async #openWorker() {
    try {
      const port = this.workerLink();
      this.#workerChannel ??= new RpcChannel(port);

      if (!this.#workerListening) {
        port.addEventListener(
          "message",
          /** @type {EventListener} */ (this.#workerMessage),
        );
        this.#workerListening = true;
      }

      await this.#workerChannel.callMethod("open", [{
        namespace: this.#namespace,
        keep: this.keep,
      }]);
    } catch (err) {
      console.warn(
        "Could not start the Record Sync shared worker; using the local engine:",
        err,
      );

      this.#workerChannel?.destroy();
      this.#workerChannel = undefined;
      this.#workerListening = false;
      this.#workerMode.value = false;

      void this.#initialise();
      void this.#refreshConnected();
    }
  }

  #workerMessage = (/** @type {MessageEvent} */ event) => {
    const message = event.data;
    if (!message || message.ns !== "record-sync") return;

    if (message.type === "items") {
      const name = String(message.collection);
      if (!COLLECTIONS.includes(/** @type {any} */ (name))) return;

      this.#workerViews.set(
        name,
        Array.isArray(message.items) ? message.items : [],
      );
      this.#workerLoaded.value = new Set([...this.#workerLoaded.value, name]);
      this.#bump(name);
      return;
    }

    if (message.type === "status") {
      this.#workerStatus.value = Boolean(message.ready);
    }
  };

  /**
   * @param {string} name
   * @param {any[]} items
   */
  async #workerSave(name, items) {
    await this.#workerOpening;

    // `open` can fail and fall back to the local engine; hand the save over
    // rather than rejecting work that is about to be writable.
    if (!this.#workerMode.value) return await this.#save(name, items);

    const channel = this.#workerChannel;
    if (!channel) throw new Error("Record Sync is not connected");

    await channel.callMethod("save", [name, items]);

    // The worker applied the patch; mirror it so readers of this element see
    // the save without waiting for the broadcast.
    this.#workerViews.set(name, items.map((item) => ({ ...item })));
    this.#workerLoaded.value = new Set([...this.#workerLoaded.value, name]);
    this.#bump(name);
  }

  /** @param {string} name */
  async #workerReload(name) {
    await this.#workerOpening;

    if (!this.#workerMode.value) return await this.#reloadLocal(name);

    const channel = this.#workerChannel;
    if (!channel) throw new Error("Record Sync is not connected");

    return await channel.callMethod("reload", [name]);
  }

  /**
   * @param {string} method
   * @param {...any} args
   */
  async #workerCall(method, ...args) {
    try {
      await this.#workerChannel?.callMethod(method, args);
    } catch (err) {
      console.warn(`Record Sync worker call '${method}' failed:`, err);
    }
  }

  // QUEUE / READY

  /**
   * @template T
   * @param {() => Promise<T>} fn
   * @returns {Promise<T>}
   */
  #serialize(fn) {
    const run = this.#queue.then(fn, fn);
    this.#queue = run.then(() => {}, () => {});

    return run;
  }

  /** @param {string} name */
  #bump(name) {
    const version = this.#versions.get(name);
    if (version) version.value++;
  }

  #bumpAll() {
    for (const version of this.#versions.values()) version.value++;
  }

  /** @param {string} name */
  #markReady(name) {
    if (this.#ready.value.has(name)) return;

    const next = new Set(this.#ready.value);
    next.add(name);
    this.#ready.value = next;
  }

  #scheduleSettle() {
    if (this.#settle !== undefined) return;

    this.#settle = setTimeout(() => {
      this.#settle = undefined;

      if (this.ready()) return;

      for (const name of COLLECTIONS) this.#markReady(name);
    }, NOT_READY_GRACE_MS);
  }
}

export default FirkinRecordSyncOutput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = FirkinRecordSyncOutput;
export const NAME = "dob-record-sync";

defineElement(NAME, FirkinRecordSyncOutput);
