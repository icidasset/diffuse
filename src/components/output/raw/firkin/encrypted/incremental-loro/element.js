import { ApiError, bytesToHex, WebCryptoSigner } from "@tokono.ma/firkin";
import * as IDB from "idb-keyval";

import { computed, signal } from "~/common/signal.js";
import { BroadcastedOutputElement } from "../../../../common.js";
import { defineElement } from "~/common/element.js";
import {
  bundle,
  bundleId,
  byField,
  byUriScheme,
} from "~/common/bundles.js";
import { collectionSchema } from "~/common/self-describing.js";
import { getSessionClient, isUnauthorized } from "~/common/firkin/session.js";
import {
  HINT_DEBOUNCE_MS,
  channelName,
  openHintChannel,
} from "~/common/firkin/channel.js";
import {
  invalidate as invalidateListing,
  listingKey,
  names as sharedNames,
} from "~/common/firkin/listings.js";
import { outputKey } from "~/common/firkin/output-key.js";

import {
  BULK_SAVE_THRESHOLD,
  COLLECTIONS,
  FIRST_READ_TIMEOUT_MS,
  IDB_ACTOR,
  IDB_DOCS,
  IDB_FLOORS,
  IDB_PACKS,
  NAMES_TTL_MS,
  NOT_CONNECTED,
  NOT_READY_GRACE_MS,
  PUSH_COALESCE_MS,
  SNAPSHOT_EVERY,
  logName,
  registryNamespace,
} from "./constants.js";
import { decryptLog, encryptEntry, frameEntries } from "./crypto.js";
import {
  applyIncremental,
  applyLog,
  changeDocument,
  changeDocumentBatched,
  changesSince,
  countChanges,
  createDocument,
  encodeDocument,
  hasRecords,
  headsAtLeast,
  headsEqual,
  headsOf,
  loadDocument,
  recordsOf,
} from "./doc.js";
import {
  appendEntry,
  extendLogSignature,
  logSignature,
  readEntries,
} from "./log.js";
import { verifySignature } from "./verify.js";

/**
 * @import { SignalReader } from "~/common/signal.d.ts";
 * @import { OutputElement } from "@specs/components/output/types.d.ts";
 * @import { BundleSelector } from "~/common/bundles.d.ts";
 * @import { CollectionName } from "~/common/self-describing.js";
 * @import { Track } from "~/definitions/types.d.ts";
 * @import { Heads } from "./doc.js";
 * @import { Session } from "~/common/firkin/session.js";
 * @import { LogAuth } from "./log.js";
 * @import { Firkin } from "@tokono.ma/firkin";
 * @import { CollectionDocument } from "./doc.js";
 */

/**
 * @typedef {Object} BulkActions
 * @property {(args: {
 *   bytes: Uint8Array;
 *   items: any[];
 *   schema?: string;
 *   versioned?: boolean;
 *   pushHeads?: string[];
 *   snapshot?: boolean;
 * }) => Promise<{
 *   changed: boolean;
 *   deltas?: Uint8Array[];
 *   docBytes?: Uint8Array;
 *   pushBytes?: Uint8Array;
 *   heads?: string[];
 * }>} applyLargeChange
 */

/**
 * A push whose entry bytes were prepared off the main thread. It is only used
 * while the document and cursor still match what the worker saw.
 *
 * @typedef {Object} PreparedPush
 * @property {Uint8Array} bytes
 * @property {string[]} heads
 * @property {string} cursorHeads
 * @property {boolean} snapshot
 */

/** Yield to the event loop between applying the worker's deltas. */
const yieldToMain = () => new Promise((resolve) => setTimeout(resolve, 0));

/**
 * The client's block cache, backed by the same IndexedDB entries the output
 * already keeps. `dagPull` uses it as a Bloom filter: blocks this device has
 * read before are not fetched again.
 */
const blockCache = {
  /** @param {string} cid */
  get: (cid) => IDB.get(`${IDB_PACKS}/${cid}`),
  /** @param {string} cid; @param {Uint8Array} bytes */
  set: (cid, bytes) => IDB.set(`${IDB_PACKS}/${cid}`, bytes),
  keys: async () =>
    (await IDB.keys())
      .filter((key) => typeof key === "string" && key.startsWith(`${IDB_PACKS}/`))
      .map((key) => /** @type {string} */ (key).slice(IDB_PACKS.length + 1)),
};

/** Bundle key for items without a grouping key; keeps them instead of dropping. */
const UNKEYED = "unkeyed";

/**
 * The userdata output: an **encrypted, incremental Loro log on Firkin**, in one
 * component.
 *
 * This is deliberately one element rather than a chain of transformers. The
 * three layers are inseparable in practice — the log protocol decides what can
 * be replayed, the encryption framing decides how entries are split back up,
 * and the document layer decides what is appendable — so keeping them in one
 * place removes the cross-component contracts (an `output-selector` chain, a
 * shared plaintext framing, a separately delivered key) that only ever had one
 * consumer.
 *
 * What it does, in order of the data flow:
 *
 * - **Collections** are Loro documents (see `doc.js`): records live in a map
 *   keyed by `id`, so concurrent edits to different records merge.
 * - **Bundled mode** (`bundled`) keeps playlist items and tracks in one
 *   document per playlist/source, with a small manifest document naming them.
 *   Saves therefore touch only the affected bundle documents.
 * - **Saves** append only the changes since the log last saw them; a whole
 *   snapshot is appended every `SNAPSHOT_EVERY` saves so the log stays
 *   replayable after `keep` trims old entries.
 * - **Entries** are sealed with AES-GCM (see `crypto.js`): one entry per save,
 *   and the decrypted log is length-prefixed per entry so it can be replayed
 *   entry by entry.
 * - **The log** lives on Firkin (see `log.js`): per-device signed manifests
 *   over content-addressed blobs, with rollback floors, hints and trimming.
 * - **Replay** (see `doc.js`) tolerates interleaved writers and trimmed
 *   dependencies.
 *
 * Local data lives in IndexedDB, so the UI is usable offline and the remote is
 * only read to merge in other devices' changes.
 *
 * @extends {BroadcastedOutputElement}
 */
class FirkinLoroOutput extends BroadcastedOutputElement {
  static NAME = "diffuse/output/raw/firkin/encrypted/incremental-loro";
  static WORKER_URL =
    "components/output/raw/firkin/encrypted/incremental-loro/worker.js";

  /** @type {import("~/common/worker.d.ts").ProxiedActions<BulkActions> | undefined} */
  #bulkWorker;

  /**
   * Optional grouping rule for tracks, supplied by the composition layer.
   * Only used in bundled mode; when absent, tracks group by URI scheme.
   *
   * @type {BundleSelector<Track> | undefined}
   */
  tracksBy = undefined;

  /** @type {Map<string, PreparedPush>} */
  #preparedPushes = new Map();

  // SIGNALS

  #isOnline = signal(navigator.onLine);
  #connected = signal(false);

  /** Whether a connection check has finished (it may have found no session). */
  #connectionChecked = false;

  /** @type {Promise<void> | undefined} */
  #refreshing;

  /** Set when a check is requested while one is already in flight. */
  #refreshAgain = false;

  /** Loaded from local storage; collections stay `loading` until then. */
  #loaded = signal(false);

  /**
   * Bumped whenever a collection's document changes so its computed re-runs.
   * Per collection so saving settings doesn't make the UI re-read the tracks.
   *
   * @type {Map<string, import("~/common/signal.d.ts").Signal<number>>}
   */
  #versions = new Map(COLLECTIONS.map((name) => [name, signal(0)]));

  /**
   * Collections we have an answer for: either a local document with records, or
   * a completed read of the log. Until a collection is in here its state stays
   * `loading`, so the UI shows its loading state instead of flashing an empty
   * list while the first read is still in flight.
   *
   * @type {import("~/common/signal.d.ts").Signal<Set<string>>}
   */
  #ready = signal(new Set());

  // DOCUMENTS

  /** @type {Map<string, CollectionDocument>} */
  #docs = new Map();

  /** @type {Map<string, { heads?: Heads; pushes: number }>} */
  #cursors = new Map();

  /** Serialises cursor writes per log, so an older write can't land last. */
  /** @type {Map<string, Promise<void>>} */
  #cursorWrites = new Map();

  /**
   * The last manifest read or written per log and device, so an append can skip
   * the registry lookup and the manifest read and go straight to the
   * compare-and-swap.
   *
   * @type {Map<string, import("./log.js").ManifestState>}
   */
  #manifests = new Map();

  /**
   * Fingerprint of the log last applied per collection, so a reload that yields
   * the same bytes isn't replayed.
   *
   * @type {Map<string, string>}
   */
  #applied = new Map();

  /** Collections whose in-memory document is known to be on disk. */
  #persisted = new Set();

  /** Collections whose last push failed (offline, signed out, …). */
  #dirty = new Set();

  /** @type {Promise<void> | undefined} */
  #pulling;

  /** @type {Promise<void> | undefined} */
  #initialising;

  /**
   * Serializes document mutations. Without this a pull that started before a
   * save would overwrite the document the save just wrote.
   */
  #queue = Promise.resolve();

  /** @type {Promise<void>} */
  #pushQueue = Promise.resolve();

  /** Pending coalesced pushes, one timer per collection. */
  #pushTimers = new Map();

  // TRANSPORT

  /** @type {Map<string, Uint8Array>} */
  #packs = new Map();

  /** @type {Map<string, Promise<unknown>>} */
  #writes = new Map();

  /** @type {ReturnType<typeof openHintChannel> | undefined} */
  #channel;

  /** @type {Map<string, ReturnType<typeof setTimeout>>} */
  #hints = new Map();

  /** Cache scope for the shared registry listing: apiBase + account + namespace. */
  /** @type {string | undefined} */
  #listingKey;

  /** @type {{ publicKey: Uint8Array; auth: LogAuth } | undefined} */
  #authCache;

  /** @type {Map<string, Map<string, number>>} */
  #floorCache = new Map();

  constructor() {
    super();

    for (const name of COLLECTIONS) {
      this.#docs.set(name, createDocument());
    }

    this.collaborations = this.#entry("collaborations");
    this.facets = this.#entry("facets");
    this.playlistItems = this.#entry("playlistItems");
    this.settings = this.#entry("settings");
    this.tracks = this.#entry("tracks");

    // Ready means "usable": with no session or no encryption key the output can
    // report an empty library, and the configurator mirrors whatever a selected
    // output reports into the local store — so an unusable output must not
    // claim to be ready, or it would wipe that mirror.
    this.ready = computed(() =>
      this.#loaded.value &&
      this.#connected.value &&
      this.#isOnline.value &&
      outputKey() !== undefined
    );

    // The session and the output encryption key are stored asynchronously, and
    // sign-in can happen after this element connected (the connect facet adds
    // the option first). Re-check when the key appears, or `ready` stays false
    // until an unrelated online event and the configurator falls back.
    this.effect(() => {
      if (outputKey() === undefined) return;

      void this.#refreshConnected();
    });

    // Pull once the output is usable (signed in, key present, online). Retries
    // whenever that flips back to true after a failure.
    this.effect(() => {
      if (!this.ready()) {
        // Nothing can be read yet. Don't leave the UI spinning if that never
        // changes (no session, no encryption key).
        this.#scheduleSettle();
        return;
      }

      clearTimeout(this.#settle);
      this.#settle = undefined;

      // A save made before the output was usable (e.g. an edit made before the
      // encryption key arrived), a push that failed, or one interrupted by a
      // reload only reached the local cache. Now that it can write, push it.
      void this.#pushUnpushed();

      void this.#pull();
    });
  }

  // ENTRIES

  /**
   * @param {string} name
   */
  #entry(name) {
    return {
      collection: computed(() => {
        this.#versions.get(name)?.get();
        if (!this.#ready.value.has(name)) return { state: "loading" };

        return {
          state: "loaded",
          data: this.#records(name),
        };
      }),
      reload: () => {
        const run = this.#isBundled(name)
          ? this.#pullBundled(name)
          : this.#pullCollection(name);

        return run.catch((err) => {
          this.#remoteFailure(err, `Could not reload the '${name}' document:`);
          throw err;
        });
      },
      save: (/** @type {any[]} */ items) => this.#save(name, items),
    };
  }

  // BUNDLED COLLECTIONS

  /** @param {string} name */
  #isBundled(name) {
    if (!this.hasAttribute("bundled")) return false;

    return name === "playlistItems" || name === "tracks";
  }

  /** @param {string} name */
  #manifestName(name) {
    return `${name}#manifest`;
  }

  /** @param {string} name; @param {string} id */
  #bundleName(name, id) {
    return `${name}#${id}`;
  }

  /**
   * The outward records of a collection. In bundled mode this is the manifest's
   * bundle order, with each bundle's records in their own order. Interleaving
   * between bundles is not preserved (playlist items and source-grouped tracks
   * do not depend on it).
   *
   * @param {string} name
   */
  #records(name) {
    if (!this.#isBundled(name)) {
      return recordsOf(this.#docs.get(name) ?? createDocument());
    }

    const manifestName = this.#manifestName(name);
    const manifest = this.#docs.get(manifestName);
    const ids = manifest
      ? recordsOf(manifest).map((record) => String(record.id))
      : [];

    // Before the first bundled save the old single document is the source of
    // truth. An empty manifest that was never persisted is only a placeholder
    // created while loading; only a manifest with records (or one already on
    // disk from a previous save) means the bundled layout is in use.
    if (
      !manifest ||
      (ids.length === 0 && !this.#persisted.has(manifestName))
    ) {
      const legacy = this.#docs.get(name);
      return legacy ? recordsOf(legacy) : [];
    }

    /** @type {any[]} */
    const items = [];

    for (const id of ids) {
      const doc = this.#docs.get(this.#bundleName(name, id));
      if (doc) items.push(...recordsOf(doc));
    }

    return items;
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

    this.#flushPushes();
    this.#closeChannel();

    super.disconnectedCallback();
  }

  #offline = () => {
    this.#isOnline.set(false);
    this.#closeChannel();
  };

  #online = () => {
    this.#isOnline.set(true);
    void this.#refreshConnected();
  };

  /**
   * A page switch on mobile suspends the page (and usually the hint socket)
   * without unloading it. Anything written while hidden would otherwise wait
   * for the next hint or a reload, so re-read the log when it is shown again.
   */
  #visibilityChange = () => {
    if (document.visibilityState !== "visible") {
      // Don't leave a coalesced push waiting on a timer the browser may never
      // run once the page is in the background.
      this.#flushPushes();
      return;
    }

    this.#catchUp();
  };

  /** The page is going away: send anything still coalescing. */
  #pageHide = () => this.#flushPushes();

  /** A page restored from the back/forward cache skipped its normal load. */
  #pageShow = (/** @type {PageTransitionEvent} */ event) => {
    if (!event.persisted) return;

    this.#catchUp(true);
  };

  /**
   * Re-read the log after the page was away or the connection was
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

    // The session check may be what is stale. A successful one flips `ready`,
    // whose effect pulls.
    if (!this.ready()) {
      void this.#refreshConnected();
      return;
    }

    if (force) this.#invalidateListings();

    void this.#pull();
  }

  #refreshConnected() {
    // A call can arrive while a check is in flight (the key and the session
    // land separately). Queue one more pass instead of dropping it: the
    // in-flight check may have started before the session was stored.
    if (this.#refreshing) {
      this.#refreshAgain = true;
      return this.#refreshing;
    }

    this.#connectionChecked = false;

    this.#refreshing = (async () => {
      do {
        this.#refreshAgain = false;
        await this.#checkConnected();
      } while (this.#refreshAgain);
    })().finally(() => {
      this.#refreshing = undefined;
      this.#connectionChecked = true;

      // The check may have concluded that nothing is usable. The ready effect
      // won't re-run if no signal changed, so settle this state explicitly
      // (after the grace period) instead of leaving the UI loading forever.
      if (this.isConnected && !this.ready()) this.#scheduleSettle();
    });

    return this.#refreshing;
  }

  async #checkConnected() {
    try {
      const client = await getSessionClient({ cache: blockCache });
      if (!client) {
        this.#invalidateListings();
        this.#listingKey = undefined;
        this.#connected.value = false;
        return;
      }

      const nextKey = listingKey(
        client.session,
        registryNamespace(this.#namespace),
      );

      if (this.#listingKey !== nextKey) {
        if (this.#listingKey) invalidateListing(this.#listingKey);
        this.#listingKey = nextKey;
      }

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

  /**
   * Subscribe to change hints from other devices.
   *
   * Other devices' writes are only visible after a read, so a hint just says
   * which collection to re-read; it never carries data. Without a channel the
   * output still works, it is only less immediate.
   *
   * @param {{ session: Session; rb: Firkin }} client
   */
  #ensureChannel(client) {
    if (this.#channel) return;

    const name = channelName(this.#namespace);
    const device = this.#auth(client.session).device;

    this.#channel = openHintChannel({
      device,
      url: async () => {
        const current = await getSessionClient({ cache: blockCache });
        if (!current) return undefined;

        return await current.rb.channelUrl(name);
      },
      onHint: (hint) => this.#refreshCollection(hint.collection),
      onReconnect: () => this.#catchUp(true),
    });
  }

  #closeChannel() {
    this.#channel?.close();
    this.#channel = undefined;

    for (const timer of this.#hints.values()) clearTimeout(timer);
    this.#hints.clear();
  }

  /**
   * Re-read one collection, coalescing a burst of hints into one read.
   *
   * A bundled collection writes one log per bundle and one for its manifest,
   * so its hints arrive under names like `tracks#<id>`; refresh the collection
   * that owns them.
   *
   * @param {string} hinted
   */
  #refreshCollection(hinted) {
    const collection = hinted.split("#")[0];
    if (!COLLECTIONS.includes(/** @type {any} */ (collection))) return;

    const pending = this.#hints.get(collection);
    if (pending !== undefined) clearTimeout(pending);

    this.#hints.set(
      collection,
      setTimeout(() => {
        this.#hints.delete(collection);

        // Another device wrote something: don't trust the cached listing.
        this.#invalidateListings();

        const name = /** @type {CollectionName} */ (collection);

        void (this.#isBundled(name)
          ? this.#pullBundled(name)
          : this.#pullCollection(name))
          .catch((err) => {
            this.#remoteFailure(
              err,
              `Could not refresh '${collection}' after a hint:`,
            );
          });
      }, HINT_DEBOUNCE_MS),
    );
  }

  // CONFIGURATION

  get #namespace() {
    return this.namespace ?? "default";
  }

  /**
   * How many entries to retain per log. Unset (or invalid) keeps everything.
   *
   * @returns {number | undefined}
   */
  get keep() {
    const raw = this.getAttribute("keep");
    if (!raw) return undefined;

    const parsed = Number(raw);

    return Number.isInteger(parsed) && parsed > 0 ? parsed : undefined;
  }

  // LOCAL

  /** @param {string} name */
  #docKey(name) {
    return `${IDB_DOCS}/${this.#namespace}/${name}`;
  }

  /**
   * Persist a collection's document.
   *
   * The whole document is written every time, as a Loro snapshot.
   *
   * @param {string} name
   * @param {CollectionDocument} doc
   * @param {{ bytes?: Uint8Array }} [options] - `bytes` supplies the already
   *   encoded snapshot (prepared by the worker).
   */
  async #persist(name, doc, options = {}) {
    await IDB.set(
      this.#docKey(name),
      options.bytes ?? encodeDocument(doc),
    );
  }

  /**
   * Load one document from the local cache.
   *
   * A stored value that isn't a Loro document is ignored: the collection starts
   * empty and the next save overwrites the cache.
   *
   * @param {string} name
   * @returns {Promise<boolean>} Whether the local document is an answer.
   */
  async #loadDoc(name) {
    try {
      const stored = await IDB.get(this.#docKey(name));
      const doc = stored instanceof Uint8Array ? loadDocument(stored) : undefined;

      if (doc) {
        this.#docs.set(name, doc);
        this.#persisted.add(name);

        // Local-first: a collection we have synced before can be shown
        // straight away — even when it is empty, because then "empty" is the
        // truth rather than "unknown".
        const cursor = await this.#cursor(name);
        await this.#reconcileCursor(name, doc);
        return hasRecords(doc) || cursor.heads !== undefined;
      }

      // Nothing cached: start from an empty document of our own, under this
      // document's actor so concurrent writers never share a sequence number.
      //
      // Cache it and keep the push cursor and the applied log: those describe
      // the remote log, not the local cache. Resetting them made every load
      // treat a settled collection as a fresh one, re-bootstrap an empty entry
      // and pull it back the next time.
      const empty = createDocument(await this.#actor(name));

      this.#docs.set(name, empty);

      try {
        await this.#persist(name, empty);
        this.#persisted.add(name);
      } catch (err) {
        this.#persisted.delete(name);
        console.warn(`Could not cache the empty '${name}' document:`, err);
      }

      await this.#reconcileCursor(name, empty);

      return false;
    } catch (err) {
      console.warn(`Could not read the local '${name}' document:`, err);
      return false;
    }
  }

  /**
   * Make the push cursor describe the document that is actually here.
   *
   * The cursor is written after the document on every path, so a cursor that
   * covers operations the document does not have means the cached document was
   * lost or replaced (eviction, a failed write, a restored older cache). The
   * cursor would then look "ahead": the next pull re-reads the log and restores
   * the document, but until then every load thinks it has unpushed changes and
   * appends Loro's empty update (a 22-byte entry) for nothing.
   *
   * Moving the cursor back is safe: the document is the source of truth for
   * what this device has, and the pull re-delivers whatever it lost.
   *
   * @param {string} name
   * @param {CollectionDocument} doc
   */
  async #reconcileCursor(name, doc) {
    const cursor = await this.#cursor(name);
    if (!cursor.heads) return;

    const heads = headsOf(doc);
    if (headsAtLeast(heads, cursor.heads)) return;

    console.warn(
      `The cached '${name}' document is behind its push cursor; re-reading it from the log.`,
    );

    await this.#setCursor(name, { heads, pushes: cursor.pushes }, true);
  }

  /**
   * Load the cached documents. Runs once; subsequent calls await the same work.
   */
  #initialise() {
    this.#initialising ??= (async () => {
      try {
        await Promise.all(COLLECTIONS.map(async (name) => {
          if (!this.#isBundled(name)) {
            if (await this.#loadDoc(name)) this.#markReady(name);
            return;
          }

          const legacy = await this.#loadDoc(name);
          const manifest = await this.#loadDoc(this.#manifestName(name));
          const manifestDoc = this.#docs.get(this.#manifestName(name));

          let bundles = false;

          if (manifestDoc) {
            for (const record of recordsOf(manifestDoc)) {
              const id = String(record.id);
              const answer = await this.#loadDoc(this.#bundleName(name, id));
              bundles = bundles || answer;
            }
          }

          if (legacy || manifest || bundles) this.#markReady(name);
        }));
      } finally {
        // Whatever happened above, the interface must stop loading.
        this.#loaded.value = true;
        this.#bumpAll();
      }
    })();

    return this.#initialising;
  }

  /**
   * @param {string} name
   * @param {any[]} items
   */
  async #save(name, items) {
    await this.#initialise();

    if (!this.#isBundled(name)) return this.#saveDoc(name, items);

    return this.#saveBundled(name, items);
  }

  /** @param {string} name */
  #selectorFor(name) {
    if (name === "playlistItems") return byField("playlist");
    if (name === "tracks") return this.tracksBy ?? byUriScheme();
    return undefined;
  }

  /**
   * Save a bundled collection as one document per bundle plus a manifest.
   *
   * @param {string} name
   * @param {any[]} items
   */
  async #saveBundled(name, items) {
    const selector = this.#selectorFor(name);
    if (!selector) return this.#saveDoc(name, items);

    /** @type {BundleSelector<any>} */
    const by = (item) => selector(item) ?? UNKEYED;
    const bundles = bundle(items, { by, id: bundleId });

    const manifestName = this.#manifestName(name);
    const manifest = this.#docs.get(manifestName);
    const previous = manifest
      ? recordsOf(manifest).map((record) => String(record.id))
      : [];
    const next = new Set(bundles.map((entry) => entry.id));

    // Write the bundle documents first, then the manifest, so the manifest
    // never points at a bundle whose data has not been applied yet.
    for (const entry of bundles) {
      await this.#saveDoc(this.#bundleName(name, entry.id), entry.items);
    }

    for (const id of previous) {
      if (next.has(id)) continue;
      await this.#saveDoc(this.#bundleName(name, id), []);
    }

    await this.#saveDoc(
      manifestName,
      bundles.map((entry) => ({ id: entry.id })),
    );

    // Migrate the old single-document log: its records are now represented by
    // the manifest and bundles. Only when it actually has records — an empty
    // legacy log would otherwise get an empty entry on every device's first
    // save.
    const legacy = this.#docs.get(name);
    if (legacy && hasRecords(legacy)) {
      await this.#saveDoc(name, []);
    }
  }

  /**
   * @param {string} name
   * @param {any[]} items
   */
  async #saveDoc(name, items) {
    return await this.#serialize(async () => {
      const current = this.#docs.get(name) ?? createDocument();
      const schema = collectionSchema(/** @type {CollectionName} */ (name));
      // Track edits reliably bump `updatedAt`, so tracks can use the version
      // fast path when deciding what changed. Other collections cannot.
      const versioned = name === "tracks" || name.startsWith("tracks#");
      // Choose the batched worker path by how much actually changed, not by
      // the collection size: an import patch passes the whole collection even
      // when only a handful of records are new.
      const counts = countChanges(current, items, versioned);
      const bulk =
        counts.changed + counts.removed >= BULK_SAVE_THRESHOLD;

      let doc;
      let changed;
      /** @type {Uint8Array | undefined} */
      let docBytes;
      /** @type {PreparedPush | undefined} */
      let prepared;

      if (bulk) {
        try {
          const cursor = await this.#cursor(name);
          const snapshot = cursor.pushes > 0 &&
            cursor.pushes % SNAPSHOT_EVERY === 0;

          const result = await this.#applyLargeChange(
            current,
            items,
            schema,
            cursor.heads,
            snapshot,
            versioned,
          );

          if (!result.changed) return;

          doc = result.doc;
          changed = true;
          docBytes = result.docBytes;
          prepared = result.pushBytes && result.heads
            ? {
              bytes: result.pushBytes,
              heads: result.heads,
              cursorHeads: (cursor.heads ?? []).join(";"),
              snapshot,
            }
            : undefined;
        } catch (err) {
          console.warn(
            "Could not apply the save in a worker; falling back:",
            err,
          );

          ({ doc, changed } = await changeDocumentBatched(
            current,
            items,
            { schema, versioned },
          ));
        }
      } else {
        ({ doc, changed } = changeDocument(current, items, {
          schema,
          versioned,
        }));
      }

      if (!changed) return;

      this.#docs.set(name, doc);
      this.#bump(name);

      this.#persisted.delete(name);

      try {
        // The worker already encoded the document; reuse those bytes instead of
        // serialising it again here.
        await this.#persist(name, doc, { bytes: docBytes });
        this.#persisted.add(name);
      } catch (err) {
        console.warn(`Could not cache the '${name}' document locally:`, err);
      }

      this.#schedulePush(name, prepared);
    });
  }

  #bulkProxy() {
    this.#bulkWorker ??= this.workerProxy();
    return this.#bulkWorker;
  }

  /**
   * Apply a large save in the worker, which returns the new document's update.
   * The worker also prepares the push entry and the compact base bytes.
   *
   * @param {CollectionDocument} current
   * @param {any[]} items
   * @param {string} schema
   * @param {Heads | undefined} pushHeads
   * @param {boolean} snapshot
   * @param {boolean} versioned
   * @returns {Promise<{
   *   doc: CollectionDocument;
   *   changed: boolean;
   *   docBytes?: Uint8Array;
   *   pushBytes?: Uint8Array;
   *   heads?: string[];
   * }>}
   */
  async #applyLargeChange(current, items, schema, pushHeads, snapshot, versioned) {
    const result = await this.#bulkProxy().applyLargeChange({
      bytes: encodeDocument(current),
      items,
      schema,
      versioned,
      pushHeads,
      snapshot,
    });

    if (!result.changed) return { doc: current, changed: false };

    let doc = current;

    for (const delta of result.deltas ?? []) {
      doc = applyIncremental(doc, delta);
      await yieldToMain();
    }

    return {
      doc,
      changed: true,
      docBytes: result.docBytes,
      pushBytes: result.pushBytes,
      heads: result.heads,
    };
  }

  /**
   * Queue a push, off the document queue: a slow or dead server must not block
   * reads, saves or other collections.
   *
   * @param {string} name
   * @param {CollectionDocument} doc
   * @param {PreparedPush} [prepared]
   */
  #enqueuePush(name, doc, prepared) {
    const run = () => this.#push(name, doc, prepared);

    this.#pushQueue = this.#pushQueue.then(run, run).then(
      () => {},
      () => {},
    );
  }

  /**
   * Coalesce a save's push. A burst of saves (a batch import, a metadata pass)
   * becomes one append of the latest document instead of one manifest rewrite
   * per save. Appends triggered by a pull stay immediate.
   *
   * @param {string} name
   * @param {PreparedPush} [prepared]
   */
  #schedulePush(name, prepared) {
    if (prepared) this.#preparedPushes.set(name, prepared);

    const pending = this.#pushTimers.get(name);
    if (pending !== undefined) clearTimeout(pending);

    this.#pushTimers.set(
      name,
      setTimeout(() => {
        this.#pushTimers.delete(name);

        const doc = this.#docs.get(name);
        const prepared = this.#preparedPushes.get(name);
        this.#preparedPushes.delete(name);

        if (doc) this.#enqueuePush(name, doc, prepared);
      }, PUSH_COALESCE_MS),
    );
  }

  /** Send every coalesced push now (before the page goes away). */
  #flushPushes() {
    for (const [name, timer] of this.#pushTimers) {
      clearTimeout(timer);

      const doc = this.#docs.get(name);
      const prepared = this.#preparedPushes.get(name);

      if (doc) this.#enqueuePush(name, doc, prepared);
    }

    this.#pushTimers.clear();
    this.#preparedPushes.clear();
  }

  /** @type {ReturnType<typeof setTimeout> | undefined} */
  #settle;

  /**
   * Give up waiting for an output that can't read, so the UI shows an empty
   * library instead of an endless loader.
   */
  #scheduleSettle() {
    if (this.#settle !== undefined) return;

    this.#settle = setTimeout(() => {
      this.#settle = undefined;

      if (this.ready()) return;

      // `ready` is false for different reasons, and "sign in" is no help to a
      // user who already is: name the part that is actually missing.
      /** @type {string} */
      let state;

      if (!this.#isOnline.value) {
        state = "offline — it reads again once the connection is back";
      } else if (!this.#connected.value) {
        // A check may simply still be in flight (a large local cache is loaded
        // in parallel); don't report the user as signed out before it answers.
        if (!this.#connectionChecked) return;

        state = "not signed in on this browser; sign in on Connect → Firkin";
      } else if (outputKey() === undefined) {
        state = "signed in, but this browser has no output encryption key " +
          "(it is derived from your passkey at sign-in) — sign in again on " +
          "this device to derive it";
      } else {
        // Everything is present: a large local cache is simply still being
        // read. It resolves on its own; don't claim the key is missing.
        return;
      }

      console.warn(
        `Firkin output: ${state}. Showing an empty library until then.`,
      );

      for (const name of COLLECTIONS) this.#markReady(name);
    }, NOT_READY_GRACE_MS);
  }

  /** @param {string} name */
  #markReady(name) {
    const base = name.split("#", 1)[0];
    const target = this.#versions.has(base) ? base : name;

    if (this.#ready.value.has(target)) return;

    const next = new Set(this.#ready.value);
    next.add(target);
    this.#ready.value = next;
  }

  /** @param {string} name */
  #bump(name) {
    const base = name.split("#", 1)[0];
    const version = this.#versions.get(base) ?? this.#versions.get(name);
    if (version) version.value++;
  }

  #bumpAll() {
    for (const version of this.#versions.values()) version.value++;
  }

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

  /**
   * The log applied to the local document: its ordered pack CIDs and the heads
   * those packs produced. Written only after a complete read, so a missing pack
   * or manifest is retried instead of being mistaken for "nothing changed".
   *
   * @param {string} name
   */
  #appliedLogKey(name) {
    return `${IDB_DOCS}/${this.#namespace}/${name}/applied`;
  }

  /**
   * @param {string} name
   * @returns {Promise<{ packs: string; heads: string; signature?: string } | undefined>}
   */
  async #appliedLog(name) {
    try {
      const stored = await IDB.get(this.#appliedLogKey(name));

      if (
        stored && typeof stored === "object" &&
        typeof stored.packs === "string" &&
        typeof stored.heads === "string"
      ) {
        return {
          packs: stored.packs,
          heads: stored.heads,
          signature: typeof stored.signature === "string"
            ? stored.signature
            : undefined,
        };
      }
    } catch (err) {
      console.warn(`Could not read the '${name}' applied log:`, err);
    }

    return undefined;
  }

  /**
   * @param {string} name
   * @param {string} packs - `;`-joined ordered pack CIDs.
   * @param {string} heads - The document heads those packs produced.
   */
  /**
   * @param {string} name
   * @param {string} packs
   * @param {string} heads
   * @param {string} [signature]
   */
  async #setAppliedLog(name, packs, heads, signature) {
    try {
      await IDB.set(this.#appliedLogKey(name), { packs, heads, signature });
    } catch (err) {
      console.warn(`Could not persist the '${name}' applied log:`, err);
    }
  }

  // REMOTE

  /** @param {string} name */
  #cursorKey(name) {
    return `${IDB_DOCS}/${this.#namespace}/${name}/cursor`;
  }

  /**
   * How far this device has pushed a collection: the heads the log already has,
   * and how many pushes happened since the document was started.
   *
   * @param {string} name
   * @returns {Promise<{ heads?: Heads; pushes: number }>}
   */
  async #cursor(name) {
    const cached = this.#cursors.get(name);
    if (cached) return cached;

    /** @type {{ heads?: Heads; pushes: number }} */
    const cursor = { pushes: 0 };

    try {
      const stored = await IDB.get(this.#cursorKey(name));
      if (stored && typeof stored === "object") {
        if (Array.isArray(stored.heads)) cursor.heads = stored.heads;
        if (Number.isInteger(stored.pushes)) cursor.pushes = stored.pushes;
      }
    } catch (err) {
      console.warn(`Could not read the '${name}' push cursor:`, err);
    }

    this.#cursors.set(name, cursor);

    return cursor;
  }

  /**
   * @param {string} name
   * @param {{ heads?: Heads; pushes: number }} cursor
   * @param {boolean} [force] - Allow the cursor to move backwards (reconcile).
   */
  async #setCursor(name, cursor, force = false) {
    const existing = this.#cursors.get(name);

    // Only ever move forwards. A push and a pull can finish in the opposite
    // order from the one they started in; a cursor that moves backwards makes
    // the next pull think the local document has unpushed changes, which it
    // appends — and the other device does the same — so the two devices keep
    // updating each other.
    if (
      !force && existing?.heads && cursor.heads &&
      !headsAtLeast(cursor.heads, existing.heads)
    ) {
      return existing;
    }

    this.#cursors.set(name, cursor);

    // Persist the cursor as it is when the write runs, and in call order.
    const write = (this.#cursorWrites.get(name) ?? Promise.resolve()).then(
      () => IDB.set(this.#cursorKey(name), this.#cursors.get(name) ?? cursor),
    );
    this.#cursorWrites.set(name, write.catch(() => {}));

    try {
      await write;
    } catch (err) {
      console.warn(`Could not persist the '${name}' push cursor:`, err);
    }

    return cursor;
  }

  /** @type {Map<string, Promise<string>>} */
  #actorPromises = new Map();

  /**
   * A stable writer id per document, persisted so reloads keep writing under
   * the same identity. Loro uses it as the document's peer id.
   *
   * @param {string} name
   * @returns {Promise<string>}
   */
  #actor(name) {
    const existing = this.#actorPromises.get(name);
    if (existing) return existing;

    const created = (async () => {
      const key = `${IDB_ACTOR}/${this.#namespace}/${name}`;

      try {
        const stored = await IDB.get(key);
        if (typeof stored === "string" && /^[0-9a-f]{32}$/.test(stored)) {
          return stored;
        }
      } catch {
        // Fall through to a new actor.
      }

      const actor = crypto.randomUUID().replace(/-/g, "");

      try {
        await IDB.set(key, actor);
      } catch (err) {
        console.warn(`Could not persist the '${name}' actor:`, err);
      }

      return actor;
    })();

    this.#actorPromises.set(name, created);
    return created;
  }

  /**
   * Whether the local document has changes the log hasn't acknowledged.
   *
   * The cursor holds the heads of the last successful push (and the heads a
   * completed read proved the log has), so anything newer is local-only: a
   * save whose push failed, or one interrupted before its queued push ran.
   *
   * @param {string} name
   * @param {CollectionDocument} doc
   */
  async #hasUnpushed(name, doc) {
    // A document with no operations has nothing of its own to publish, however
    // its cursor looks (a fresh device has none at all).
    if (doc.opCount() === 0) return false;

    const heads = headsOf(doc);
    const cursor = await this.#cursor(name);

    // Compare versions semantically: Loro encodes the same version differently
    // depending on the order peers were added, so string equality is not enough.
    if (cursor.heads) {
      // Cursor ahead: the cached document was lost, and the next pull restores
      // it from the log. There is nothing here to publish.
      if (!headsAtLeast(heads, cursor.heads)) return false;

      // Same version: whatever the encoding says, nothing changed.
      if (headsAtLeast(cursor.heads, heads)) return false;
    }

    // The applied record says what the log already contains. A document that is
    // exactly that has nothing of its own to push, even when its cursor was
    // lost (a restored cache, a cleared IndexedDB, a race).
    const applied = await this.#appliedLog(name);

    return !applied?.heads ||
      !headsEqual(applied ? [applied.heads] : [], heads);
  }

  /**
   * Every document that belongs to a collection. In bundled mode this is the
   * manifest, the bundle documents currently in memory, and the old single
   * document while it is being migrated.
   *
   * @param {string} name
   */
  #docNames(name) {
    if (!this.#isBundled(name)) return [name];

    /** @type {string[]} */
    const names = [];

    for (const key of this.#docs.keys()) {
      if (key === name || key.startsWith(`${name}#`)) names.push(key);
    }

    return names;
  }

  /**
   * Push every collection whose local document is ahead of the log.
   *
   * Saves are written locally first and pushed from memory, so closing the page
   * (or a tab that navigates away) before the queued push runs would otherwise
   * leave the change local-only until the next save of that collection. Checking
   * the persisted cursor against the loaded document on startup is what makes
   * an interrupted upload resume.
   */
  async #pushUnpushed() {
    await this.#initialise();

    // Let an in-flight pull settle first: it records what the log already holds
    // (the cursor and the applied record), and without that a document that is
    // merely cached looks unpushed and gets re-sent on every load.
    if (this.#pulling) await this.#pulling.catch(() => {});

    for (const name of COLLECTIONS) {
      for (const docName of this.#docNames(name)) {
        const doc = this.#docs.get(docName);
        if (!doc) continue;

        if (!(await this.#hasUnpushed(docName, doc))) continue;

        this.#enqueuePush(docName, doc);
      }
    }
  }

  /**
   * Push one document to the log.
   *
   * The first push of a session is a delta when we know what the log has (see
   * the cursor advance after a pull); otherwise `changesSince` returns a whole
   * document anyway.
   *
   * @param {string} name
   * @param {CollectionDocument} doc
   * @param {PreparedPush} [prepared]
   */
  async #push(name, doc, prepared) {
    // Only the group leader writes; other tabs read their changes back from the
    // log like any other device.
    if (!(await this.isLeader())) return;

    // Not writable yet: offline, no session, or the output key is not loaded on
    // this browser. The save is already in the local cache, and marking the
    // collection dirty is what makes the ready effect push it later — trying
    // the write here would only log an error for work that is safely stored.
    if (!this.ready()) {
      this.#dirty.add(name);
      return;
    }

    const cursor = await this.#cursor(name);
    const heads = headsOf(doc);

    // Export only when the document is strictly ahead of the cursor.
    // - The cursor can be ahead when the cached document lost operations (it
    //   was evicted or replaced): nothing to export, and Loro would answer with
    //   a small empty update.
    // - The versions can be equal while their encodings differ (Loro encodes in
    //   peer-insertion order): nothing changed either.
    // Appending the empty update would add an empty log entry on every load.
    if (cursor.heads) {
      if (!headsAtLeast(heads, cursor.heads)) return;
      if (headsAtLeast(cursor.heads, heads)) return;
    }

    const snapshot = cursor.pushes > 0 && cursor.pushes % SNAPSHOT_EVERY === 0;

    // Use the worker's precomputed entry only while the document and cursor
    // still match what it saw; otherwise compute it here.
    /** @type {Uint8Array} */
    let bytes;

    if (
      prepared &&
      prepared.snapshot === snapshot &&
      prepared.cursorHeads === (cursor.heads ?? []).join(";") &&
      prepared.heads.join(";") === heads.join(";")
    ) {
      bytes = prepared.bytes;
    } else {
      bytes = snapshot ? encodeDocument(doc) : changesSince(doc, cursor.heads);
    }

    if (bytes.length === 0) return;

    /** @type {{ device: string; manifest: import("./log.js").ManifestState; cid: string; appended: boolean } | undefined} */
    let appended;

    try {
      appended = await this.#append(name, bytes);
    } catch (err) {
      this.#dirty.add(name);
      this.#remoteFailure(err, `Could not push the '${name}' changes:`);
      return;
    }

    this.#dirty.delete(name);

    // The cursor first: the applied record below is only meaningful once the
    // push cursor agrees, and a loader may observe the record as soon as it is
    // written. A duplicate append (the same plaintext already in the log) did
    // not add anything, so it must not advance the snapshot cadence either.
    await this.#setCursor(name, {
      heads,
      pushes: cursor.pushes + (appended?.appended ? 1 : 0),
    });

    // This device's log is now exactly what its document holds, so record the
    // push as applied. Without this, the next page load would see a manifest it
    // hasn't recorded and pull its own write back.
    if (appended?.manifest) {
      const applied = await this.#appliedLog(name);
      const packs = applied?.packs ?? "";
      const known = packs.split(";");

      // The entry this push appended belongs to the applied log too; the rest
      // is whatever the last pull recorded.
      const nextPacks = appended.appended && !known.includes(appended.cid)
        ? (packs ? `${packs};${appended.cid}` : appended.cid)
        : packs;

      await this.#setAppliedLog(
        name,
        nextPacks,
        heads.join(";"),
        extendLogSignature(
          applied?.signature,
          logName(name, appended.device),
          appended.manifest.cid,
        ),
      );
    }
  }

  /**
   * Append one plaintext entry to the log, sealing it first.
   *
   * @param {string} name
   * @param {Uint8Array} entry
   * @returns {Promise<{ device: string; manifest: import("./log.js").ManifestState; cid: string; appended: boolean } | undefined>}
   */
  #append(name, entry) {
    const run = async () => {
      const client = await getSessionClient({ cache: blockCache });
      if (!client) throw new Error(NOT_CONNECTED);

      const key = outputKey();
      if (!key) throw new Error("The output encryption key is not loaded");
      if (!entry || entry.length === 0) return;

      const sealed = await encryptEntry(key, entry);
      const auth = this.#auth(client.session);
      const cacheKey = `${name}\u0000${auth.device}`;

      const { cid, appended, manifest } = await appendEntry(
        client.rb,
        this.#namespace,
        name,
        sealed,
        {
          auth,
          floors: await this.#floors(name),
          keep: this.keep,
          current: this.#manifests.get(cacheKey),
        },
      );

      this.#manifests.set(cacheKey, manifest);

      this.#packs.set(cid, sealed);

      try {
        await IDB.set(`${IDB_PACKS}/${cid}`, sealed);
      } catch (err) {
        console.warn("Could not cache a Firkin entry:", err);
      }

      // A duplicate entry changes neither the listing nor the other devices, so
      // it is not worth invalidating the cache or sending a hint for it.
      if (appended) {
        this.#invalidateListings();

        // Best effort: the entry is already committed, so a lost hint only
        // delays other devices.
        this.#channel?.send(name);
      }

      return { device: auth.device, manifest, cid, appended };
    };

    // Serialize appends per log so concurrent saves can't rewrite the same
    // manifest from a stale read.
    const previous = this.#writes.get(name) ?? Promise.resolve();
    const written = previous.then(run, run);
    this.#writes.set(name, written.then(() => {}, () => {}));

    return written;
  }

  /**
   * Read and decrypt one collection's log, framed entry by entry.
   *
   * @param {string} name
   * @param {CollectionDocument} current
   * @returns {Promise<{ data: Uint8Array | undefined; packs: string; signature?: string; unchanged: boolean; complete: boolean; incremental: boolean }>}
   */
  async #readLog(name, current) {
    const client = await getSessionClient({ cache: blockCache });
    if (!client) {
      return {
        data: undefined,
        packs: "",
        signature: undefined,
        unchanged: false,
        complete: false,
        incremental: false,
      };
    }

    const names = await this.#names(client.rb);
    const signature = logSignature(names, name);
    const heads = headsOf(current);
    const unpushed = await this.#hasUnpushed(name, current);
    const applied = await this.#appliedLog(name);

    // The registry listing is the log's identity: when every writer's manifest
    // still has the CID this device applied last, and the local document is
    // exactly that result, nothing was appended. Skip reading any manifest —
    // and the DAG pull each one would otherwise cost — entirely.
    if (
      applied?.signature === signature &&
      headsEqual(applied ? [applied.heads] : [], heads) &&
      !unpushed
    ) {
      return {
        data: undefined,
        packs: applied.packs,
        signature,
        unchanged: true,
        complete: true,
        incremental: false,
      };
    }

    const entries = await this.#entries(client, name, names);
    const packs = entries.map((entry) => entry.cid).join(";");

    if (entries.length === 0) {
      return {
        data: undefined,
        packs,
        signature,
        unchanged: false,
        complete: false,
        incremental: false,
      };
    }

    // The stored log is a prefix of the remote one: another writer appended,
    // which is the usual case. Fetch, decrypt and replay only the new entries
    // instead of the whole (up to `keep`) log. The stored log only describes
    // *this* document when its heads still match it; a document that diverged
    // (another tab wrote, a cache was restored) must replay the whole log, or
    // the entries it is missing would look like they are already applied.
    const appliedPacks = applied?.packs ? applied.packs.split(";") : [];
    const incremental = appliedPacks.length > 0 &&
      headsEqual(applied ? [applied.heads] : [], heads) &&
      hasRecords(current) &&
      appliedPacks.every((cid, index) => entries[index]?.cid === cid);

    const pending = entries.slice(incremental ? appliedPacks.length : 0);

    const loaded = await Promise.all(
      pending.map((entry) =>
        entry.bytes ?? this.#readPack(client.rb, entry.cid)
      ),
    );
    const present = /** @type {Uint8Array[]} */ (loaded.filter(Boolean));
    if (present.length === 0) {
      return {
        data: undefined,
        packs,
        signature,
        unchanged: false,
        complete: false,
        incremental,
      };
    }

    const key = outputKey();
    if (!key) {
      return {
        data: undefined,
        packs,
        signature,
        unchanged: false,
        complete: false,
        incremental,
      };
    }

    return {
      data: frameEntries(await decryptLog(key, concat(present))),
      packs,
      signature,
      unchanged: false,
      complete: present.length === pending.length,
      incremental,
    };
  }

  /**
   * Pull every collection. Serialized: concurrent calls share one pass.
   */
  #pull() {
    this.#pulling ??= (async () => {
      await this.#initialise();

      // One slow collection must not leave the others loading. The network
      // reads run in parallel; the applies are serialized by `#serialize`.
      const settle = setTimeout(() => {
        for (const name of COLLECTIONS) this.#markReady(name);
      }, FIRST_READ_TIMEOUT_MS);

      try {
        await Promise.all(COLLECTIONS.map(async (name) => {
          try {
            await (this.#isBundled(name)
              ? this.#pullBundled(name)
              : this.#pullCollection(name));
          } catch (err) {
            this.#remoteFailure(err, `Could not pull the '${name}' document:`);
            this.#markReady(name);
          }
        }));
      } finally {
        // Deliberately not cleared: if a read resolved but the output still
        // reports `loading`, this is what finally settles the UI.
        this.#pulling = undefined;
      }
    })();

    return this.#pulling;
  }

  /**
   * Pull a bundled collection: the legacy document, the manifest, then every
   * bundle the manifest names.
   *
   * @param {string} name
   */
  async #pullBundled(name) {
    // Data written before bundled mode is still in the old single document.
    // Read it so it stays visible; the next save migrates and clears it.
    await this.#pullCollection(name);
    await this.#pullCollection(this.#manifestName(name));

    const manifest = this.#docs.get(this.#manifestName(name));
    if (!manifest) return;

    const ids = recordsOf(manifest).map((record) => String(record.id));

    await Promise.all(
      ids.map((id) => this.#pullCollection(this.#bundleName(name, id))),
    );
  }

  /**
   * Re-read one collection from the log and apply it.
   *
   * @param {string} name
   */
  async #pullCollection(name) {
    await this.#initialise();

    // A slow or unreachable output must not hold the first paint hostage: show
    // what we have (usually nothing) and fill it in when the read lands.
    const settle = setTimeout(() => {
      this.#markReady(name);
    }, FIRST_READ_TIMEOUT_MS);

    try {
      if (!this.ready()) return;

      // Read (network + decrypt) outside the document queue: a slow or dead
      // server must not block saves or the other collections' applies.
      const current = this.#docs.get(name) ?? createDocument();
      const log = await this.#readLog(name, current);

      await this.#serialize(async () => {
        if (!this.ready()) return;

        await this.#applyLocked(
          name,
          { state: "loaded", ...log },
          { bootstrap: true },
        );
      });
    } finally {
      clearTimeout(settle);
    }
  }

  /**
   * The shared body. Must be called inside `#serialize`.
   *
   * @param {string} name
   * @param {{ state: "loading" } | { state: "loaded"; data: Uint8Array | undefined; packs: string; signature?: string; unchanged: boolean; complete: boolean; incremental: boolean } | { state: "error" }} collection
   * @param {{ bootstrap?: boolean }} [options] - `bootstrap` publishes our local
   *   document when the log has nothing yet; only safe right after a read.
   */
  async #applyLocked(name, collection, options = {}) {
    // A loading collection is not an answer; leave it to the next read.
    if (collection.state !== "loaded") return;

    try {
      // The stored pack list says the local document already is this log.
      if (collection.unchanged) return;

      const bytes = collection.data;
      const current = this.#docs.get(name) ?? createDocument();

      // The incremental read only fetched the entries appended after the stored
      // log. If the document was emptied in the meantime (a save raced the
      // read), those deltas have no base to apply to; the next pull does a full
      // read instead.
      if (collection.incremental && !hasRecords(current)) return;

      const before = headsOf(current).join(";");

      // Capture this before applying: if the local document has changes the log
      // doesn't, the merged heads below must not be recorded as pushed.
      const unpushed = await this.#hasUnpushed(name, current);

      if (!(bytes instanceof Uint8Array)) {
        // Nothing remote yet: publish what we have. Only when the log has no
        // entries at all — a non-empty pack list that produced no bytes means
        // the read failed partway, and pushing a whole snapshot then would
        // re-upload what the log already holds on every load. An empty
        // document is never published either.
        if (
          options.bootstrap && current.opCount() > 0 &&
          collection.packs === ""
        ) {
          this.#enqueuePush(name, current);
        }
        return;
      }

      // Replaying an unchanged log is wasted work; the output hands back the
      // same bytes, so remember what was applied.
      const fingerprint = fingerprintOf(bytes);
      if (fingerprint === this.#applied.get(name)) {
        if (
          this.#persisted.has(name) && !unpushed &&
          collection.complete && collection.packs.length > 0
        ) {
          await this.#setAppliedLog(
            name,
            collection.packs,
            before,
            collection.signature,
          );
        }

        // Reconcile the push cursor with a log that added nothing: a device
        // that restored its document from the cache can have the applied record
        // without a cursor, and without one its next push sends a snapshot.
        if (!unpushed && !this.#dirty.has(name)) {
          await this.#setCursor(name, {
            heads: headsOf(current),
            pushes: (await this.#cursor(name)).pushes,
          });
        }
        return;
      }

      const doc = applyLog(current, bytes, await this.#actor(name));

      this.#applied.set(name, fingerprint);
      this.#docs.set(name, doc);

      if (headsOf(doc).join(";") === before) {
        // The log added nothing. Still record it when the document (which is
        // already what the log describes) is on disk, so the next pull can skip
        // reading the log at all.
        if (
          this.#persisted.has(name) && !unpushed &&
          collection.complete && collection.packs.length > 0
        ) {
          await this.#setAppliedLog(
            name,
            collection.packs,
            before,
            collection.signature,
          );
        }

        // Retry a failed push now that something proved the server is
        // reachable; without this a dirty collection would wait for its next
        // save.
        if (unpushed) {
          this.#enqueuePush(name, doc);
          return;
        }

        if (!this.#dirty.has(name)) {
          await this.#setCursor(name, {
            heads: headsOf(doc),
            pushes: (await this.#cursor(name)).pushes,
          });
        }
        return;
      }

      this.#bump(name);

      let stored = false;
      this.#persisted.delete(name);

      try {
        await this.#persist(name, doc);
        this.#persisted.add(name);
        stored = true;
      } catch (err) {
        console.warn(`Could not cache the '${name}' document locally:`, err);
      }

      // Only record the log as applied after the document is on disk: a record
      // that leads the document would make the next load skip the entries the
      // stale document is missing. Also skip it when local-only changes remain,
      // or the next pull would stop retrying their push.
      if (
        stored && !unpushed && collection.complete &&
        collection.packs.length > 0
      ) {
        await this.#setAppliedLog(
          name,
          collection.packs,
          headsOf(doc).join(";"),
          collection.signature,
        );
      }

      // Only record these heads as remotely held when the local document was
      // fully pushed before the merge. Otherwise the merged heads include
      // local-only changes, and recording them would skip those changes on the
      // next push.
      if (!unpushed && !this.#dirty.has(name)) {
        const cursor = await this.#cursor(name);
        await this.#setCursor(name, {
          heads: headsOf(doc),
          pushes: cursor.pushes,
        });
      } else {
        this.#enqueuePush(name, doc);
      }
    } finally {
      this.#markReady(name);
    }
  }

  // READ

  /**
   * Read one pack from memory, IndexedDB, or the server.
   *
   * Caching each pack as it arrives is what makes an interrupted pull resume:
   * the next read re-uses what was already fetched and only asks for the rest.
   *
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
    } catch (err) {
      console.warn("Could not read a cached Firkin entry:", err);
    }

    try {
      const bytes = await rb.get(cid);
      this.#packs.set(cid, bytes);

      try {
        await IDB.set(`${IDB_PACKS}/${cid}`, bytes);
      } catch (err) {
        console.warn("Could not cache a Firkin entry:", err);
      }

      return bytes;
    } catch (err) {
      // A missing block makes for a shorter log; the reader decides whether
      // what remains is enough.
      if (err instanceof ApiError && err.status === 404) return undefined;
      throw err;
    }
  }

  // AUTH

  /**
   * Signing material for the current session.
   *
   * The device id is the session's public key, so a manifest can only ever be
   * written under the ref its key owns.
   *
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

  // SEQUENCES

  /** @param {string} name */
  #floorsKey(name) {
    return `${IDB_FLOORS}/${this.#namespace}/${name}`;
  }

  /**
   * The newest manifest sequence already accepted per device. A manifest with a
   * lower one is a replay and is ignored.
   *
   * @param {string} name
   * @returns {Promise<Map<string, number>>}
   */
  async #floors(name) {
    const cached = this.#floorCache.get(name);
    if (cached) return cached;

    /** @type {Map<string, number>} */
    const floors = new Map();

    try {
      const stored = await IDB.get(this.#floorsKey(name));
      if (stored && typeof stored === "object") {
        for (const [device, seq] of Object.entries(stored)) {
          if (typeof seq === "number") floors.set(device, seq);
        }
      }
    } catch (err) {
      console.warn(`Could not read the '${name}' manifest floors:`, err);
    }

    this.#floorCache.set(name, floors);

    return floors;
  }

  /**
   * @param {string} name
   * @param {Array<[string, number]>} accepted
   */
  async #recordFloors(name, accepted) {
    if (accepted.length === 0) return;

    const floors = await this.#floors(name);
    let dirty = false;

    for (const [device, seq] of accepted) {
      if ((floors.get(device) ?? 0) >= seq) continue;

      floors.set(device, seq);
      dirty = true;
    }

    if (!dirty) return;

    try {
      await IDB.set(this.#floorsKey(name), Object.fromEntries(floors));
    } catch (err) {
      console.warn(`Could not persist the '${name}' manifest floors:`, err);
    }
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

  /**
   * The namespace listing, shared by every collection in this pull pass.
   *
   * @param {Firkin} rb
   */
  async #names(rb) {
    return await this.#loadNames(rb)(registryNamespace(this.#namespace));
  }

  /**
   * Read a log with signature and rollback checks, recording the sequences it
   * accepted so a later replay can be recognised.
   *
   * @param {{ session: Session; rb: Firkin }} client
   * @param {string} name
   * @param {Record<string, string> | null} names
   */
  async #entries(client, name, names) {
    /** @type {Array<[string, number]>} */
    const accepted = [];

    const entries = await readEntries(client.rb, this.#namespace, name, {
      auth: this.#auth(client.session),
      floors: await this.#floors(name),
      loadNames: async () => names,
      onAccept: (device, seq) => accepted.push([device, seq]),
      onManifest: (device, manifestCid, payload) => {
        this.#manifests.set(`${name}\u0000${device}`, {
          cid: manifestCid,
          payload,
        });
      },
    });

    await this.#recordFloors(name, accepted);

    return entries;
  }
}

/**
 * Cheap content fingerprint. The log only grows, so length plus a hash is
 * enough to tell "same log as last time" from "something changed".
 *
 * @param {Uint8Array} bytes
 */
function fingerprintOf(bytes) {
  let hash = 2166136261;

  for (let index = 0; index < bytes.length; index++) {
    hash ^= bytes[index];
    hash = Math.imul(hash, 16777619);
  }

  return `${bytes.length}:${hash >>> 0}`;
}

/**
 * @param {Uint8Array[]} entries
 */
function concat(entries) {
  const total = entries.reduce((sum, bytes) => sum + bytes.length, 0);
  const out = new Uint8Array(total);

  let offset = 0;
  for (const bytes of entries) {
    out.set(bytes, offset);
    offset += bytes.length;
  }

  return out;
}

export default FirkinLoroOutput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = FirkinLoroOutput;
export const NAME = "dob-firkin-loro";

defineElement(NAME, FirkinLoroOutput);
