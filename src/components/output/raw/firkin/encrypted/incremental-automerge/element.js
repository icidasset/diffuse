import { ApiError, bytesToHex, WebCryptoSigner } from "@tokono.ma/firkin";
import * as IDB from "idb-keyval";

import { computed, signal } from "~/common/signal.js";
import { BroadcastedOutputElement } from "../../../../common.js";
import { defineElement } from "~/common/element.js";
import { collectionSchema } from "~/common/self-describing.js";
import { getSessionClient } from "~/common/firkin/session.js";
import {
  HINT_DEBOUNCE_MS,
  channelName,
  openHintChannel,
} from "~/common/firkin/channel.js";
import { outputKey } from "~/common/firkin/output-key.js";

import {
  BULK_SAVE_THRESHOLD,
  COLLECTIONS,
  FIRST_READ_TIMEOUT_MS,
  IDB_ACTOR,
  IDB_DOCS,
  IDB_FLOORS,
  IDB_PACKS,
  LOCAL_COMPACT_EVERY,
  NAMES_TTL_MS,
  NOT_CONNECTED,
  NOT_READY_GRACE_MS,
  PUSH_COALESCE_MS,
  SNAPSHOT_EVERY,
} from "./constants.js";
import { decryptLog, encryptEntry, frameEntries } from "./crypto.js";
import {
  applyIncremental,
  applyLog,
  changeDocument,
  changeDocumentBatched,
  changesSince,
  createDocument,
  deltaSince,
  encodeDocument,
  hasRecords,
  headsOf,
  loadDocument,
  recordsOf,
} from "./doc.js";
import { appendEntry, readEntries } from "./log.js";
import { verifySignature } from "./verify.js";

/**
 * @import { SignalReader } from "~/common/signal.d.ts";
 * @import { OutputElement } from "@specs/components/output/types.d.ts";
 * @import { CollectionName } from "~/common/self-describing.js";
 * @import { Heads } from "@automerge/automerge";
 * @import { Session } from "~/common/firkin/session.js";
 * @import { LogAuth } from "./log.js";
 * @import { Firkin } from "@tokono.ma/firkin";
 * @import { AutomergeDoc } from "./doc.js";
 */

/**
 * @typedef {Object} BulkActions
 * @property {(args: { bytes: Uint8Array; items: any[]; schema?: string }) => Promise<{ changed: boolean; deltas?: Uint8Array[] }>} applyLargeChange
 */

/** Yield to the event loop between applying the worker's deltas. */
const yieldToMain = () => new Promise((resolve) => setTimeout(resolve, 0));

/**
 * The userdata output: an **encrypted, incremental Automerge log on Firkin**,
 * in one component.
 *
 * This is deliberately one element rather than a chain of transformers. The
 * three layers are inseparable in practice — the log protocol decides what can
 * be replayed, the encryption framing decides how entries are split back up,
 * and the Automerge layer decides what is appendable — so keeping them in one
 * place removes the cross-component contracts (an `output-selector` chain, a
 * shared plaintext framing, a separately delivered key) that only ever had one
 * consumer.
 *
 * What it does, in order of the data flow:
 *
 * - **Collections** are Automerge documents (see `doc.js`): records live in a
 *   map keyed by `id`, so concurrent edits to different records merge.
 * - **Saves** append only the changes since the log last saw them; a whole
 *   document is appended every `SNAPSHOT_EVERY` saves so the log stays
 *   replayable after `keep` trims old entries.
 * - **Entries** are sealed with AES-GCM (see `crypto.js`): one entry per save,
 *   and the decrypted log is length-prefixed per entry so it can be replayed
 *   entry by entry.
 * - **The log** lives on Firkin (see `log.js`): per-device signed manifests
 *   over content-addressed blobs, with rollback floors, hints and trimming.
 * - **Replay** (see `doc.js`) tolerates interleaved writers and trimmed
 *   dependencies by applying entries one at a time.
 *
 * Local data lives in IndexedDB, so the UI is usable offline and the remote is
 * only read to merge in other devices' changes.
 *
 * @extends {BroadcastedOutputElement}
 */
class FirkinAutomergeOutput extends BroadcastedOutputElement {
  static NAME = "diffuse/output/raw/firkin/encrypted/incremental-automerge";
  static WORKER_URL =
    "components/output/raw/firkin/encrypted/incremental-automerge/worker.js";

  /** @type {import("~/common/worker.d.ts").ProxiedActions<BulkActions> | undefined} */
  #bulkWorker;

  // SIGNALS

  #isOnline = signal(navigator.onLine);
  #connected = signal(false);

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

  /** @type {Map<string, AutomergeDoc>} */
  #docs = new Map();

  /** Incremental local-cache deltas appended since the base document. */
  /** @type {Map<string, Uint8Array[]>} */
  #chunks = new Map();

  /** Heads covered by the local cache (base + chunks). @type {Map<string, Heads>} */
  #persistedHeads = new Map();

  /** Collections with a full base document in IndexedDB. */
  #hasBase = new Set();

  /** @type {Map<string, { heads?: Heads; pushes: number }>} */
  #cursors = new Map();

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

  /**
   * One namespace listing, shared by the collections that read from it.
   *
   * All five collections of an output live in the same namespace, so a pull
   * would otherwise ask the same question five times.
   *
   * @type {Map<string, { at: number; names: Promise<Record<string, string> | null> }>}
   */
  #listings = new Map();

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
   * @param {CollectionName} name
   */
  #entry(name) {
    return {
      collection: computed(() => {
        this.#versions.get(name)?.get();
        if (!this.#loaded.value) return { state: "loading" };
        if (!this.#ready.value.has(name)) return { state: "loading" };

        return {
          state: "loaded",
          data: recordsOf(this.#docs.get(name) ?? createDocument()),
        };
      }),
      reload: () => this.#pullCollection(name),
      save: (/** @type {any[]} */ items) => this.#save(name, items),
    };
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

    this.#catchUp();
  };

  /**
   * Re-read the log after the page was away or the connection was
   * re-established. Hints are best effort, so a missed hint is recovered here
   * rather than waiting for the next write or a reload.
   */
  #catchUp() {
    if (!this.isConnected) return;

    // The session check may be what is stale. A successful one flips `ready`,
    // whose effect pulls.
    if (!this.ready()) {
      void this.#refreshConnected();
      return;
    }

    this.#listings.clear();
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

    this.#refreshing = (async () => {
      do {
        this.#refreshAgain = false;
        await this.#checkConnected();
      } while (this.#refreshAgain);
    })().finally(() => {
      this.#refreshing = undefined;
    });

    return this.#refreshing;
  }

  async #checkConnected() {
    try {
      const client = await getSessionClient();
      if (!client) {
        this.#connected.value = false;
        return;
      }

      try {
        // A cheap authenticated read proves the session (and account) works.
        await client.rb.registryList();
        this.#connected.value = true;

        this.#ensureChannel(client);
      } catch (err) {
        // Unauthorized/forbidden means the account is unregistered or the
        // delegation was revoked; other errors are treated optimistically.
        this.#connected.value = !(
          err instanceof ApiError && (err.status === 401 || err.status === 403)
        );
      }
    } catch {
      this.#connected.value = false;
    }
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
        const current = await getSessionClient();
        if (!current) return undefined;

        return await current.rb.channelUrl(name);
      },
      onHint: (collection) => this.#refreshCollection(collection),
      onReconnect: () => this.#catchUp(),
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
   * @param {string} collection
   */
  #refreshCollection(collection) {
    if (!COLLECTIONS.includes(/** @type {any} */ (collection))) return;

    const pending = this.#hints.get(collection);
    if (pending !== undefined) clearTimeout(pending);

    this.#hints.set(
      collection,
      setTimeout(() => {
        this.#hints.delete(collection);

        // Another device wrote something: don't trust the cached listing.
        this.#listings.clear();

        void this.#pullCollection(/** @type {CollectionName} */ (collection))
          .catch((err) => {
            console.warn(
              `Could not refresh '${collection}' after a hint:`,
              err,
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

  /** @param {CollectionName} name */
  #docKey(name) {
    return `${IDB_DOCS}/${this.#namespace}/${name}`;
  }

  /** @param {CollectionName} name */
  #chunksKey(name) {
    return `${this.#docKey(name)}/changes`;
  }

  /**
   * Persist a collection's document.
   *
   * The first write (and periodic compaction) stores the whole document. Other
   * writes append the delta since the previous save, so a save doesn't serialise
   * the entire library. Loading replays the base and then the chunks.
   *
   * @param {CollectionName} name
   * @param {AutomergeDoc} doc
   * @param {{ compact?: boolean }} [options] - `compact` rewrites the base
   *   instead of appending a delta (a bulk save's delta is as large as the
   *   whole collection).
   */
  async #persist(name, doc, options = {}) {
    const heads = headsOf(doc);
    const previous = this.#persistedHeads.get(name);

    if (options.compact) {
      await IDB.set(this.#docKey(name), encodeDocument(doc));

      this.#hasBase.add(name);
      this.#persistedHeads.set(name, heads);
      this.#chunks.set(name, []);

      try {
        await IDB.del(this.#chunksKey(name));
      } catch (err) {
        console.warn(`Could not clear the '${name}' local changes:`, err);
      }

      return;
    }

    if (!this.#hasBase.has(name) || previous === undefined) {
      await IDB.set(this.#docKey(name), encodeDocument(doc));

      this.#hasBase.add(name);
      this.#persistedHeads.set(name, heads);
      this.#chunks.set(name, []);

      try {
        await IDB.del(this.#chunksKey(name));
      } catch (err) {
        console.warn(`Could not clear the '${name}' local changes:`, err);
      }

      return;
    }

    if (heads.join(";") === previous.join(";")) return;

    const delta = deltaSince(doc, previous);

    // The document no longer shares the cached base's history (for example an
    // empty local document adopted a remote root). A delta chunk couldn't be
    // replayed, so replace the base instead.
    if (delta === undefined) {
      await IDB.set(this.#docKey(name), encodeDocument(doc));

      this.#persistedHeads.set(name, heads);
      this.#chunks.set(name, []);

      try {
        await IDB.del(this.#chunksKey(name));
      } catch (err) {
        console.warn(`Could not clear the '${name}' local changes:`, err);
      }

      return;
    }

    if (delta.length === 0) {
      this.#persistedHeads.set(name, heads);
      return;
    }

    const chunks = [...(this.#chunks.get(name) ?? []), delta];

    if (chunks.length >= LOCAL_COMPACT_EVERY) {
      await IDB.set(this.#docKey(name), encodeDocument(doc));

      this.#persistedHeads.set(name, heads);
      this.#chunks.set(name, []);

      try {
        await IDB.del(this.#chunksKey(name));
      } catch (err) {
        console.warn(`Could not clear the '${name}' local changes:`, err);
      }

      return;
    }

    await IDB.set(this.#chunksKey(name), chunks);

    this.#chunks.set(name, chunks);
    this.#persistedHeads.set(name, heads);
  }

  /** @param {CollectionName} name */
  async #resetChunks(name) {
    this.#chunks.delete(name);
    this.#hasBase.delete(name);

    try {
      await IDB.del(this.#chunksKey(name));
    } catch (err) {
      console.warn(`Could not reset the '${name}' local changes:`, err);
    }
  }

  /**
   * Load the cached documents. Runs once; subsequent calls await the same work.
   */
  #initialise() {
    this.#initialising ??= (async () => {
      try {
        await Promise.all(COLLECTIONS.map(async (name) => {
          try {
            const stored = await IDB.get(this.#docKey(name));
            let doc = stored instanceof Uint8Array
              ? loadDocument(stored)
              : undefined;

            if (doc) {
              this.#hasBase.add(name);

              // Replay the deltas appended since the base was written.
              try {
                const storedChunks = await IDB.get(this.#chunksKey(name));
                const chunks = Array.isArray(storedChunks)
                  ? storedChunks.filter((chunk) => chunk instanceof Uint8Array)
                  : [];
                /** @type {Uint8Array[]} */
                const applied = [];

                for (const chunk of chunks) {
                  try {
                    doc = applyIncremental(doc, chunk);
                    applied.push(chunk);
                  } catch (err) {
                    console.warn(
                      `Could not apply a cached '${name}' change:`,
                      err,
                    );
                    break;
                  }
                }

                this.#chunks.set(name, applied);
              } catch (err) {
                console.warn(`Could not read the '${name}' local changes:`, err);
              }

              this.#docs.set(name, doc);
              this.#persisted.add(name);
              this.#persistedHeads.set(name, headsOf(doc));

              // Local-first: a collection we have synced before can be shown
              // straight away — even when it is empty, because then "empty" is
              // the truth rather than "unknown".
              const cursor = await this.#cursor(name);
              if (hasRecords(doc) || cursor.heads !== undefined) {
                this.#markReady(name);
              }
            } else {
              // Nothing cached: start from an empty document of our own, under
              // this device's actor so concurrent writers never share a
              // sequence number.
              const empty = createDocument(await this.#actor());

              this.#docs.set(name, empty);
              this.#persisted.delete(name);
              this.#persistedHeads.set(name, headsOf(empty));

              await this.#resetCursor(name);
              await this.#resetAppliedLog(name);
              await this.#resetChunks(name);
            }
          } catch (err) {
            console.warn(`Could not read the local '${name}' document:`, err);
          }
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
   * @param {CollectionName} name
   * @param {any[]} items
   */
  async #save(name, items) {
    await this.#initialise();

    return await this.#serialize(async () => {
      const current = this.#docs.get(name) ?? createDocument();
      const schema = collectionSchema(name);
      const bulk = items.length >= BULK_SAVE_THRESHOLD;

      let doc;
      let changed;

      if (bulk) {
        try {
          ({ doc, changed } = await this.#applyLargeChange(
            current,
            items,
            schema,
          ));
        } catch (err) {
          console.warn(
            "Could not apply the save in a worker; falling back:",
            err,
          );

          ({ doc, changed } = await changeDocumentBatched(
            current,
            items,
            { schema },
          ));
        }
      } else {
        ({ doc, changed } = changeDocument(current, items, { schema }));
      }

      if (!changed) return;

      this.#docs.set(name, doc);
      this.#bump(name);

      this.#persisted.delete(name);

      try {
        // A bulk save's delta would be as large as the collection; rewrite the
        // base instead of appending it as a chunk.
        await this.#persist(name, doc, { compact: bulk });
        this.#persisted.add(name);
      } catch (err) {
        console.warn(`Could not cache the '${name}' document locally:`, err);
      }

      this.#schedulePush(name);
    });
  }

  #bulkProxy() {
    this.#bulkWorker ??= this.workerProxy();
    return this.#bulkWorker;
  }

  /**
   * Apply a large save in the worker, which returns one delta per batch. The
   * deltas are applied here with yields, so neither the Automerge change nor
   * the resulting document update blocks the page.
   *
   * @param {AutomergeDoc} current
   * @param {any[]} items
   * @param {string} schema
   * @returns {Promise<{ doc: AutomergeDoc; changed: boolean }>}
   */
  async #applyLargeChange(current, items, schema) {
    const result = await this.#bulkProxy().applyLargeChange({
      bytes: encodeDocument(current),
      items,
      schema,
    });

    if (!result.changed) return { doc: current, changed: false };

    let doc = current;

    for (const delta of result.deltas ?? []) {
      doc = applyIncremental(doc, delta);
      await yieldToMain();
    }

    return { doc, changed: true };
  }

  /**
   * Queue a push, off the document queue: a slow or dead server must not block
   * reads, saves or other collections.
   *
   * @param {CollectionName} name
   * @param {AutomergeDoc} doc
   */
  #enqueuePush(name, doc) {
    const run = () => this.#push(name, doc);

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
   * @param {CollectionName} name
   */
  #schedulePush(name) {
    const pending = this.#pushTimers.get(name);
    if (pending !== undefined) clearTimeout(pending);

    this.#pushTimers.set(
      name,
      setTimeout(() => {
        this.#pushTimers.delete(name);

        const doc = this.#docs.get(name);
        if (doc) this.#enqueuePush(name, doc);
      }, PUSH_COALESCE_MS),
    );
  }

  /** Send every coalesced push now (before the page goes away). */
  #flushPushes() {
    for (const [name, timer] of this.#pushTimers) {
      clearTimeout(timer);

      const doc = this.#docs.get(name);
      if (doc) this.#enqueuePush(name, doc);
    }

    this.#pushTimers.clear();
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
        state = "not signed in on this browser; sign in on Connect → Firkin";
      } else {
        state = "signed in, but this browser has no output encryption key " +
          "(it is derived from your passkey at sign-in) — sign in again on " +
          "this device to derive it";
      }

      console.warn(
        `Firkin output: ${state}. Showing an empty library until then.`,
      );

      for (const name of COLLECTIONS) this.#markReady(name);
    }, NOT_READY_GRACE_MS);
  }

  /**
   * Record that a collection has an answer to show.
   *
   * @param {CollectionName} name
   */
  #markReady(name) {
    if (this.#ready.value.has(name)) return;

    const next = new Set(this.#ready.value);
    next.add(name);
    this.#ready.value = next;
  }

  /** @param {CollectionName} name */
  #bump(name) {
    const version = this.#versions.get(name);
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
   * @param {CollectionName} name
   */
  #appliedLogKey(name) {
    return `${IDB_DOCS}/${this.#namespace}/${name}/applied`;
  }

  /**
   * @param {CollectionName} name
   * @returns {Promise<{ packs: string; heads: string } | undefined>}
   */
  async #appliedLog(name) {
    try {
      const stored = await IDB.get(this.#appliedLogKey(name));

      if (
        stored && typeof stored === "object" &&
        typeof stored.packs === "string" &&
        typeof stored.heads === "string"
      ) {
        return { packs: stored.packs, heads: stored.heads };
      }
    } catch (err) {
      console.warn(`Could not read the '${name}' applied log:`, err);
    }

    return undefined;
  }

  /**
   * @param {CollectionName} name
   * @param {string} packs - `;`-joined ordered pack CIDs.
   * @param {string} heads - The document heads those packs produced.
   */
  async #setAppliedLog(name, packs, heads) {
    try {
      await IDB.set(this.#appliedLogKey(name), { packs, heads });
    } catch (err) {
      console.warn(`Could not persist the '${name}' applied log:`, err);
    }
  }

  /** @param {CollectionName} name */
  async #resetAppliedLog(name) {
    try {
      await IDB.del(this.#appliedLogKey(name));
    } catch (err) {
      console.warn(`Could not reset the '${name}' applied log:`, err);
    }
  }

  // REMOTE

  /** @param {CollectionName} name */
  #cursorKey(name) {
    return `${IDB_DOCS}/${this.#namespace}/${name}/cursor`;
  }

  /**
   * How far this device has pushed a collection: the heads the log already has,
   * and how many pushes happened since the document was started.
   *
   * @param {CollectionName} name
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
   * @param {CollectionName} name
   * @param {{ heads?: Heads; pushes: number }} cursor
   */
  async #setCursor(name, cursor) {
    this.#cursors.set(name, cursor);

    try {
      await IDB.set(this.#cursorKey(name), cursor);
    } catch (err) {
      console.warn(`Could not persist the '${name}' push cursor:`, err);
    }
  }

  /** @param {CollectionName} name */
  async #resetCursor(name) {
    const cursor = { pushes: 0 };
    this.#cursors.set(name, cursor);

    try {
      await IDB.del(this.#cursorKey(name));
    } catch (err) {
      console.warn(`Could not reset the '${name}' push cursor:`, err);
    }
  }

  /** @type {Promise<string> | undefined} */
  #actorPromise;

  /**
   * This device's Automerge actor id, persisted so reloads keep writing under
   * the same identity.
   *
   * @returns {Promise<string>}
   */
  #actor() {
    this.#actorPromise ??= (async () => {
      const key = `${IDB_ACTOR}/${this.#namespace}`;

      const created = crypto.randomUUID().replace(/-/g, "");

      try {
        const stored = await IDB.get(key);
        if (typeof stored === "string" && /^[0-9a-f]{32}$/.test(stored)) {
          return stored;
        }

        await IDB.set(key, created);
      } catch (err) {
        console.warn("Could not persist the Automerge actor id:", err);
      }

      return created;
    })();

    return this.#actorPromise;
  }

  /**
   * Whether the local document has changes the log hasn't acknowledged.
   *
   * The cursor holds the heads of the last successful push (and the heads a
   * completed read proved the log has), so anything newer is local-only: a
   * save whose push failed, or one interrupted before its queued push ran.
   *
   * @param {CollectionName} name
   * @param {AutomergeDoc} doc
   */
  async #hasUnpushed(name, doc) {
    const cursor = await this.#cursor(name);

    return headsOf(doc).join(";") !== (cursor.heads ?? []).join(";");
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

    for (const name of COLLECTIONS) {
      const doc = this.#docs.get(name);
      if (!doc) continue;
      if (!(await this.#hasUnpushed(name, doc))) continue;

      this.#enqueuePush(name, doc);
    }
  }

  /**
   * Push one document to the log.
   *
   * The first push of a session is a delta when we know what the log has (see
   * the cursor advance after a pull); otherwise `changesSince` returns a whole
   * document anyway.
   *
   * @param {CollectionName} name
   * @param {AutomergeDoc} doc
   */
  async #push(name, doc) {
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
    const snapshot = cursor.pushes > 0 && cursor.pushes % SNAPSHOT_EVERY === 0;

    const bytes = snapshot ? encodeDocument(doc) : changesSince(doc, cursor.heads);
    if (bytes.length === 0) return;

    try {
      await this.#append(name, bytes);
    } catch (err) {
      this.#dirty.add(name);
      console.warn(`Could not push the '${name}' changes:`, err);
      return;
    }

    this.#dirty.delete(name);
    await this.#setCursor(name, {
      heads: headsOf(doc),
      pushes: cursor.pushes + 1,
    });
  }

  /**
   * Append one plaintext entry to the log, sealing it first.
   *
   * @param {CollectionName} name
   * @param {Uint8Array} entry
   * @returns {Promise<void>}
   */
  #append(name, entry) {
    const run = async () => {
      const client = await getSessionClient();
      if (!client) throw new Error(NOT_CONNECTED);

      const key = outputKey();
      if (!key) throw new Error("The output encryption key is not loaded");
      if (!entry || entry.length === 0) return;

      const sealed = await encryptEntry(key, entry);

      const { cid } = await appendEntry(
        client.rb,
        this.#namespace,
        name,
        sealed,
        {
          auth: this.#auth(client.session),
          floors: await this.#floors(name),
          keep: this.keep,
        },
      );

      this.#packs.set(cid, sealed);

      try {
        await IDB.set(`${IDB_PACKS}/${cid}`, sealed);
      } catch (err) {
        console.warn("Could not cache a Firkin entry:", err);
      }

      // Our own write changes the namespace listing.
      this.#listings.clear();

      // Best effort: the entry is already committed, so a lost hint only
      // delays other devices.
      this.#channel?.send(name);
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
   * @param {CollectionName} name
   * @param {AutomergeDoc} current
   * @returns {Promise<{ data: Uint8Array | undefined; packs: string; unchanged: boolean; complete: boolean; incremental: boolean }>}
   */
  async #readLog(name, current) {
    const client = await getSessionClient();
    if (!client) {
      return {
        data: undefined,
        packs: "",
        unchanged: false,
        complete: false,
        incremental: false,
      };
    }

    const entries = await this.#entries(client, name);
    const packs = entries.map((entry) => entry.cid).join(";");
    const applied = entries.length > 0 ? await this.#appliedLog(name) : undefined;

    // Content-addressed change detection: the ordered pack CIDs are the log's
    // identity, and the stored heads prove the local document still has exactly
    // the result of applying them. Nothing was appended, so skip the fetch,
    // decrypt and replay entirely.
    if (entries.length > 0) {
      if (
        applied?.packs === packs &&
        applied.heads === headsOf(current).join(";") &&
        // The cursor must agree too, or a crash between recording the log and
        // the push cursor would make this skip hide a pending push.
        !(await this.#hasUnpushed(name, current))
      ) {
        return {
          data: undefined,
          packs,
          unchanged: true,
          complete: true,
          incremental: false,
        };
      }
    }

    if (entries.length === 0) {
      return {
        data: undefined,
        packs,
        unchanged: false,
        complete: false,
        incremental: false,
      };
    }

    // The stored log is a prefix of the remote one: another writer appended,
    // which is the usual case. Fetch, decrypt and replay only the new entries
    // instead of the whole (up to `keep`) log.
    const appliedPacks = applied?.packs ? applied.packs.split(";") : [];
    const incremental = appliedPacks.length > 0 &&
      hasRecords(current) &&
      appliedPacks.every((cid, index) => entries[index]?.cid === cid);

    const pending = entries.slice(incremental ? appliedPacks.length : 0);

    const loaded = await Promise.all(
      pending.map((entry) => this.#readPack(client.rb, entry.cid)),
    );
    const present = /** @type {Uint8Array[]} */ (loaded.filter(Boolean));
    if (present.length === 0) {
      return {
        data: undefined,
        packs,
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
        unchanged: false,
        complete: false,
        incremental,
      };
    }

    return {
      data: frameEntries(await decryptLog(key, concat(present))),
      packs,
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
            await this.#pullCollection(name);
          } catch (err) {
            console.warn(`Could not pull the '${name}' document:`, err);
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
   * Re-read one collection from the log and apply it.
   *
   * @param {CollectionName} name
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
   * @param {CollectionName} name
   * @param {{ state: "loading" } | { state: "loaded"; data: Uint8Array | undefined; packs: string; unchanged: boolean; complete: boolean; incremental: boolean } | { state: "error" }} collection
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
        // Nothing remote yet: publish what we have so the namespace exists and
        // another device can start from it. An empty document counts — it makes
        // the first read cheaper and surfaces a failing push straight away.
        if (options.bootstrap) {
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
          await this.#setAppliedLog(name, collection.packs, before);
        }
        return;
      }

      const doc = applyLog(current, bytes, await this.#actor());

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
          await this.#setAppliedLog(name, collection.packs, before);
        }

        // Retry a failed push now that something proved the server is
        // reachable; without this a dirty collection would wait for its next
        // save.
        if (unpushed) this.#enqueuePush(name, doc);
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
   * The namespace listing, reused for a moment so a pull doesn't ask the same
   * question once per collection.
   *
   * @param {Firkin} rb
   */
  #loadNames(rb) {
    return (/** @type {string} */ namespace) => {
      const cached = this.#listings.get(namespace);
      if (cached && Date.now() - cached.at < NAMES_TTL_MS) {
        return cached.names;
      }

      const names = rb.registryNames(namespace).catch((err) => {
        this.#listings.delete(namespace);
        throw err;
      });

      this.#listings.set(namespace, { at: Date.now(), names });

      return names;
    };
  }

  /**
   * Read a log with signature and rollback checks, recording the sequences it
   * accepted so a later replay can be recognised.
   *
   * @param {{ session: Session; rb: Firkin }} client
   * @param {CollectionName} name
   */
  async #entries(client, name) {
    /** @type {Array<[string, number]>} */
    const accepted = [];

    const entries = await readEntries(client.rb, this.#namespace, name, {
      auth: this.#auth(client.session),
      floors: await this.#floors(name),
      loadNames: this.#loadNames(client.rb),
      onAccept: (device, seq) => accepted.push([device, seq]),
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

export default FirkinAutomergeOutput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = FirkinAutomergeOutput;
export const NAME = "dob-firkin-automerge";

defineElement(NAME, FirkinAutomergeOutput);
