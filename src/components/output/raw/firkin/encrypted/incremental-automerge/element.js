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
  COLLECTIONS,
  FIRST_READ_TIMEOUT_MS,
  IDB_ACTOR,
  IDB_DOCS,
  IDB_FLOORS,
  IDB_PACKS,
  NAMES_TTL_MS,
  NOT_CONNECTED,
  NOT_READY_GRACE_MS,
  SNAPSHOT_EVERY,
} from "./constants.js";
import { decryptLog, encryptEntry, frameEntries } from "./crypto.js";
import {
  applyLog,
  changeDocument,
  changesSince,
  createDocument,
  encodeDocument,
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

  // SIGNALS

  #isOnline = signal(navigator.onLine);
  #connected = signal(false);

  /** Loaded from local storage; collections stay `loading` until then. */
  #loaded = signal(false);

  /** Bumped whenever a document changes so the collection computeds re-run. */
  #version = signal(0);

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

  /** @type {Map<string, { heads?: Heads; pushes: number }>} */
  #cursors = new Map();

  /**
   * Fingerprint of the log last applied per collection, so a reload that yields
   * the same bytes isn't replayed.
   *
   * @type {Map<string, string>}
   */
  #applied = new Map();

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
      // encryption key arrived) only reached the local cache. Now that it can
      // be written, push it.
      for (const name of this.#dirty) {
        const doc = this.#docs.get(name);
        if (doc) this.#enqueuePush(name, doc);
      }

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
        this.#version.get();
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

    void this.#initialise();
    void this.#refreshConnected();
  }

  /** @override */
  disconnectedCallback() {
    globalThis.removeEventListener("online", this.#online);
    globalThis.removeEventListener("offline", this.#offline);

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

  async #refreshConnected() {
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

  /**
   * Load the cached documents. Runs once; subsequent calls await the same work.
   */
  #initialise() {
    this.#initialising ??= (async () => {
      try {
        await Promise.all(COLLECTIONS.map(async (name) => {
          try {
            const stored = await IDB.get(this.#docKey(name));
            const doc = stored instanceof Uint8Array
              ? loadDocument(stored)
              : undefined;

            if (doc) {
              this.#docs.set(name, doc);

              // Local-first: a collection we have synced before can be shown
              // straight away — even when it is empty, because then "empty" is
              // the truth rather than "unknown".
              const cursor = await this.#cursor(name);
              if (recordsOf(doc).length > 0 || cursor.heads !== undefined) {
                this.#markReady(name);
              }
            } else {
              // Nothing cached: start from an empty document of our own, under
              // this device's actor so concurrent writers never share a
              // sequence number.
              this.#docs.set(name, createDocument(await this.#actor()));
              await this.#resetCursor(name);
            }
          } catch (err) {
            console.warn(`Could not read the local '${name}' document:`, err);
          }
        }));
      } finally {
        // Whatever happened above, the interface must stop loading.
        this.#loaded.value = true;
        this.#version.value++;
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
      const { doc, changed } = changeDocument(current, items, {
        schema: collectionSchema(name),
        stripUndefined: name === "facets",
      });

      if (!changed) return;

      this.#docs.set(name, doc);
      this.#version.value++;

      try {
        await IDB.set(this.#docKey(name), encodeDocument(doc));
      } catch (err) {
        console.warn(`Could not cache the '${name}' document locally:`, err);
      }

      this.#enqueuePush(name, doc);
    });
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
   * @returns {Promise<Uint8Array | undefined>}
   */
  async #readLog(name) {
    const client = await getSessionClient();
    if (!client) return undefined;

    const entries = await this.#entries(client, name);
    if (entries.length === 0) return undefined;

    const packs = await Promise.all(
      entries.map((entry) => this.#readPack(client.rb, entry.cid)),
    );
    const present = /** @type {Uint8Array[]} */ (packs.filter(Boolean));
    if (present.length === 0) return undefined;

    const key = outputKey();
    if (!key) return undefined;

    return frameEntries(await decryptLog(key, concat(present)));
  }

  /**
   * Pull every collection. Serialized: concurrent calls share one pass.
   */
  #pull() {
    this.#pulling ??= (async () => {
      await this.#initialise();

      // One slow collection must not leave the others loading, and the reads
      // run one after another.
      const settle = setTimeout(() => {
        for (const name of COLLECTIONS) this.#markReady(name);
      }, FIRST_READ_TIMEOUT_MS);

      try {
        for (const name of COLLECTIONS) {
          try {
            await this.#pullCollection(name);
          } catch (err) {
            console.warn(`Could not pull the '${name}' document:`, err);
            this.#markReady(name);
          }
        }
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
      await this.#serialize(async () => {
        if (!this.ready()) return;

        const log = await this.#readLog(name);

        await this.#applyLocked(
          name,
          { state: "loaded", data: log },
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
   * @param {{ state: "loading" } | { state: "loaded"; data: Uint8Array | undefined } | { state: "error" }} collection
   * @param {{ bootstrap?: boolean }} [options] - `bootstrap` publishes our local
   *   document when the log has nothing yet; only safe right after a read.
   */
  async #applyLocked(name, collection, options = {}) {
    // A loading collection is not an answer; leave it to the next read.
    if (collection.state !== "loaded") return;

    try {
      const bytes = collection.data;
      const current = this.#docs.get(name) ?? createDocument();

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
      if (fingerprint === this.#applied.get(name)) return;

      const before = headsOf(current).join(";");
      const doc = applyLog(current, bytes, await this.#actor());

      this.#applied.set(name, fingerprint);
      this.#docs.set(name, doc);

      if (headsOf(doc).join(";") === before) return;

      this.#version.value++;

      try {
        await IDB.set(this.#docKey(name), encodeDocument(doc));
      } catch (err) {
        console.warn(`Could not cache the '${name}' document locally:`, err);
      }

      // The log we just replayed holds everything we know about, so the remote
      // already has these heads: the next save can append only what changed.
      if (!this.#dirty.has(name)) {
        const cursor = await this.#cursor(name);
        await this.#setCursor(name, {
          heads: headsOf(doc),
          pushes: cursor.pushes,
        });
      }
    } finally {
      this.#markReady(name);
    }
  }

  // READ

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
