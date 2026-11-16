import * as IDB from "idb-keyval";
import { ifDefined } from "lit-html/directives/if-defined.js";

import "~/components/output/polymorphic/indexed-db/element.js";

import { computed, signal } from "~/common/signal.js";
import { OutputTransformer } from "../../base.js";
import { collectionSchema } from "~/common/self-describing.js";
import { defineElement } from "~/common/element.js";

import {
  COLLECTIONS,
  FIRST_READ_TIMEOUT_MS,
  IDB_PREFIX,
  LOCAL_READ_TIMEOUT_MS,
  NOT_READY_GRACE_MS,
  SNAPSHOT_EVERY,
} from "./constants.js";
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

/**
 * @import { RenderArg } from "~/common/element.d.ts"
 * @import { SignalReader } from "~/common/signal.d.ts";
 * @import { OutputElement } from "@specs/components/output/types.d.ts";
 * @import { CollectionName } from "~/common/self-describing.js"
 * @import { Heads } from "@automerge/automerge"
 */

/**
 * Stores each collection as an Automerge document.
 *
 * It only uses the standard output interface — `ready`, `collection`, `save`,
 * `reload` — so it knows nothing about the output underneath it. Two things
 * make that enough:
 *
 * - **Records live in a map keyed by `id`** (see `doc.js`), so concurrent edits
 *   to different records merge instead of one device's collection winning.
 * - **Saves append deltas** (`saveSince`), and a whole document is written every
 *   `SNAPSHOT_EVERY` saves. Over an output that keeps history (a log, like the
 *   Firkin output in `mode="log"`), a save is a few hundred bytes and `get`
 *   returns the whole log to replay.
 *
 * Deltas only reconstruct a value when every earlier entry is still readable, so
 * an output that *replaces* its value (S3, Dropbox) needs `snapshot` set: every
 * save then writes a whole document. That is configuration on this element —
 * nothing here inspects the output for capabilities.
 *
 * Local data lives in a `dop-indexed-db` child, so the UI is usable offline and
 * the remote is only read to merge in other devices' changes.
 *
 * @extends {OutputTransformer<Uint8Array>}
 */
class AutomergeBytesOutputTransformer extends OutputTransformer {
  // SIGNALS

  #localOutput = signal(
    /** @type {OutputElement<Uint8Array | undefined> | undefined} */ (undefined),
  );

  /** Loaded from local storage; collections stay `loading` until then. */
  #loaded = signal(false);

  /** Bumped whenever a document changes so the collection computeds re-run. */
  #version = signal(0);

  /**
   * Collections we have an answer for: either a local document with records, or
   * a completed read of the output. Until a collection is in here its state
   * stays `loading`, so the UI shows its loading state instead of flashing an
   * empty list while the first read is still in flight.
   *
   * @type {import("~/common/signal.d.ts").Signal<Set<string>>}
   */
  #ready = signal(new Set());

  // STATE

  /** @type {Map<string, import("./doc.js").AutomergeDoc>} */
  #docs = new Map();

  /** @type {Map<string, { heads?: Heads; pushes: number }>} */
  #cursors = new Map();

  /**
   * Fingerprint of the remote bytes last applied per collection, so a reload
   * that yields the same log isn't replayed.
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
   * Resolves once `connectedCallback` has found the local output. Effects run
   * from the constructor, so without this a pull can start initialising before
   * the local cache is known — and skip it entirely.
   *
   * @type {PromiseWithResolvers<void>}
   */
  #localReady = Promise.withResolvers();

  /**
   * Serializes document mutations. Without this a pull that started before a
   * save would overwrite the document the save just wrote.
   */
  #queue = Promise.resolve();

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
    this.ready = computed(() => this.#loaded.value && this.base().ready());

    // Pull once the backing output is usable (signed in, online, …). Retries
    // whenever that flips back to true after a failure.
    this.effect(() => {
      const remote = this.base();

      if (!remote.ready()) {
        // Nothing can be read yet. Don't leave the UI spinning if that never
        // changes (no session, no encryption key).
        this.#scheduleSettle();
        return;
      }

      clearTimeout(this.#settle);
      this.#settle = undefined;

      // A save made before the output was usable (e.g. carrying the local copy
      // in before the encryption key arrived) only reached the local cache.
      // Now that it can be written, push it.
      for (const name of this.#dirty) {
        const doc = this.#docs.get(name);
        if (doc) this.#enqueuePush(name, doc);
      }

      void this.#pull();
    });

    // Apply anything the output hands over later without being asked — a live
    // hint that made it re-read, or a transformer below finishing async work.
    // Deliberately does not reload: that would feed back into this effect.
    this.effect(() => {
      const remote = this.base();
      if (!remote.ready()) return;

      for (const name of COLLECTIONS) {
        void this.#apply(name, remote[name].collection());
      }
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

    /** @type {OutputElement<Uint8Array | undefined> | null} */
    const local = this.root().querySelector("dop-indexed-db");
    if (!local) {
      // Nothing to restore from; don't leave `#initialise` waiting.
      this.#localReady.resolve();
      throw new Error("Can't find local output");
    }

    customElements.whenDefined(local.localName).then(() => {
      this.#localOutput.value = local;
      this.#localReady.resolve();
      void this.#initialise();
    });
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    return html`
      <dop-indexed-db
        namespace="${ifDefined(this.getAttribute(`namespace`))}"
      ></dop-indexed-db>
    `;
  }

  // LOCAL

  /**
   * Load the cached documents. Runs once; subsequent calls await the same work.
   */
  #initialise() {
    this.#initialising ??= (async () => {
      try {
        // `connectedCallback` resolves this; the timeout is a belt-and-braces
        // so a missing local element can never leave the collections loading.
        await Promise.race([
          this.#localReady.promise,
          new Promise((resolve) => setTimeout(resolve, NOT_READY_GRACE_MS)),
        ]);

        const local = this.#localOutput.value;

        if (local) {
          await Promise.all(COLLECTIONS.map(async (name) => {
            try {
              // The local output reads through a worker; a stuck worker must
              // not keep the whole interface loading.
              await Promise.race([
                local[name].reload(),
                new Promise((resolve) =>
                  setTimeout(resolve, LOCAL_READ_TIMEOUT_MS)
                ),
              ]);

              const collection = local[name].collection();
              if (collection.state !== "loaded") return;

              const doc = loadDocument(collection.data);
              if (doc) {
                this.#docs.set(name, doc);

                // Local-first: a collection we have synced before can be shown
                // straight away — even when it is empty, because then "empty"
                // is the truth rather than "unknown".
                const cursor = await this.#cursor(name);
                if (
                  recordsOf(doc).length > 0 || cursor.heads !== undefined
                ) {
                  this.#markReady(name);
                }
              } else {
                // Nothing cached: start from an empty document of our own,
                // under this device's actor so concurrent writers never share a
                // sequence number.
                this.#docs.set(name, createDocument(await this.#actor()));
                await this.#resetCursor(name);
              }
            } catch (err) {
              console.warn(`Could not read the local '${name}' document:`, err);
            }
          }));
        }
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

      const local = this.#localOutput.value;
      if (local) {
        try {
          await local[name].save(encodeDocument(doc));
        } catch (err) {
          console.warn(`Could not cache the '${name}' document locally:`, err);
        }
      }

      this.#enqueuePush(name, doc);
    });
  }

  /** @type {Promise<void>} */
  #pushQueue = Promise.resolve();

  /**
   * Queue a push, off the document queue: a slow or dead server must not block
   * reads, saves or other collections.
   *
   * @param {CollectionName} name
   * @param {import("./doc.js").AutomergeDoc} doc
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

      if (this.base().ready()) return;

      console.warn(
        "Firkin output: nothing can be read yet (no session, or no " +
          "encryption key on this browser). Showing an empty library; sign " +
          "in on Connect → Firkin to fix it.",
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

  /**
   * Push one document to the backing output.
   *
   * @param {CollectionName} name
   * @param {import("./doc.js").AutomergeDoc} doc
   */
  async #push(name, doc) {
    // Without the backing output there is nothing to push to yet; wait for it
    // so an early save isn't silently dropped.
    await this.output.whenDefined;

    const cursor = await this.#cursor(name);
    // The first push of a session is a delta when we know what the remote has
    // (see the cursor advance after a pull); otherwise `changesSince` returns a
    // whole document anyway.
    const snapshot = this.hasAttribute("snapshot") ||
      (cursor.pushes > 0 && cursor.pushes % SNAPSHOT_EVERY === 0);

    const bytes = snapshot ? encodeDocument(doc) : changesSince(doc, cursor.heads);
    if (bytes.length === 0) return;

    try {
      await this.base()[name].save(bytes);
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
   * Re-read one collection from the output and apply it.
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
        const remote = this.base();
        if (!remote.ready()) return;

        await Promise.race([
          remote[name].reload(),
          new Promise((resolve) => setTimeout(resolve, FIRST_READ_TIMEOUT_MS)),
        ]);

        await this.#applyLocked(name, remote[name].collection(), {
          bootstrap: true,
        });
      });
    } finally {
      clearTimeout(settle);
    }
  }

  /**
   * Apply a collection value the output already has.
   *
   * @param {CollectionName} name
   * @param {{ state: "loading" } | { state: "loaded"; data: Uint8Array | undefined } | { state: "error" }} collection
   */
  async #apply(name, collection) {
    await this.#initialise();

    await this.#serialize(async () => {
      const remote = this.base();
      if (!remote.ready()) return;

      await this.#applyLocked(name, collection);
    });
  }

  /**
   * The shared body. Must be called inside `#serialize`.
   *
   * @param {CollectionName} name
   * @param {{ state: "loading" } | { state: "loaded"; data: Uint8Array | undefined } | { state: "error" }} collection
   * @param {{ bootstrap?: boolean }} [options] - `bootstrap` publishes our local
   *   document when the output has nothing yet; only safe right after a read.
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

      const local = this.#localOutput.value;
      if (local) {
        try {
          await local[name].save(encodeDocument(doc));
        } catch (err) {
          console.warn(`Could not cache the '${name}' document locally:`, err);
        }
      }

      // The log we just replayed holds everything we know about, so the remote
      // already has these heads: the next save can append only what changed.
      if (!this.#dirty.has(name)) {
        const cursor = await this.#cursor(name);
        await this.#setCursor(name, { heads: headsOf(doc), pushes: cursor.pushes });
      }
    } finally {
      this.#markReady(name);
    }
  }

  // ACTOR

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
      const key = `${IDB_PREFIX}/${this.namespace ?? "default"}/actor`;

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

  // PUSH CURSOR

  /** @param {CollectionName} name */
  #cursorKey(name) {
    return `${IDB_PREFIX}/${this.namespace ?? "default"}/${name}/cursor`;
  }

  /**
   * How far this device has pushed a collection: the heads the remote already
   * has, and how many pushes happened since the document was started.
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

export default AutomergeBytesOutputTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = AutomergeBytesOutputTransformer;
export const NAME = "dtob-automerge";

defineElement(NAME, CLASS);
