import * as IDB from "idb-keyval";
import { ifDefined } from "lit-html/directives/if-defined.js";

import "~/components/output/polymorphic/indexed-db/element.js";

import { computed, signal } from "~/common/signal.js";
import { OutputTransformer } from "../../base.js";
import { collectionSchema } from "~/common/self-describing.js";
import { defineElement } from "~/common/element.js";

import { COLLECTIONS, IDB_PREFIX, MAX_PACKS } from "./constants.js";
import {
  adoptDocument,
  applyPack,
  changeDocument,
  changesSince,
  createDocument,
  encodeDocument,
  headsOf,
  loadDocument,
  mergeDocument,
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
 * The extra surface an append-only output exposes (see
 * `@specs/components/output/bytes/firkin/types.d.ts`). Detect it at runtime so
 * the same transformer also works over whole-value outputs like S3/Dropbox.
 *
 * @typedef {Object} AppendCapable
 * @property {true} appends
 * @property {(name: string) => Promise<string[]>} entryCids
 * @property {(name: string, cids?: string[]) => Promise<import("@specs/components/output/bytes/firkin/types.d.ts").FirkinEntry[]>} packs
 * @property {(name: string) => Promise<number>} packCount
 * @property {(name: string, snapshot: Uint8Array) => Promise<void>} compact
 */

/**
 * Stores each collection as an Automerge document.
 *
 * Two things matter for multi-device use:
 *
 * - Records live in a map keyed by `id` (see `doc.js`), so concurrent edits to
 *   different records merge instead of one device's collection winning wholesale.
 * - When the backing output appends (the Firkin log), only the changes since
 *   the last push are written — an edit to one track is a few hundred bytes,
 *   not the whole document.
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

  // STATE

  /** @type {Map<string, import("./doc.js").AutomergeDoc>} */
  #docs = new Map();

  /** @type {Map<string, Heads | undefined>} */
  #pushed = new Map();

  /** Collections whose last push failed (offline, signed out, …). */
  #dirty = new Set();

  /**
   * Log entries already applied, per collection. Persisted so a page load only
   * fetches what was appended since the last one.
   *
   * @type {Map<string, Set<string>>}
   */
  #seen = new Map();

  /** @type {Promise<void> | undefined} */
  #pulling;

  /**
   * Serializes document mutations. Without this a pull that started before a
   * save would overwrite the document the save just wrote.
   */
  #queue = Promise.resolve();

  /** @type {Promise<void> | undefined} */
  #initialising;

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

    this.ready = computed(() => this.#loaded.value);

    // Pull once the backing output is usable (signed in, online, …). Retries
    // whenever that flips back to true after a failure.
    this.effect(() => {
      if (!this.base().ready()) return;
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
    if (!local) throw new Error("Can't find local output");

    customElements.whenDefined(local.localName).then(() => {
      this.#localOutput.value = local;
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
      const local = this.#localOutput.value;

      if (local) {
        await Promise.all(COLLECTIONS.map(async (name) => {
          try {
            await local[name].reload();

            const collection = local[name].collection();
            if (collection.state !== "loaded") return;

            const doc = loadDocument(collection.data);
            if (doc) {
              this.#docs.set(name, doc);
            } else {
              // Nothing cached: start from an empty document of our own, under
              // this device's actor so concurrent writers never share a
              // sequence number.
              this.#docs.set(name, createDocument(await this.#actor()));

              // No local copy means nothing has been applied yet, whatever the
              // cursor says.
              await this.#resetSeen(name);
            }
          } catch (err) {
            console.warn(`Could not read the local '${name}' document:`, err);
          }
        }));
      }

      this.#loaded.value = true;
      this.#version.value++;
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

      await this.#push(name, doc);
    });
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
   * @param {{ full?: boolean }} [options]
   */
  async #push(name, doc, options = {}) {
    // Without the backing output there is nothing to push to yet; wait for it
    // so an early save isn't silently dropped.
    await this.output.whenDefined;

    /** @type {AppendCapable | undefined} */
    const element = /** @type {any} */ (this.output.signal());
    const appends = element?.appends === true;

    const pushedHeads = await this.#pushedHeads(name);
    const bytes = appends && !options.full
      ? changesSince(doc, pushedHeads)
      : encodeDocument(doc);

    if (bytes.length === 0) return;

    try {
      await this.base()[name].save(bytes);
    } catch (err) {
      this.#dirty.add(name);
      console.warn(`Could not push the '${name}' changes:`, err);
      return;
    }

    this.#dirty.delete(name);

    if (!appends) return;

    await this.#setPushedHeads(name, headsOf(doc));
    await this.#compactIfNeeded(name, doc, element);
  }

  /**
   * Pull every collection. Serialized: concurrent calls share one pass.
   */
  #pull() {
    this.#pulling ??= (async () => {
      await this.#initialise();

      try {
        for (const name of COLLECTIONS) {
          try {
            await this.#pullCollection(name);
          } catch (err) {
            console.warn(`Could not pull the '${name}' document:`, err);
          }
        }
      } finally {
        this.#pulling = undefined;
      }
    })();

    return this.#pulling;
  }

  /**
   * @param {CollectionName} name
   */
  async #pullCollection(name) {
    await this.#initialise();

    await this.#serialize(async () => {
      const remote = this.base();
      if (!remote.ready()) return;

      /** @type {AppendCapable | undefined} */
      const element = /** @type {any} */ (this.output.signal());
      const appends = element?.appends === true &&
        typeof element.entryCids === "function" &&
        typeof element.packs === "function";

      let doc = this.#docs.get(name) ?? createDocument();
      const before = headsOf(doc).join(";");

      let complete = true;
      let empty = false;
      let applied = 0;

      if (appends) {
        const cids = await element.entryCids(name);
        empty = cids.length === 0;

        // Only ask for entries this device hasn't applied yet. The cursor is
        // persisted, so a reload costs one manifest read per writer instead of
        // downloading the whole history again.
        const seen = await this.#seenCids(name);
        const unseen = cids.filter((cid) => !seen.has(cid));
        const entries = unseen.length > 0
          ? await /** @type {AppendCapable} */ (element).packs(name, unseen)
          : [];

        // With nothing of our own, adopt the shared document instead of
        // merging into the empty one we started with: independently created
        // documents keep separate roots, and later changes written against the
        // other root would stay invisible here.
        let pending = [...entries];
        if (recordsOf(doc).length === 0) {
          for (const entry of [...pending].reverse()) {
            try {
              const adopted = adoptDocument(entry.bytes, await this.#actor());
              if (recordsOf(adopted).length === 0) continue;

              doc = adopted;
              pending = pending.filter((item) => item !== entry);
              break;
            } catch {
              // Not a document; a change chunk needs the rest of the log first.
            }
          }
        }

        // Apply in dependency order: an entry may reference changes written by
        // another device that comes later in the log.
        let progress = true;

        while (pending.length > 0 && progress) {
          progress = false;

          for (let i = 0; i < pending.length; i++) {
            try {
              doc = applyPack(doc, pending[i].bytes);
              pending.splice(i, 1);
              i--;
              progress = true;
            } catch {
              // Missing dependencies; try again after the other entries.
            }
          }
        }

        applied = entries.length - pending.length;
        complete = pending.length === 0;

        // Record only what landed, and drop CIDs that left the log (compaction
        // or a manifest gone missing) so the cursor stays bounded.
        await this.#recordSeen(
          name,
          pending.length === 0
            ? entries.map((entry) => entry.cid)
            : entries.filter((entry) => !pending.includes(entry)).map((entry) =>
              entry.cid
            ),
          cids,
        );

        if (!complete) {
          console.warn(
            `Waiting on ${pending.length} '${name}' log entr${
              pending.length === 1 ? "y" : "ies"
            } with missing changes`,
          );
        }
      } else {
        // Whole-value backing output: merge the remote document in.
        await remote[name].reload();
        const collection = remote[name].collection();

        if (collection.state === "loaded") {
          if (collection.data instanceof Uint8Array) {
            try {
              doc = mergeDocument(doc, collection.data);
            } catch (err) {
              console.warn(`Could not merge the remote '${name}' document:`, err);
            }
          } else if (collection.data === undefined) {
            empty = true;
          }
        }
      }

      // Always keep the handle `applyPack` returned: applying an entry that is
      // already in the document still produces a new handle and marks the old
      // one outdated, so keeping the old one breaks every later pull.
      this.#docs.set(name, doc);

      if (headsOf(doc).join(";") !== before) {
        this.#version.value++;

        const local = this.#localOutput.value;
        if (local) {
          try {
            await local[name].save(encodeDocument(doc));
          } catch (err) {
            console.warn(`Could not cache the '${name}' document locally:`, err);
          }
        }
      }

      // A complete pull means the remote log already holds everything we know
      // about, so move the push cursor up to it: the next save then appends
      // only what changed after this point instead of re-uploading the entries
      // we just pulled.
      if (appends && applied > 0 && complete && !this.#dirty.has(name)) {
        await this.#setPushedHeads(name, headsOf(doc));
      }

      // A log with no entries means the remote has nothing yet: publish what we
      // have locally so the next device can start from it.
      if (empty && recordsOf(doc).length > 0) {
        await this.#push(name, doc, { full: true });
      } else if (complete && this.#dirty.has(name)) {
        await this.#push(name, doc);
      }
    });
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

  // PULL CURSOR

  /** @param {CollectionName} name */
  #seenKey(name) {
    return `${IDB_PREFIX}/${this.namespace ?? "default"}/${name}/seen`;
  }

  /**
   * CIDs of the log entries this device has already applied.
   *
   * @param {CollectionName} name
   * @returns {Promise<Set<string>>}
   */
  async #seenCids(name) {
    const cached = this.#seen.get(name);
    if (cached) return cached;

    /** @type {Set<string>} */
    let seen = new Set();
    try {
      const stored = await IDB.get(this.#seenKey(name));
      if (Array.isArray(stored)) {
        seen = new Set(stored.filter((cid) => typeof cid === "string"));
      }
    } catch (err) {
      console.warn(`Could not read the '${name}' pull cursor:`, err);
    }

    this.#seen.set(name, seen);

    return seen;
  }

  /**
   * Remember which entries landed, and forget ones that left the log.
   *
   * @param {CollectionName} name
   * @param {string[]} applied
   * @param {string[]} current - Every CID currently in the log.
   */
  async #recordSeen(name, applied, current) {
    const seen = await this.#seenCids(name);
    for (const cid of applied) seen.add(cid);

    const keep = new Set(current);
    for (const cid of [...seen]) {
      if (!keep.has(cid)) seen.delete(cid);
    }

    try {
      await IDB.set(this.#seenKey(name), [...seen]);
    } catch (err) {
      console.warn(`Could not persist the '${name}' pull cursor:`, err);
    }
  }

  /** @param {CollectionName} name */
  async #resetSeen(name) {
    this.#seen.set(name, new Set());

    try {
      await IDB.del(this.#seenKey(name));
    } catch (err) {
      console.warn(`Could not reset the '${name}' pull cursor:`, err);
    }
  }

  // PUSH CURSOR

  /** @param {CollectionName} name */
  #pushedKey(name) {
    return `${IDB_PREFIX}/${this.namespace ?? "default"}/${name}/pushed`;
  }

  /**
   * The heads the remote already has for this collection.
   *
   * @param {CollectionName} name
   * @returns {Promise<Heads | undefined>}
   */
  async #pushedHeads(name) {
    if (this.#pushed.has(name)) return this.#pushed.get(name);

    /** @type {Heads | undefined} */
    let heads;
    try {
      const stored = await IDB.get(this.#pushedKey(name));
      if (
        Array.isArray(stored) &&
        stored.every((hash) => typeof hash === "string")
      ) {
        heads = stored;
      }
    } catch (err) {
      console.warn(`Could not read the '${name}' push cursor:`, err);
    }

    this.#pushed.set(name, heads);

    return heads;
  }

  /**
   * @param {CollectionName} name
   * @param {Heads} heads
   */
  async #setPushedHeads(name, heads) {
    this.#pushed.set(name, heads);

    try {
      await IDB.set(this.#pushedKey(name), heads);
    } catch (err) {
      console.warn(`Could not persist the '${name}' push cursor:`, err);
    }
  }

  /**
   * Collapse a long log into one snapshot once it passes `MAX_PACKS`.
   *
   * @param {CollectionName} name
   * @param {import("./doc.js").AutomergeDoc} doc
   * @param {AppendCapable} element
   */
  async #compactIfNeeded(name, doc, element) {
    if (typeof element.packCount !== "function") return;
    if (typeof element.compact !== "function") return;

    try {
      if (await element.packCount(name) <= MAX_PACKS) return;

      await element.compact(name, encodeDocument(doc));
    } catch (err) {
      console.warn(`Could not compact the '${name}' log:`, err);
    }
  }
}

export default AutomergeBytesOutputTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = AutomergeBytesOutputTransformer;
export const NAME = "dtob-automerge";

defineElement(NAME, CLASS);
