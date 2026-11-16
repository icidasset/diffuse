import { ApiError, bytesToHex, WebCryptoSigner } from "@tokono.ma/firkin";
import * as IDB from "idb-keyval";

import { computed, signal } from "~/common/signal.js";
import { BroadcastedOutputElement, outputManager } from "../../common.js";
import { defineElement } from "~/common/element.js";
import {
  HINT_DEBOUNCE_MS,
  channelName,
  openHintChannel,
} from "~/common/firkin/channel.js";
import { getSessionClient } from "~/common/firkin/session.js";

import {
  IDB_FLOORS,
  IDB_PACKS,
  NAMES_TTL_MS,
  NOT_CONNECTED,
} from "./constants.js";
import { appendEntry, readEntries } from "./log.js";
import { verifySignature } from "./verify.js";

/**
 * @import {OutputElement} from "@specs/components/output/types.d.ts"
 * @import {FirkinOutputElement} from "@specs/components/output/bytes/firkin/types.d.ts"
 * @import {Session} from "~/common/firkin/session.js"
 * @import {LogAuth} from "./log.js"
 * @import {Firkin} from "@tokono.ma/firkin"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * An **append-only** store on Firkin: every `put` appends one immutable blob.
 *
 * What `get` returns is the element's `mode`:
 *
 * - `mode="value"` (default) — the most recently appended entry. Pair it with
 *   a whole-value transformer (`dtob-dasl-sync`, `dtob-json`); `keep="1"` keeps
 *   the per-device manifest bounded.
 * - `mode="log"` — the concatenation of every entry, oldest first. Pair it with
 *   a transformer whose format is a sequence of self-delimiting chunks
 *   (`dtob-automerge`): appending a delta is a small write, and replaying the
 *   whole log reconstructs the value.
 *
 * Either way the element exposes the plain `OutputElement<Uint8Array>` surface,
 * so no consumer checks a Firkin-specific capability. Choosing a `mode` is
 * configuration, not a dependency.
 *
 * Each manifest is signed by the session key it is filed under, so a client
 * holding the account's `/firkin` capability can't pass its writes off as
 * another device's — it can only deny service on a ref, which that device
 * repairs on its next write.
 *
 * @implements {OutputElement<Uint8Array | undefined>}
 * @implements {FirkinOutputElement}
 */
class FirkinOutput extends BroadcastedOutputElement {
  static NAME = "diffuse/output/bytes/firkin";

  #manager;

  // SIGNALS

  #isOnline = signal(navigator.onLine);
  #connected = signal(false);

  // LOG

  /** @type {Map<string, Uint8Array>} */
  #packs = new Map();

  /** @type {Map<string, Promise<unknown>>} */
  #writes = new Map();

  // HINTS

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

  constructor() {
    super();

    this.#manager = outputManager({
      collaborations: {
        empty: () => undefined,
        get: () => this.#get("collaborations"),
        put: (data) => this.#put("collaborations", data),
      },
      facets: {
        empty: () => undefined,
        get: () => this.#get("facets"),
        put: (data) => this.#put("facets", data),
      },
      init: () => this.whenConnected(),
      playlistItems: {
        empty: () => undefined,
        get: () => this.#get("playlistItems"),
        put: (data) => this.#put("playlistItems", data),
      },
      settings: {
        empty: () => undefined,
        get: () => this.#get("settings"),
        put: (data) => this.#put("settings", data),
      },
      tracks: {
        empty: () => undefined,
        get: () => this.#get("tracks"),
        put: (data) => this.#put("tracks", data),
      },
    });

    this.collaborations = this.#manager.collaborations;
    this.facets = this.#manager.facets;
    this.playlistItems = this.#manager.playlistItems;
    this.settings = this.#manager.settings;
    this.tracks = this.#manager.tracks;
  }

  ready = computed(() => this.#connected.value && this.#isOnline.value);

  // LIFECYCLE

  /** @override */
  connectedCallback() {
    this.replicateSavedData(this.#manager);

    super.connectedCallback();

    globalThis.addEventListener("online", this.#online);
    globalThis.addEventListener("offline", this.#offline);

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
    const reload = this.#reload(collection);
    if (!reload) return;

    const pending = this.#hints.get(collection);
    if (pending !== undefined) clearTimeout(pending);

    this.#hints.set(
      collection,
      setTimeout(() => {
        this.#hints.delete(collection);

        // Another device wrote something: don't trust the cached listing.
        this.#listings.clear();

        void reload().catch((err) => {
          console.warn(`Could not refresh '${collection}' after a hint:`, err);
        });
      }, HINT_DEBOUNCE_MS),
    );
  }

  /**
   * @param {string} collection
   * @returns {(() => Promise<void>) | undefined}
   */
  #reload(collection) {
    const manager = /** @type {any} */ (this.#manager);

    return manager[collection]?.reload;
  }

  // CONFIGURATION

  get #namespace() {
    return this.namespace ?? "default";
  }

  /** `"value"` returns the newest entry; `"log"` returns every entry. */
  get mode() {
    return this.getAttribute("mode") === "log" ? "log" : "value";
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

  // LOG

  /**
   * Every entry for `name`, oldest first.
   *
   * @param {string} name
   */
  async packs(name) {
    const client = await getSessionClient();
    if (!client) return [];

    const entries = await this.#entries(client, name);
    const bytes = await Promise.all(
      entries.map((entry) => this.#readPack(client.rb, entry.cid)),
    );

    return entries.flatMap((entry, index) =>
      bytes[index] ? [{ cid: entry.cid, bytes: bytes[index] }] : []
    );
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

  /** @type {{ publicKey: Uint8Array; auth: LogAuth } | undefined} */
  #authCache;

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

  /** @type {Map<string, Map<string, number>>} */
  #floorCache = new Map();

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
   * @param {string} name
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

  // GET & PUT

  /**
   * @param {string} name
   * @returns {Promise<Uint8Array | undefined>}
   */
  #get = async (name) => {
    const client = await getSessionClient();
    if (!client) return undefined;

    const entries = await this.#entries(client, name);
    if (entries.length === 0) return undefined;

    if (this.mode === "value") {
      const newest = entries.at(-1);
      if (!newest) return undefined;

      return await this.#readPack(client.rb, newest.cid);
    }

    const bytes = await Promise.all(
      entries.map((entry) => this.#readPack(client.rb, entry.cid)),
    );
    const present = /** @type {Uint8Array[]} */ (bytes.filter(Boolean));

    return present.length === 0 ? undefined : concat(present);
  };

  /**
   * @param {string} name
   * @param {Uint8Array | undefined} data
   * @returns {Promise<void>}
   */
  #put = (name, data) => {
    const run = async () => {
      const client = await getSessionClient();
      if (!client) throw new Error(NOT_CONNECTED);
      if (!data || data.length === 0) return;

      const { cid } = await appendEntry(
        client.rb,
        this.#namespace,
        name,
        data,
        {
          auth: this.#auth(client.session),
          floors: await this.#floors(name),
          keep: this.keep,
        },
      );

      this.#packs.set(cid, data);

      try {
        await IDB.set(`${IDB_PACKS}/${cid}`, data);
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
  };
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

export default FirkinOutput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = FirkinOutput;
export const NAME = "dob-firkin";

defineElement(NAME, FirkinOutput);
