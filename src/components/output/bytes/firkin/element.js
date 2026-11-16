import { ApiError } from "@tokono.ma/firkin";

import { computed, signal } from "~/common/signal.js";
import { BroadcastedOutputElement, outputManager } from "../../common.js";
import { defineElement } from "~/common/element.js";
import { getSessionClient } from "~/common/firkin/session.js";

import { NOT_CONNECTED, readDeviceId } from "./constants.js";
import {
  appendEntry,
  countEntries,
  latestEntry,
  readEntries,
  replaceLog,
} from "./log.js";

/**
 * @import {OutputElement, OutputManager} from "@specs/components/output/types.d.ts"
 * @import {FirkinOutputElement} from "@specs/components/output/bytes/firkin/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * An **append-only** value store on Firkin: every `put` appends one immutable
 * blob, and `get` returns the most recently appended one.
 *
 * That makes it usable two ways:
 *
 * - as a plain value store (pair it with any transformer that expects whole
 *   values, like `dtob-dasl-sync` or `dtob-json`) that happens to keep history;
 * - as a log, by reading `packs(name)` and replaying the entries — what
 *   `dtob-automerge` does, so a save only uploads the changes since the last
 *   push instead of a whole document.
 *
 * Set `keep="1"` when a value-shaped transformer is the only reader: the log
 * then drops everything but the newest entry, which keeps the per-device
 * manifest bounded. Without it, history is retained.
 *
 * @implements {OutputElement<Uint8Array | undefined>}
 * @implements {FirkinOutputElement}
 */
class FirkinOutput extends BroadcastedOutputElement {
  static NAME = "diffuse/output/bytes/firkin";

  /** Signals that `put` appends instead of replacing (see the class docs). */
  /** @type {true} */
  appends = true;

  #manager;

  // SIGNALS

  #isOnline = signal(navigator.onLine);
  #connected = signal(false);

  // LOG

  /** @type {Map<string, Uint8Array>} */
  #packs = new Map();

  /** @type {Map<string, Promise<unknown>>} */
  #writes = new Map();

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

    super.disconnectedCallback();
  }

  #offline = () => this.#isOnline.set(false);
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

  // LOG

  /**
   * CIDs of every entry for `name`, oldest first.
   *
   * Reads manifests only, so asking "what is new?" costs one request per
   * writer rather than one per entry.
   *
   * @param {string} name
   * @returns {Promise<string[]>}
   */
  async entryCids(name) {
    const client = await getSessionClient();
    if (!client) return [];

    const entries = await readEntries(client.rb, this.#namespace, name);

    return entries.map((entry) => entry.cid);
  }

  /**
   * Entry bytes for `name`, oldest first. Pass `cids` to fetch only those; the
   * result keeps log order and skips entries whose blob is gone.
   *
   * @param {string} name
   * @param {string[]} [cids]
   * @returns {Promise<import("@specs/components/output/bytes/firkin/types.d.ts").FirkinEntry[]>}
   */
  async packs(name, cids) {
    const client = await getSessionClient();
    if (!client) return [];

    const wanted = cids ? new Set(cids) : undefined;
    const entries = (await readEntries(client.rb, this.#namespace, name))
      .filter((entry) => !wanted || wanted.has(entry.cid));

    const bytes = await Promise.all(
      entries.map((entry) => this.#readPack(client.rb, entry.cid)),
    );

    return entries.flatMap((entry, index) =>
      bytes[index] ? [{ cid: entry.cid, bytes: bytes[index] }] : []
    );
  }

  /**
   * How many entries this browser has appended for `name`.
   *
   * @param {string} name
   */
  async packCount(name) {
    const client = await getSessionClient();
    if (!client) return 0;

    return await countEntries(
      client.rb,
      this.#namespace,
      name,
      await readDeviceId(),
    );
  }

  /**
   * Replace this browser's entries for `name` with a single snapshot.
   *
   * @param {string} name
   * @param {Uint8Array} snapshot
   */
  async compact(name, snapshot) {
    const client = await getSessionClient();
    if (!client) throw new Error(NOT_CONNECTED);

    const cid = await replaceLog(
      client.rb,
      this.#namespace,
      name,
      await readDeviceId(),
      snapshot,
    );

    this.#packs.set(cid, snapshot);
  }

  /**
   * @param {import("@tokono.ma/firkin").Firkin} rb
   * @param {string} cid
   */
  async #readPack(rb, cid) {
    const cached = this.#packs.get(cid);
    if (cached) return cached;

    try {
      const bytes = await rb.get(cid);
      this.#packs.set(cid, bytes);
      return bytes;
    } catch (err) {
      // A missing block makes for a shorter log; the reader decides whether
      // what remains is enough.
      if (err instanceof ApiError && err.status === 404) return undefined;
      throw err;
    }
  }

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

  // GET & PUT

  /**
   * The newest entry for `name`.
   *
   * @param {string} name
   * @returns {Promise<Uint8Array | undefined>}
   */
  #get = async (name) => {
    const client = await getSessionClient();
    if (!client) return undefined;

    const entry = await latestEntry(client.rb, this.#namespace, name);
    if (!entry) return undefined;

    return await this.#readPack(client.rb, entry.cid);
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
        await readDeviceId(),
        data,
        { keep: this.keep },
      );

      this.#packs.set(cid, data);
    };

    // Serialize appends per log so concurrent saves can't rewrite the same
    // manifest from a stale read.
    const previous = this.#writes.get(name) ?? Promise.resolve();
    const written = previous.then(run, run);
    this.#writes.set(name, written.then(() => {}, () => {}));

    return written;
  };
}

export default FirkinOutput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = FirkinOutput;
export const NAME = "dob-firkin";

defineElement(NAME, FirkinOutput);
