import { Client, ok } from "@atcute/client";
import { BroadcastableDiffuseElement } from "@common/element.js";
import { computed, signal } from "@common/signal.js";
import { outputManager } from "../../common.js";
import { login, logout, OAuthUserAgent, restoreOrFinalize } from "./oauth.js";

/**
 * @import {Signal} from "@common/signal.d.ts"
 * @import {OutputManager} from "../../types.d.ts"
 * @import {ATProtoOutputElement} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ATProtoOutputElement}
 */
class ATProtoOutput extends BroadcastableDiffuseElement {
  static NAME = "diffuse/output/raw/atproto";

  #manager;

  /** @type {PromiseWithResolvers<void>} */
  #authenticated = Promise.withResolvers();

  /** @type {Client | null} */
  #rpc = null;

  /** @type {OAuthUserAgent | null} */
  #agent = null;

  constructor() {
    super();

    /** @type {OutputManager} */
    this.#manager = outputManager({
      facets: {
        empty: () => [],
        get: () => this.#listRecords("sh.diffuse.output.facet"),
        put: (data) => this.#putRecords("sh.diffuse.output.facet", data),
      },
      playlists: {
        empty: () => [],
        get: () => this.#listRecords("sh.diffuse.output.playlist"),
        put: (data) => this.#putRecords("sh.diffuse.output.playlist", data),
      },
      themes: {
        empty: () => [],
        get: () => this.#listRecords("sh.diffuse.output.theme"),
        put: (data) => this.#putRecords("sh.diffuse.output.theme", data),
      },
      tracks: {
        empty: () => [],
        get: () => this.#listRecords("sh.diffuse.output.track"),
        put: (data) => this.#putRecords("sh.diffuse.output.track", data),
      },
    });

    this.facets = this.#manager.facets;
    this.playlists = this.#manager.playlists;
    this.themes = this.#manager.themes;
    this.tracks = this.#manager.tracks;
  }

  // SIGNALS

  #did = signal(/** @type {string | null} */ (null));

  // STATE

  did = this.#did.get;

  ready = computed(() => {
    return this.#did.value !== null && navigator.onLine
  });

  // LIFECYCLE

  /** @override */
  connectedCallback() {
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(this.nameWithGroup, {
        put: { strategy: "replicate", fn: this.#putIncoming },
      });

      if (actions) {
        this.#put = this.#putOutgoing(actions.put);
      }
    }

    super.connectedCallback();

    this.#tryRestore();
  }

  // AUTH

  async #tryRestore() {
    await this.whenConnected();

    const session = await restoreOrFinalize();

    if (session) {
      this.#setSession(session);
    }
  }

  /**
   * @param {import("@atcute/oauth-browser-client").Session} session
   */
  #setSession(session) {
    this.#agent = new OAuthUserAgent(session);
    this.#rpc = new Client({ handler: this.#agent });
    this.#did.value = session.info.sub;
    this.#authenticated.resolve();
  }

  /**
   * Initiate the OAuth flow.
   * Navigates the browser to the authorization server.
   *
   * @param {string} handle
   */
  async login(handle) {
    await login(handle);
  }

  /**
   * Sign out and revoke the current session.
   */
  async logout() {
    if (this.#agent) {
      await logout(this.#agent);
      this.#agent = null;
      this.#authenticated = Promise.withResolvers();
      this.#did.value = null;
      this.#rpc = null;
    }
  }

  // RECORDS

  /**
   * @template T
   * @param {string} collection
   * @returns {Promise<T[]>}
   */
  async #listRecords(collection) {
    if (!this.#rpc || !this.#did.value) return [];

    const records = [];
    let cursor;

    do {
      /** @type {any} */
      const page = await ok(this.#rpc.get(
        "com.atproto.repo.listRecords",
        { params: { repo: this.#did.value, collection, limit: 100, cursor } },
      ));

      for (const record of page.records) {
        records.push(record.value);
      }

      cursor = page.cursor;
    } while (cursor);

    return records;
  }

  /**
   * @param {string} collection
   * @param {Array<{ id: string }>} data
   */
  async #putRecordsSync(collection, data) {
    if (!this.#rpc || !this.#did.value) return;

    // 1. Fetch current state
    /** @type {Map<string, { rkey: string, value: unknown }>} */
    const existing = new Map();
    let cursor;

    do {
      /** @type {any} */
      const page = await ok(this.#rpc.get(
        "com.atproto.repo.listRecords",
        { params: { repo: this.#did.value, collection, limit: 100, cursor } },
      ));

      for (const record of page.records) {
        const rkey = record.uri.split("/").pop();
        existing.set(record.value.id, { rkey, value: record.value });
      }

      cursor = page.cursor;
    } while (cursor);

    // 2. Build desired state
    const desired = new Map(
      data.map((record) => [record.id, { $type: collection, ...record }]),
    );

    // 3. Compute diff
    /** @type {unknown[]} */
    const writes = [];

    for (const [id, { rkey }] of existing) {
      if (!desired.has(id)) {
        writes.push({
          $type: "com.atproto.repo.applyWrites#delete",
          collection,
          rkey,
        });
      }
    }

    for (const [id, record] of desired) {
      const entry = existing.get(id);

      if (!entry) {
        writes.push({
          $type: "com.atproto.repo.applyWrites#create",
          collection,
          rkey: id,
          value: record,
        });
      } else if (JSON.stringify(entry.value) !== JSON.stringify(record)) {
        writes.push({
          $type: "com.atproto.repo.applyWrites#update",
          collection,
          rkey: entry.rkey,
          value: record,
        });
      }
    }

    // 4. Apply
    if (writes.length > 0) {
      await this.#rpc.post("com.atproto.repo.applyWrites", {
        input: { repo: this.#did.value, writes },
      });
    }
  }

  // GET & PUT (broadcasting layer)

  /**
   * @param {string} collection
   * @param {Array<{ id: string }>} data
   */
  #putProxy = (collection, data) => this.#putRecordsSync(collection, data);
  #put = this.#putProxy;

  /**
   * @param {string} collection
   * @param {Array<{ id: string }>} data
   */
  #putRecords = (collection, data) => this.#put(collection, data);

  /**
   * @param {(uuidSender: ReturnType<typeof crypto.randomUUID>, collection: string, data: Array<{ id: string }>) => Promise<void>} action
   * @returns {(collection: string, data: Array<{ id: string }>) => Promise<void>}
   */
  #putOutgoing = (action) => async (collection, data) => {
    return await action(this.uuid, collection, data);
  };

  /**
   * @param {ReturnType<typeof crypto.randomUUID>} uuidSender
   * @param {string} collection
   * @param {Array<{ id: string }>} data
   */
  #putIncoming(uuidSender, collection, data) {
    if (uuidSender === this.uuid) {
      this.#putProxy(collection, data);
    } else {
      /** @type {Record<string, Signal<unknown[]>>} */
      const collectionMap = {
        "sh.diffuse.output.facet": this.#manager.signals.facets,
        "sh.diffuse.output.playlist": this.#manager.signals.playlists,
        "sh.diffuse.output.theme": this.#manager.signals.themes,
        "sh.diffuse.output.track": this.#manager.signals.tracks,
      };

      const sig = collectionMap[collection];
      if (sig) sig.value = data;
    }
  }
}

export default ATProtoOutput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ATProtoOutput;
export const NAME = "dor-atproto";

customElements.define(NAME, ATProtoOutput);
