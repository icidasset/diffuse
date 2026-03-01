import { Client, ClientResponseError, ok } from "@atcute/client";
import * as TID from "@atcute/tid";

import { computed, signal } from "@common/signal.js";
import { BroadcastedOutputElement, outputManager } from "../../common.js";

import {
  clearStoredSession,
  login,
  logout,
  OAuthUserAgent,
  restoreOrFinalize,
  TokenRefreshError,
} from "./oauth.js";

/**
 * @import {PlaylistItemBundle, TrackBundle} from "@definitions/types.d.ts"
 * @import {OutputManager} from "../../types.d.ts"
 * @import {ATProtoOutputElement} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ATProtoOutputElement}
 */
class ATProtoOutput extends BroadcastedOutputElement {
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
        get: () => this.listRecords("sh.diffuse.output.facet"),
        put: (data) => this.#putRecords("sh.diffuse.output.facet", data),
      },
      playlistItems: {
        empty: () => [],
        get: async () => {
          const bundles = await this.listRecords(
            "sh.diffuse.output.playlistItemBundle",
          );

          return bundles.flatMap((bundle) => bundle.playlistItems ?? []);
        },
        put: (data) => {
          /** @type {PlaylistItemBundle[]} */
          const bundles = [];

          for (let i = 0; i < data.length; i += 100) {
            bundles.push({
              $type: "sh.diffuse.output.playlistItemBundle",
              id: TID.now(),
              playlistItems: data.slice(i, i + 100),
            });
          }

          return this.#putRecords(
            "sh.diffuse.output.playlistItemBundle",
            bundles,
            { upsertBatchSize: 1 },
          );
        },
      },
      themes: {
        empty: () => [],
        get: () => this.listRecords("sh.diffuse.output.theme"),
        put: (data) => this.#putRecords("sh.diffuse.output.theme", data),
      },
      tracks: {
        empty: () => [],
        get: async () => {
          const bundles = await this.listRecords(
            "sh.diffuse.output.trackBundle",
          );

          return bundles.flatMap((bundle) => bundle.tracks ?? []);
        },
        put: (data) => {
          /** @type {TrackBundle[]} */
          const bundles = [];

          for (let i = 0; i < data.length; i += 100) {
            bundles.push({
              $type: "sh.diffuse.output.trackBundle",
              id: TID.now(),
              tracks: data.slice(i, i + 100),
            });
          }

          return this.#putRecords("sh.diffuse.output.trackBundle", bundles, {
            upsertBatchSize: 1,
          });
        },
      },
    });

    this.facets = this.#manager.facets;
    this.playlistItems = this.#manager.playlistItems;
    this.themes = this.#manager.themes;
    this.tracks = this.#manager.tracks;
  }

  // SIGNALS

  #did = signal(/** @type {string | null} */ (null));
  #isOnline = signal(navigator.onLine);
  #rev = signal(/** @type {string | null} */ (null));

  did = this.#did.get;
  rev = this.#rev.get;

  ready = computed(() => {
    return this.#did.value !== null && !!this.#rpc && this.#isOnline.value;
  });

  // LIFECYCLE

  /** @override */
  connectedCallback() {
    this.replicateSavedData(this.#manager);

    super.connectedCallback();

    this.#tryRestore();

    globalThis.addEventListener("online", this.#online);
    globalThis.addEventListener("offline", this.#offline);
  }

  /** @override */
  disconnectedCallback() {
    globalThis.removeEventListener("online", this.#online);
    globalThis.removeEventListener("offline", this.#offline);
  }

  #offline = () => this.#isOnline.set(false);
  #online = () => this.#isOnline.set(true);

  // AUTH

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

  /**
   * Clear session state without contacting the server.
   * Used when the session has already been revoked.
   */
  #clearSession() {
    this.#agent = null;
    this.#authenticated = Promise.withResolvers();
    this.#did.value = null;
    this.#rpc = null;

    clearStoredSession();
  }

  /**
   * @param {unknown} err
   * @returns {boolean}
   */
  #isSessionError(err) {
    if (err instanceof TokenRefreshError) return true;
    // OAuthUserAgent.handle() swallows TokenRefreshError and returns the
    // original 401 response, which ok() wraps as a ClientResponseError.
    if (err instanceof ClientResponseError && err.status === 401) return true;
    if (err && typeof err === "object" && "cause" in err) {
      return this.#isSessionError(/** @type {any} */ (err).cause);
    }
    return false;
  }

  async #tryRestore() {
    await this.whenConnected();

    try {
      const session = await restoreOrFinalize();

      if (session) {
        this.#setSession(session);
      }
    } catch (err) {
      if (this.#isSessionError(err)) {
        this.#clearSession();
      } else {
        throw err;
      }
    }
  }

  /**
   * @param {import("@atcute/oauth-browser-client").Session} session
   */
  #setSession(session) {
    const agent = new OAuthUserAgent(session);

    // Intercept token refresh to detect session revocation proactively.
    // OAuthUserAgent.handle() swallows TokenRefreshError silently,
    // so we hook into getSession to clear state as soon as refresh fails.
    const originalGetSession = agent.getSession.bind(agent);
    agent.getSession = /** @param {any[]} args */ (...args) => {
      const promise = originalGetSession(...args);

      promise.catch((err) => {
        if (err instanceof TokenRefreshError) {
          this.#clearSession();
        }
      });

      return promise;
    };

    this.#agent = agent;
    this.#rpc = new Client({ handler: agent });
    this.#did.value = session.info.sub;
    this.#authenticated.resolve();
  }

  // RECORDS

  /**
   * Fetch the latest commit rev for this repo.
   * Returns `null` if not authenticated or on error.
   *
   * @returns {Promise<string | null>}
   */
  async getLatestCommit() {
    const did = this.#did.value;

    const rpc = this.#rpc;
    if (!rpc || !did) return null;

    try {
      const result = await ok(rpc.get(
        "com.atproto.sync.getLatestCommit",
        { params: { did } },
      ));

      this.#rev.value = result?.rev;
      return result?.rev;
    } catch (err) {
      if (this.#isSessionError(err)) {
        this.#clearSession();
        return null;
      }

      throw err;
    }
  }

  /**
   * @template T
   * @param {string} collection
   * @param {string} [did]
   * @returns {Promise<T[]>}
   */
  async listRecords(collection, did) {
    did ??= this.#did.value ?? undefined;

    const rpc = this.#rpc;
    if (!rpc || !did) return [];

    try {
      const records = [];

      /** @type {any} */
      let cursor;

      do {
        const page = await ok(rpc.get(
          "com.atproto.repo.listRecords",
          { params: { repo: did, collection, limit: 100, cursor } },
        ));

        for (const record of (page?.records ?? [])) {
          records.push(record.value);
        }

        cursor = page?.cursor;
      } while (cursor);

      return records;
    } catch (err) {
      if (this.#isSessionError(err)) {
        this.#clearSession();
        return [];
      }

      throw err;
    }
  }

  /**
   * @param {string} collection
   * @param {Array<{ id: string }>} data
   * @param {{ deleteBatchSize?: number, upsertBatchSize?: number }} [options]
   */
  async #putRecords(collection, data, { deleteBatchSize = 100, upsertBatchSize = deleteBatchSize } = {}) {
    const rpc = this.#rpc;
    if (!rpc || !this.#did.value) return;

    try {
      // 1. Fetch current state
      /** @type {Map<string, { rkey: string, value: unknown }>} */
      const existing = new Map();

      /** @type {any} */
      let cursor;

      do {
        const page = await ok(rpc.get(
          "com.atproto.repo.listRecords",
          {
            params: { repo: this.#did.value, collection, limit: 100, cursor },
          },
        ));

        for (const record of (page?.records ?? [])) {
          const rkey = record.uri.split("/").pop();
          existing.set(record.value.id, { rkey, value: record.value });
        }

        cursor = page?.cursor;
      } while (cursor);

      // 2. Build desired state
      const desired = new Map(
        data.map((record) => [record.id, { $type: collection, ...record }]),
      );

      // 3. Compute diff
      /** @type {unknown[]} */
      const deletes = [];

      /** @type {unknown[]} */
      const upserts = [];

      for (const [id, { rkey }] of existing) {
        if (!desired.has(id)) {
          deletes.push({
            $type: "com.atproto.repo.applyWrites#delete",
            collection,
            rkey,
          });
        }
      }

      for (const [id, record] of desired) {
        const entry = existing.get(id);

        if (!entry) {
          upserts.push({
            $type: "com.atproto.repo.applyWrites#create",
            collection,
            rkey: id,
            value: record,
          });
        } else if (JSON.stringify(entry.value) !== JSON.stringify(record)) {
          upserts.push({
            $type: "com.atproto.repo.applyWrites#update",
            collection,
            rkey: entry.rkey,
            value: record,
          });
        }
      }

      // 4. Apply in batches
      const applyBatch = async (/** @type {unknown[]} */ batch) => {
        const result = await ok(rpc.post("com.atproto.repo.applyWrites", {
          input: { repo: this.#did.value, writes: batch },
        }));

        if (result?.commit?.rev) {
          this.#rev.value = result.commit.rev;
        }
      };

      for (let i = 0; i < deletes.length; i += deleteBatchSize) {
        await applyBatch(deletes.slice(i, i + deleteBatchSize));
      }

      for (let i = 0; i < upserts.length; i += upsertBatchSize) {
        await applyBatch(upserts.slice(i, i + upsertBatchSize));
      }
    } catch (err) {
      if (this.#isSessionError(err)) {
        this.#clearSession();
        return;
      }

      throw err;
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
