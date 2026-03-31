import { Client, ClientResponseError, ok } from "@atcute/client";
import { ComAtprotoSyncSubscribeRepos } from "@atcute/atproto";
import { encode } from "@atcute/cbor";
import { xxh32r } from "xxh32/dist/raw.js";
import * as Repo from "@atcute/repo";
import * as IDB from "idb-keyval";

import { computed, signal } from "~/common/signal.js";
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
 * @import {Track, TrackBundle} from "~/definitions/types.d.ts"
 * @import {OutputManager} from "../../types.d.ts"
 * @import {ATProtoOutputElement} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

const WRITE_WINDOW_MS = 3_600_000;
const WRITE_RATE_LIMIT = 1500;
const WRITE_IDB_KEY = "diffuse/output/raw/atproto/writes";

/** @type {Set<string>} */
const WATCHED_COLLECTIONS = new Set([
  "sh.diffuse.output.facet",
  "sh.diffuse.output.playlistItem",
  "sh.diffuse.output.trackBundle",
]);

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

  /** @type {string | null} */
  #pdsUrl = null;

  /** @type {AsyncIterator<any> | null} */
  #firehoseIterator = null;
  #firehoseGen = 0;

  constructor() {
    super();

    /** @type {Track[] | null} */
    let lastPersistedTracks = null;

    /** @type {OutputManager} */
    this.#manager = outputManager({
      facets: {
        empty: () => [],
        get: () => this.listRecords("sh.diffuse.output.facet"),
        put: (data) => this.putRecords("sh.diffuse.output.facet", data),
      },
      playlistItems: {
        empty: () => [],
        get: () => this.listRecords("sh.diffuse.output.playlistItem"),
        put: (data) => this.putRecords("sh.diffuse.output.playlistItem", data),
      },
      tracks: {
        empty: () => [],
        get: async () => {
          const bundles = await this.listRecords(
            "sh.diffuse.output.trackBundle",
          );

          const tracks = bundles.flatMap((bundle) => bundle.tracks ?? []);
          lastPersistedTracks = tracks;

          return tracks;
        },
        put: async (data) => {
          const hashCurrent = xxh32r(encode(lastPersistedTracks ?? []));
          const hashNew = xxh32r(encode(data));

          if (hashCurrent === hashNew) {
            return;
          }

          /** @type {TrackBundle[]} */
          const bundles = [];

          for (let i = 0; i < data.length; i += 100) {
            const chunk = data.slice(i, i + 100);
            bundles.push({
              $type: "sh.diffuse.output.trackBundle",
              id: xxh32r(encode(chunk)).toString(16),
              tracks: chunk,
            });
          }

          await this.putRecords("sh.diffuse.output.trackBundle", bundles, {
            upsertBatchSize: 1,
          });

          lastPersistedTracks = data;
        },
      },
    });

    this.facets = this.#manager.facets;
    this.playlistItems = this.#manager.playlistItems;
    this.tracks = this.#manager.tracks;
  }

  // SIGNALS

  #did = signal(/** @type {`did:${string}:${string}` | null} */ (null));
  #isOnline = signal(navigator.onLine);
  #rev = signal(/** @type {string | null} */ (null));
  #revFetchedAt = 0;
  #writing = 0;

  /** @type {Array<{ fn: () => Promise<void>, resolve: () => void, reject: (err: unknown) => void }>} */
  #writeQueue = [];
  #writeDraining = false;
  /** @type {Map<string, { cancelled: boolean }>} */
  #writeCancels = new Map();

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
    this.#stopFirehose();
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
      this.#stopFirehose();
      await logout(this.#agent);
      this.#agent = null;
      this.#authenticated = Promise.withResolvers();
      this.#did.value = null;
      this.#pdsUrl = null;
      this.#rpc = null;
    }
  }

  /**
   * Clear session state without contacting the server.
   * Used when the session has already been revoked.
   */
  #clearSession() {
    this.#stopFirehose();
    this.#agent = null;
    this.#authenticated = Promise.withResolvers();
    this.#did.value = null;
    this.#pdsUrl = null;
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
    this.#pdsUrl = session.info.aud;
    this.#authenticated.resolve();
    this.#startFirehose();
  }

  // FIREHOSE

  #stopFirehose() {
    const iter = this.#firehoseIterator;
    this.#firehoseIterator = null;
    iter?.return?.();
  }

  async #startFirehose() {
    this.#stopFirehose();

    const gen = ++this.#firehoseGen;
    const pdsUrl = this.#pdsUrl;
    if (!pdsUrl) return;

    const wssUrl = pdsUrl.replace(
      /^https?:\/\//,
      (m) => m === "https://" ? "wss://" : "ws://",
    );

    const { FirehoseSubscription } = await import("@atcute/firehose");

    // Abort if superseded while awaiting the import
    if (this.#firehoseGen !== gen) return;

    const subscription = new FirehoseSubscription({
      service: wssUrl,
      nsid: ComAtprotoSyncSubscribeRepos.mainSchema,
      validateMessages: false,
    });

    const iter = subscription[Symbol.asyncIterator]();
    this.#firehoseIterator = iter;

    try {
      for await (const message of iter) {
        this.#handleFirehoseCommit(message);
      }
    } catch {
      // Non-fatal; partysocket handles reconnection automatically
    }
  }

  /**
   * @param {any} message
   */
  #handleFirehoseCommit(message) {
    if (message.$type !== "com.atproto.sync.subscribeRepos#commit") return;
    if (message.repo !== this.#did.value) return;

    // Skip commits we made ourselves (rev already reflects current state)
    if (message.rev === this.#rev.value) return;

    const touched = new Set(
      (message.ops ?? [])
        .map((/** @type {any} */ op) => op.path?.split("/")[0])
        .filter((/** @type {string} */ c) => WATCHED_COLLECTIONS.has(c)),
    );

    if (touched.size === 0) return;
    if (this.#writing > 0) return;

    if (touched.has("sh.diffuse.output.facet")) this.#manager.facets.reload();
    if (touched.has("sh.diffuse.output.playlistItem")) {
      this.#manager.playlistItems.reload();
    }
    if (touched.has("sh.diffuse.output.trackBundle")) {
      this.#manager.tracks.reload();
    }
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
      this.#revFetchedAt = Date.now();
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
   * Fetch the full repo CAR for the authenticated user, cached in IDB by rev.
   * Returns null if not authenticated or if the commit rev cannot be determined.
   *
   * @returns {Promise<Uint8Array | null>}
   */
  async #getRepoCar() {
    const did = this.#did.value;
    const rpc = this.#rpc;
    if (!rpc || !did) return null;

    const REV_TTL_MS = 5_000;
    const latestRev =
      (Date.now() - this.#revFetchedAt < REV_TTL_MS && this.#rev.value)
        ? this.#rev.value
        : await this.getLatestCommit();
    if (!latestRev) return null;

    const IDB_KEY = `diffuse/output/raw/atproto/repo/${did}`;
    const cached =
      /** @type {{ rev: string, bytes: Uint8Array } | undefined} */ (
        await IDB.get(IDB_KEY)
      );

    if (cached?.rev === latestRev) {
      return cached.bytes;
    }

    const bytes = await ok(rpc.get("com.atproto.sync.getRepo", {
      params: { did },
      as: "bytes",
    }));

    await IDB.set(IDB_KEY, { rev: latestRev, bytes });
    return bytes;
  }

  /**
   * @template T
   * @param {string} collection
   * @param {string} [did]
   * @returns {Promise<T[]>}
   */
  async listRecords(collection, did) {
    did ??= this.#did.value ?? undefined;

    if (!this.#rpc || !did) return [];

    try {
      const bytes = await this.#getRepoCar();
      if (!bytes) return [];

      const records = [];
      for (const entry of Repo.fromUint8Array(bytes)) {
        if (entry.collection === collection) {
          records.push(/** @type {T} */ (entry.record));
        }
      }
      return records;
    } catch (err) {
      if (this.#isSessionError(err)) {
        this.#clearSession();
        return [];
      }

      throw err;
    }
  }

  // WRITE QUEUE

  /** @returns {Promise<{ id: string, ts: number }[]>} */
  async #loadWriteWindow() {
    const now = Date.now();
    const all = /** @type {{ id: string, ts: number }[]} */ (
      await IDB.get(WRITE_IDB_KEY) ?? []
    );
    return all.filter((e) => now - e.ts < WRITE_WINDOW_MS);
  }

  /** @param {string[]} ids */
  async #recordWritten(ids) {
    const now = Date.now();
    const window = await this.#loadWriteWindow();
    await IDB.set(WRITE_IDB_KEY, [
      ...window,
      ...ids.map((id) => ({ id, ts: now })),
    ]);
  }

  /**
   * @param {() => Promise<void>} fn
   * @returns {Promise<void>}
   */
  #enqueueWrite(fn) {
    return new Promise((resolve, reject) => {
      this.#writeQueue.push({ fn, resolve, reject });
      this.#drainWrites();
    });
  }

  async #drainWrites() {
    if (this.#writeDraining) return;
    this.#writeDraining = true;

    while (this.#writeQueue.length > 0) {
      const { fn, resolve, reject } = /** @type {{ fn: () => Promise<void>, resolve: () => void, reject: (err: unknown) => void }} */ (
        this.#writeQueue.shift()
      );
      try {
        await fn();
        resolve();
      } catch (err) {
        reject(err);
      }
    }

    this.#writeDraining = false;
  }

  /**
   * @param {string} collection
   * @param {Array<{ id: string }>} data
   * @param {{ deleteBatchSize?: number, upsertBatchSize?: number }} [options]
   */
  async putRecords(
    collection,
    data,
    { deleteBatchSize = 100, upsertBatchSize = deleteBatchSize } = {},
  ) {
    if (!this.#rpc || !this.#did.value) return;

    // Supersede any prior write for this collection
    const prior = this.#writeCancels.get(collection);
    if (prior) prior.cancelled = true;

    const token = { cancelled: false };
    this.#writeCancels.set(collection, token);

    return this.#enqueueWrite(async () => {
      if (token.cancelled) return;
      try {
        await this.#doPutRecords(collection, data, { deleteBatchSize, upsertBatchSize }, token);
      } finally {
        if (this.#writeCancels.get(collection) === token) {
          this.#writeCancels.delete(collection);
        }
      }
    });
  }

  /**
   * @param {string} collection
   * @param {Array<{ id: string }>} data
   * @param {{ deleteBatchSize: number, upsertBatchSize: number }} options
   * @param {{ cancelled: boolean }} token
   */
  async #doPutRecords(collection, data, { deleteBatchSize, upsertBatchSize }, token) {
    const rpc = this.#rpc;
    const did = this.#did.value;
    if (!rpc || !did) return;

    this.#writing++;
    try {
      // 1. Fetch current state
      /** @type {Map<string, { rkey: string, value: unknown }>} */
      const existing = new Map();

      const repoBytes = await this.#getRepoCar();
      if (repoBytes) {
        for (const entry of Repo.fromUint8Array(repoBytes)) {
          if (entry.collection === collection) {
            const record = /** @type {any} */ (entry.record);
            existing.set(record.id, { rkey: entry.rkey, value: record });
          }
        }
      }

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

      // 4. Apply batches, throttled to WRITE_RATE_LIMIT ops/hour.
      // The write queue ensures we are the only writer, so one precise sleep
      // is enough — no need to re-check in a loop.
      const applyBatch = async (/** @type {any[]} */ batch) => {
        const window = await this.#loadWriteWindow();

        if (window.length + batch.length > WRITE_RATE_LIMIT) {
          const needed = window.length + batch.length - WRITE_RATE_LIMIT;
          const sorted = [...window].sort((a, b) => a.ts - b.ts);
          const waitMs = WRITE_WINDOW_MS - (Date.now() - sorted[needed - 1].ts) + 1;
          await new Promise((resolve) => setTimeout(resolve, waitMs));
        }

        const result = await ok(rpc.post("com.atproto.repo.applyWrites", {
          input: { repo: did, writes: batch },
        }));

        const writtenIds = batch.map((op) => op.rkey ?? op.value?.id).filter(
          Boolean,
        );
        await this.#recordWritten(writtenIds);

        if (result?.commit?.rev) {
          this.#rev.value = result.commit.rev;
        }
      };

      for (let i = 0; i < deletes.length; i += deleteBatchSize) {
        if (token.cancelled) return;
        await applyBatch(deletes.slice(i, i + deleteBatchSize));
      }

      for (let i = 0; i < upserts.length; i += upsertBatchSize) {
        if (token.cancelled) return;
        await applyBatch(upserts.slice(i, i + upsertBatchSize));
      }
    } catch (err) {
      if (this.#isSessionError(err)) {
        this.#clearSession();
        return;
      }

      throw err;
    } finally {
      this.#writing--;
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
