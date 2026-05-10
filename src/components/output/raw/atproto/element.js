import { Client, ClientResponseError, ok } from "@atcute/client";
import { ComAtprotoSyncSubscribeRepos } from "@atcute/atproto";
import { decode, encode } from "@atcute/cbor";
import { xxh32r } from "xxh32/dist/raw.js";

import { computed, signal } from "~/common/signal.js";
import { BroadcastedOutputElement, outputManager } from "../../common.js";
import { defineElement } from "~/common/element.js";

import {
  clearStoredSession,
  login,
  logout,
  OAuthUserAgent,
  restoreOrFinalize,
  TokenRefreshError,
} from "./oauth.js";

/**
 * @import {OutputManager} from "../../types.d.ts"
 * @import {ATProtoOutputElement} from "./types.d.ts"
 * @import {MessageOf} from "@atcute/firehose"
 * @typedef {import("@atcute/atproto").ComAtprotoRepoApplyWrites.$input['writes'][number]} WriteOp
 */

/** @type {Set<string>} */
const WATCHED_COLLECTIONS = new Set([
  "sh.diffuse.output.facet",
  "sh.diffuse.output.playlistItemBundle",
  "sh.diffuse.output.setting",
  "sh.diffuse.output.trackBundle",
]);

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

  /** @type {PromiseWithResolvers<void>} */
  #restoreSettled = Promise.withResolvers();

  /** @type {Client | null} */
  #rpc = null;

  /** @type {OAuthUserAgent | null} */
  #agent = null;

  /** @type {string | null} */
  #pdsUrl = null;

  constructor() {
    super();

    /** @type {OutputManager} */
    this.#manager = outputManager({
      init: async () => {
        await this.#restoreSettled.promise;
        return true;
      },
      facets: {
        empty: () => [],
        get: () => this.listRecords("sh.diffuse.output.facet"),
        put: (data) => this.putRecords("sh.diffuse.output.facet", data),
      },
      playlistItems: this.#blobCollection("sh.diffuse.output.playlistItemBundle", { groupBy: "playlist" }),
      settings: {
        empty: () => [],
        get: () => this.listRecords("sh.diffuse.output.setting"),
        put: (data) => this.putRecords("sh.diffuse.output.setting", data),
      },
      tracks: this.#blobCollection("sh.diffuse.output.trackBundle"),
    });

    this.facets = this.#manager.facets;
    this.playlistItems = this.#manager.playlistItems;
    this.settings = this.#manager.settings;
    this.tracks = this.#manager.tracks;
  }

  // SIGNALS

  #did = signal(/** @type {`did:${string}:${string}` | null} */ (null));
  #handle = signal(/** @type {string | null} */ (null));
  #isOnline = signal(navigator.onLine);
  #rev = signal(/** @type {string | null} */ (null));
  #firehoseRev = signal(/** @type {string | null} */ (null));

  /** @type {AsyncIterator<unknown> | null} */
  #firehoseIterator = null;
  #firehoseGen = 0;
  #writing = 0;

  /** @type {Array<{ fn: () => Promise<void>, resolve: () => void, reject: (err: unknown) => void }>} */
  #writeQueue = [];
  #writeDraining = false;
  /** @type {Map<string, { cancelled: boolean }>} */
  #writeCancels = new Map();

  did = this.#did.get;
  handle = this.#handle.get;
  rev = this.#rev.get;
  firehoseRev = this.#firehoseRev.get;

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
   * @returns {Promise<void>}
   */
  whenRestored() {
    return this.#restoreSettled.promise;
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
      this.#handle.value = null;
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
    this.#handle.value = null;
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
      return this.#isSessionError((/** @type {{ cause: unknown }} */ (err)).cause);
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
    } finally {
      this.#restoreSettled.resolve();
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
    agent.getSession = /** @type {typeof originalGetSession} */ ((...args) => {
      const promise = originalGetSession(...args);

      promise.catch((err) => {
        if (err instanceof TokenRefreshError) {
          this.#clearSession();
        }
      });

      return promise;
    });

    this.#agent = agent;
    this.#rpc = new Client({ handler: agent });
    this.#did.value = session.info.sub;
    this.#pdsUrl = session.info.aud;
    this.#authenticated.resolve();
    this.#fetchHandle(session.info.sub);
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
   * @param {MessageOf<typeof ComAtprotoSyncSubscribeRepos.mainSchema>} message
   */
  #handleFirehoseCommit(message) {
    if (message.$type !== "com.atproto.sync.subscribeRepos#commit") return;

    const commit = /** @type {{ repo: string, rev: string, ops?: Array<{ path: string }> }} */ (message);

    if (commit.repo !== this.#did.value) return;
    if (commit.rev === this.#rev.value) return;
    if (this.#writing > 0) return;

    const touched = new Set(
      (commit.ops ?? [])
        .map((op) => op.path?.split("/")[0])
        .filter((c) => WATCHED_COLLECTIONS.has(c)),
    );

    if (touched.size === 0) return;

    this.#firehoseRev.value = commit.rev;

    if (touched.has("sh.diffuse.output.facet")) this.#manager.facets.reload();
    if (touched.has("sh.diffuse.output.playlistItemBundle")) this.#manager.playlistItems.reload();
    if (touched.has("sh.diffuse.output.setting")) this.#manager.settings.reload();
    if (touched.has("sh.diffuse.output.trackBundle")) this.#manager.tracks.reload();
  }

  /**
   * @param {string} did
   */
  async #fetchHandle(did) {
    const rpc = this.#rpc;
    if (!rpc) return;
    try {
      const result = await ok(rpc.get("com.atproto.repo.describeRepo", {
        params: {
          repo: /** @type {import("@atcute/lexicons").ActorIdentifier} */ (did),
        },
      }));
      if (this.#did.value === did) {
        this.#handle.value = result?.handle ?? null;
      }
    } catch {
      // Non-fatal; handle stays null
    }
  }

  // RECORDS

  /**
   * Returns `{ empty, get, put }` for a collection stored as CBOR blobs.
   * When `groupBy` is provided each distinct value of that field gets its own
   * bundle record; otherwise all items are packed into a single bundle.
   * Each call gets its own closure-local state.
   *
   * @param {string} nsid
   * @param {{ groupBy?: string }} [options]
   */
  #blobCollection(nsid, { groupBy } = {}) {
    if (groupBy) {
      /** @type {Map<string, string>} groupKey → content hash */
      let lastHashes = new Map();
      /** @type {Map<string, unknown>} groupKey → blob ref */
      let lastBlobs = new Map();

      return {
        empty: () => /** @type {unknown[]} */ ([]),
        get: async () => {
          const bundles = await this.listRecords(nsid);
          /** @type {unknown[]} */
          const items = [];
          lastHashes = new Map();
          lastBlobs = new Map();

          for (const bundle of bundles) {
            if (!bundle.data?.ref?.$link) continue;

            const key = /** @type {Record<string, unknown>} */ (bundle)[groupBy];
            if (typeof key !== "string") continue;

            const bytes = await this.#fetchBlob(bundle.data.ref.$link);
            const groupItems = /** @type {unknown[]} */ (decode(bytes));
            if (!Array.isArray(groupItems)) continue;

            items.push(...groupItems);
            lastHashes.set(key, xxh32r(bytes).toString(16));
            lastBlobs.set(key, bundle.data);
          }

          return items;
        },
        put: async (/** @type {unknown[]} */ data) => {
          const nsidTyped = /** @type {`${string}.${string}.${string}`} */ (nsid);

          /** @type {Map<string, unknown[]>} */
          const groups = new Map();
          for (const item of data) {
            const record = /** @type {Record<string, unknown>} */ (item);
            const key = record[groupBy];
            if (typeof key !== "string") continue;
            const group = groups.get(key) ?? [];
            if (!groups.has(key)) groups.set(key, group);
            group.push(item);
          }

          // Snapshot state so we only commit on success
          const newHashes = new Map(lastHashes);
          const newBlobs = new Map(lastBlobs);

          /** @type {WriteOp[]} */
          const writes = [];

          for (const [key, groupItems] of groups) {
            const bytes = encode(groupItems);
            const hash = xxh32r(bytes).toString(16);

            if (lastHashes.get(key) === hash && lastBlobs.has(key)) continue;

            const blob = await this.#uploadBlob(bytes);
            if (!blob) continue;

            const rkey = xxh32r(encode(key)).toString(16);
            const value = { $type: nsidTyped, id: rkey, [groupBy]: key, data: blob };

            if (lastHashes.has(key)) {
              writes.push({ $type: "com.atproto.repo.applyWrites#update", collection: nsidTyped, rkey, value });
            } else {
              writes.push({ $type: "com.atproto.repo.applyWrites#create", collection: nsidTyped, rkey, value });
            }

            newHashes.set(key, hash);
            newBlobs.set(key, blob);
          }

          for (const key of lastHashes.keys()) {
            if (!groups.has(key)) {
              const rkey = xxh32r(encode(key)).toString(16);
              writes.push({ $type: "com.atproto.repo.applyWrites#delete", collection: nsidTyped, rkey });
              newHashes.delete(key);
              newBlobs.delete(key);
            }
          }

          if (writes.length === 0) return;

          await this.#applyWriteOps(writes);

          lastHashes = newHashes;
          lastBlobs = newBlobs;
        },
      };
    }

    // Single-blob variant (used for tracks)
    /** @type {unknown[] | null} */
    let lastPersisted = null;

    return {
      empty: () => /** @type {unknown[]} */ ([]),
      get: async () => {
        const bundles = await this.listRecords(nsid);
        /** @type {unknown[]} */
        const items = [];
        for (const bundle of bundles) {
          if (!bundle.data?.ref?.$link) continue;
          const bytes = await this.#fetchBlob(bundle.data.ref.$link);
          items.push(...decode(bytes));
        }
        lastPersisted = items;
        return items;
      },
      put: async (/** @type {unknown[]} */ data) => {
        if (xxh32r(encode(lastPersisted ?? [])) === xxh32r(encode(data))) return;
        const bytes = encode(data);
        const blob = await this.#uploadBlob(bytes);
        if (!blob) return;
        const id = xxh32r(bytes).toString(16);
        const bundle = { id, data: blob };
        await this.putRecords(nsid, [bundle]);
        lastPersisted = data;
      },
    };
  }

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

  /** @param {Uint8Array} bytes */
  async #uploadBlob(bytes) {
    const rpc = this.#rpc;
    if (!rpc) return;
    const result = await ok(rpc.post("com.atproto.repo.uploadBlob", {
      input: bytes,
      headers: { "content-type": "application/octet-stream" },
    }));
    return result?.blob;
  }

  /**
   * @param {string} cid
   * @returns {Promise<Uint8Array>}
   */
  async #fetchBlob(cid) {
    const rpc = this.#rpc;
    const did = this.#did.value;
    if (!rpc || !did) return new Uint8Array();
    return await ok(rpc.get("com.atproto.sync.getBlob", {
      params: { did, cid },
      as: "bytes",
    }));
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
      const records = [];
      /** @type {string | undefined} */
      let cursor;
      do {
        const page = /** @type {{ records: { value: unknown }[], cursor?: string }} */ (
          await ok(this.#rpc.get("com.atproto.repo.listRecords", {
            params: {
              repo:
                /** @type {import("@atcute/lexicons").ActorIdentifier} */ (did),
              collection:
                /** @type {`${string}.${string}.${string}`} */ (collection),
              limit: 100,
              cursor,
            },
          }))
        );
        records.push(...page.records.map((r) => /** @type {T} */ (r.value)));
        cursor = page.cursor;
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

  // WRITE QUEUE

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
      const { fn, resolve, reject } =
        /** @type {{ fn: () => Promise<void>, resolve: () => void, reject: (err: unknown) => void }} */ (
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
        await this.#doPutRecords(collection, data, {
          deleteBatchSize,
          upsertBatchSize,
        }, token);
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
  async #doPutRecords(
    collection,
    data,
    { deleteBatchSize, upsertBatchSize },
    token,
  ) {
    const rpc = this.#rpc;
    const did = this.#did.value;
    if (!rpc || !did) return;

    const nsid = /** @type {`${string}.${string}.${string}`} */ (collection);

    this.#writing++;
    try {
      // 1. Fetch current state
      /** @type {Map<string, { rkey: string, value: unknown }>} */
      const existing = new Map();

      /** @type {string | undefined} */
      let cursor;
      do {
        const page = /** @type {{ records: { uri: string, value: unknown }[], cursor?: string }} */ (
          await ok(rpc.get("com.atproto.repo.listRecords", {
            params: { repo: did, collection: nsid, limit: 100, cursor },
          }))
        );
        for (const { uri, value } of page.records) {
          const record = /** @type {{ id: string }} */ (value);
          const rkey = /** @type {string} */ (uri.split("/").at(-1));
          existing.set(record.id, { rkey, value: record });
        }
        cursor = page.cursor;
      } while (cursor);

      // 2. Build desired state
      const desired = new Map(
        data.map((record) => [record.id, { $type: nsid, ...record }]),
      );

      // 3. Compute diff
      /** @type {WriteOp[]} */
      const deletes = [];

      /** @type {WriteOp[]} */
      const upserts = [];

      for (const [id, { rkey }] of existing) {
        if (!desired.has(id)) {
          deletes.push({ $type: "com.atproto.repo.applyWrites#delete", collection: nsid, rkey });
        }
      }

      for (const [id, record] of desired) {
        const entry = existing.get(id);

        if (!entry) {
          upserts.push({
            $type: "com.atproto.repo.applyWrites#create",
            collection: nsid,
            rkey: id,
            value: record,
          });
        } else if (JSON.stringify(entry.value) !== JSON.stringify(record)) {
          upserts.push({
            $type: "com.atproto.repo.applyWrites#update",
            collection: nsid,
            rkey: entry.rkey,
            value: record,
          });
        }
      }

      // 4. Apply batches
      /** @param {WriteOp[]} batch */
      const applyBatch = async (batch) => {
        const result = await ok(rpc.post("com.atproto.repo.applyWrites", {
          input: { repo: did, writes: batch },
        }));

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

  /**
   * Apply pre-computed write operations via applyWrites, respecting the write
   * queue and #writing guard.
   *
   * @param {WriteOp[]} writes
   * @param {number} [batchSize]
   */
  #applyWriteOps(writes, batchSize = 100) {
    if (!this.#rpc || !this.#did.value) return Promise.resolve();
    return this.#enqueueWrite(async () => {
      const rpc = this.#rpc;
      const did = this.#did.value;
      if (!rpc || !did) return;
      this.#writing++;
      try {
        for (let i = 0; i < writes.length; i += batchSize) {
          const batch = writes.slice(i, i + batchSize);
          const result = await ok(rpc.post("com.atproto.repo.applyWrites", {
            input: { repo: did, writes: batch },
          }));
          if (result?.commit?.rev) this.#rev.value = result.commit.rev;
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
    });
  }
}

export default ATProtoOutput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ATProtoOutput;
export const NAME = "dor-atproto";

defineElement(NAME, ATProtoOutput);
