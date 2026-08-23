import { Agent } from "@atproto/api";
import { decode, encode } from "@atcute/cbor";
import { xxh32r } from "xxh32/dist/raw.js";

import { computed, signal } from "~/common/signal.js";
import { BroadcastedOutputElement, outputManager } from "../../common.js";
import { defineElement } from "~/common/element.js";

import { login, logout, restoreOrFinalize } from "./oauth.js";

/**
 * @import {OutputManager} from "@specs/components/output/types.d.ts"
 * @import {ATProtoSpaceOutputElement} from "@specs/components/output/raw/atproto-space/types.d.ts"
 * @import {OAuthSession} from "@atproto/oauth-client"
 */

const SPACE_TYPE = "sh.diffuse.atproto.space";
const SPACE_KEY = "self";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Raw output backed by an AT Protocol permissioned data space, storing the
 * user's Diffuse data in their own (`authority=self`) space.
 *
 * @implements {ATProtoSpaceOutputElement}
 */
class ATProtoSpaceOutput extends BroadcastedOutputElement {
  static NAME = "diffuse/output/raw/atproto-space";

  #manager;

  /** @type {PromiseWithResolvers<void>} */
  #restoreSettled = Promise.withResolvers();

  /** @type {Agent | null} */
  #agent = null;

  /** @type {OAuthSession | null} */
  #session = null;

  #writing = 0;

  /** @type {Array<{ fn: () => Promise<void>, resolve: () => void, reject: (err: unknown) => void }>} */
  #writeQueue = [];
  #writeDraining = false;
  /** @type {Map<string, { cancelled: boolean }>} */
  #writeCancels = new Map();

  #did = signal(/** @type {string | null} */ (null));
  #handle = signal(/** @type {string | null} */ (null));

  did = this.#did.get;
  handle = this.#handle.get;

  ready = computed(() => this.#did.value !== null && this.#agent !== null);

  constructor() {
    super();

    /** @type {OutputManager} */
    this.#manager = outputManager({
      init: async () => {
        await this.#restoreSettled.promise;
        return true;
      },
      facets: this.#recordCollection("sh.diffuse.output.facet"),
      playlistItems: this.#blobCollection(
        "sh.diffuse.output.playlistItemBundle",
        { groupBy: "playlist" },
      ),
      settings: this.#recordCollection("sh.diffuse.output.setting"),
      tracks: this.#blobCollection("sh.diffuse.output.trackBundle", {
        groupBy: "scheme",
        keyOf: (item) => {
          const uri = String(
            /** @type {Record<string, unknown>} */ (item)["uri"] ?? "",
          );
          const colon = uri.indexOf(":");
          return colon > 0 ? uri.substring(0, colon) : undefined;
        },
      }),
    });

    this.facets = this.#manager.facets;
    this.playlistItems = this.#manager.playlistItems;
    this.settings = this.#manager.settings;
    this.tracks = this.#manager.tracks;
  }

  // LIFECYCLE

  /** @override */
  connectedCallback() {
    this.replicateSavedData(this.#manager);

    super.connectedCallback();

    this.#tryRestore();
  }

  // AUTH

  /**
   * Initiate the OAuth flow with a `space:` scope.
   *
   * @param {string} handle
   */
  async login(handle) {
    const session = await login(handle);
    this.#setSession(session);
  }

  /** @returns {Promise<void>} */
  whenRestored() {
    return this.#restoreSettled.promise;
  }

  async logout() {
    if (this.#session) {
      await logout(this.#session);
      this.#session = null;
      this.#agent = null;
      this.#did.value = null;
      this.#handle.value = null;
    }
  }

  async #tryRestore() {
    await this.whenConnected();

    try {
      const session = await restoreOrFinalize();
      if (session) this.#setSession(session);
    } finally {
      this.#restoreSettled.resolve();
    }
  }

  /**
   * @param {OAuthSession} session
   */
  #setSession(session) {
    // The OAuth session's DPoP-bound fetch handler plugs directly into the
    // generated client; every request is authenticated with the access token.
    this.#agent = new Agent((url, init) => session.fetchHandler(url, init));
    this.#session = session;
    this.#did.value = session.did;
    this.#ensureSpace();
  }

  // SPACE

  #spaceUri() {
    const did = this.#did.value;
    if (!did) return null;
    return `at://${did}/space/${SPACE_TYPE}/${SPACE_KEY}`;
  }

  /**
   * Ensure the personal space exists, creating it on first use.
   *
   * Existence is probed with `getSpace` (scoped to our own space, needing only
   * `read_self`) rather than `listSpaces` (which needs `authority=*`) or an
   * unconditional `createSpace` (which 400s with `SpaceAlreadyExists` on every
   * subsequent page load).
   */
  async #ensureSpace() {
    const agent = this.#agent;
    const space = this.#spaceUri();
    if (!agent || !space) return;

    try {
      await agent.com.atproto.simplespace.getSpace({ space });
      return; // already exists
    } catch (err) {
      const code = err && typeof err === "object"
        ? /** @type {{ error?: string }} */ (err).error
        : undefined;
      if (code !== "SpaceNotFound") throw err;
    }

    await agent.com.atproto.simplespace.createSpace({
      type: SPACE_TYPE,
      skey: SPACE_KEY,
      policy: { $type: "com.atproto.simplespace.defs#memberListPolicy" },
      appAccess: { $type: "com.atproto.simplespace.defs#open" },
    });
  }

  // RECORDS

  /**
   * Returns `{ empty, get, put }` for a small record collection (facets, settings).
   *
   * @param {string} nsid
   */
  #recordCollection(nsid) {
    /** @type {Map<string, Record<string, unknown>> | null} */
    let lastKnown = null;

    return {
      empty: () => [],
      get: async () => {
        const records = await this.#listRecords(nsid);
        lastKnown = new Map(
          /** @type {Array<Record<string, unknown>>} */ (records).map((r) => [
            String(r["id"]),
            r,
          ]),
        );
        return records;
      },
      put: async (/** @type {unknown[]} */ data) => {
        const nsidTyped = /** @type {`${string}.${string}.${string}`} */ (nsid);

        /** @type {Map<string, Record<string, unknown>>} */
        const desired = new Map(
          /** @type {Array<{ id: string }>} */ (data).map((r) => [
            r.id,
            /** @type {Record<string, unknown>} */ ({ $type: nsidTyped, ...r }),
          ]),
        );

        const known = lastKnown ?? new Map();

        /** @type {Array<[string, Record<string, unknown>]>} */
        const upserts = [];
        for (const [id, record] of desired) {
          const existing = known.get(id);
          if (existing && JSON.stringify(existing) === JSON.stringify(record)) {
            continue;
          }
          upserts.push([id, record]);
        }

        /** @type {Array<{ $type: "com.atproto.space.applyWrites#delete", collection: string, rkey: string }>} */
        const deletes = [];
        for (const id of known.keys()) {
          if (!desired.has(id)) {
            deletes.push({
              $type: "com.atproto.space.applyWrites#delete",
              collection: nsidTyped,
              rkey: id,
            });
          }
        }

        if (upserts.length === 0 && deletes.length === 0) return;

        const newKnown = new Map(known);
        for (const [id, record] of upserts) newKnown.set(id, record);
        for (const { rkey } of deletes) newKnown.delete(rkey);

        const prior = this.#writeCancels.get(nsid);
        if (prior) prior.cancelled = true;
        const token = { cancelled: false };
        this.#writeCancels.set(nsid, token);

        await this.#enqueueWrite(async () => {
          if (token.cancelled) return;
          const agent = this.#agent;
          const did = this.#did.value;
          const space = this.#spaceUri();
          if (!agent || !did || !space) return;
          this.#writing++;
          try {
            for (const [rkey, record] of upserts) {
              await agent.com.atproto.space.putRecord({
                space,
                repo: did,
                collection: nsidTyped,
                rkey,
                record,
              });
            }
            for (let i = 0; i < deletes.length; i += 100) {
              await agent.com.atproto.space.applyWrites({
                space,
                repo: did,
                writes: deletes.slice(i, i + 100),
              });
            }
            lastKnown = newKnown;
          } finally {
            this.#writing--;
            if (this.#writeCancels.get(nsid) === token) {
              this.#writeCancels.delete(nsid);
            }
          }
        });
      },
    };
  }

  /**
   * Returns `{ empty, get, put }` for a collection stored as CBOR blobs.
   *
   * @param {string} nsid
   * @param {{ groupBy: string, keyOf?: (item: unknown) => string | undefined }} options
   */
  #blobCollection(nsid, { groupBy, keyOf } = /** @type {any} */ ({})) {
    /** @type {Map<string, string>} */
    let lastHashes = new Map();
    /** @type {Map<string, unknown>} */
    let lastBlobs = new Map();

    return {
      empty: () => /** @type {unknown[]} */ ([]),
      get: async () => {
        const bundles = await this.#listRecords(nsid);
        /** @type {unknown[]} */
        const items = [];
        /** @type {Map<string, string>} */
        const newHashes = new Map();
        /** @type {Map<string, unknown>} */
        const newBlobs = new Map();

        for (const bundle of bundles) {
          if (!bundle.data?.ref?.$link) continue;

          const key = /** @type {Record<string, unknown>} */ (bundle)[groupBy];
          if (typeof key !== "string") continue;

          const bytes = await this.#fetchBlob(bundle.data.ref.$link);
          const groupItems = /** @type {unknown[]} */ (decode(bytes));
          if (!Array.isArray(groupItems)) continue;

          items.push(...groupItems);
          newHashes.set(key, xxh32r(encode(groupItems)).toString(16));
          newBlobs.set(key, bundle.data);
        }

        lastHashes = newHashes;
        lastBlobs = newBlobs;
        return items;
      },
      put: async (/** @type {unknown[]} */ data) => {
        const nsidTyped = /** @type {`${string}.${string}.${string}`} */ (nsid);

        const extractKey = keyOf ??
          ((/** @type {unknown} */ item) =>
            /** @type {string | undefined} */ (
              /** @type {Record<string, unknown>} */ (item)[groupBy]
            ));

        /** @type {Map<string, unknown[]>} */
        const groups = new Map();
        for (const item of data) {
          const key = extractKey(item);
          if (typeof key !== "string") continue;
          const group = groups.get(key) ?? [];
          if (!groups.has(key)) groups.set(key, group);
          group.push(item);
        }

        const newHashes = new Map(lastHashes);
        const newBlobs = new Map(lastBlobs);

        /** @type {Array<{ rkey: string, value: unknown }>} */
        const upserts = [];

        for (const [key, groupItems] of groups) {
          const bytes = encode(groupItems);
          const hash = xxh32r(bytes).toString(16);

          if (lastHashes.get(key) === hash && lastBlobs.has(key)) continue;

          const blob = await this.#uploadBlob(bytes);
          if (!blob) continue;

          const rkey = xxh32r(encode(key)).toString(16);
          const value = {
            $type: nsidTyped,
            id: rkey,
            [groupBy]: key,
            data: blob,
          };
          upserts.push({ rkey, value });
          newHashes.set(key, hash);
          newBlobs.set(key, blob);
        }

        /** @type {Array<{ $type: "com.atproto.space.applyWrites#delete", collection: string, rkey: string }>} */
        const deletes = [];
        for (const key of lastHashes.keys()) {
          if (!groups.has(key)) {
            const rkey = xxh32r(encode(key)).toString(16);
            deletes.push({
              $type: "com.atproto.space.applyWrites#delete",
              collection: nsidTyped,
              rkey,
            });
            newHashes.delete(key);
            newBlobs.delete(key);
          }
        }

        if (upserts.length === 0 && deletes.length === 0) return;

        await this.#enqueueWrite(async () => {
          const agent = this.#agent;
          const did = this.#did.value;
          const space = this.#spaceUri();
          if (!agent || !did || !space) return;
          this.#writing++;
          try {
            for (const { rkey, value } of upserts) {
              await agent.com.atproto.space.putRecord({
                space,
                repo: did,
                collection: nsidTyped,
                rkey,
                record: /** @type {Record<string, unknown>} */ (value),
              });
            }
            for (let i = 0; i < deletes.length; i += 100) {
              await agent.com.atproto.space.applyWrites({
                space,
                repo: did,
                writes: deletes.slice(i, i + 100),
              });
            }
          } finally {
            this.#writing--;
          }
        });

        lastHashes = newHashes;
        lastBlobs = newBlobs;
      },
    };
  }

  // HELPERS

  /**
   * List the record values in a collection from the user's space.
   *
   * @template T
   * @param {string} collection
   * @returns {Promise<T[]>}
   */
  async #listRecords(collection) {
    const agent = this.#agent;
    const did = this.#did.value;
    const space = this.#spaceUri();
    if (!agent || !did || !space) return /** @type {T[]} */ ([]);

    /** @type {T[]} */
    const records = [];
    /** @type {string | undefined} */
    let cursor;
    do {
      const { data } = await agent.com.atproto.space.listRecords({
        space,
        repo: did,
        collection,
        limit: 100,
        cursor,
      });
      for (const record of data.records) {
        if (record.value !== undefined) {
          records.push(/** @type {T} */ (record.value));
        }
      }
      cursor = data.cursor;
    } while (cursor);
    return records;
  }

  /**
   * @param {Uint8Array} bytes
   * @returns {Promise<unknown>}
   */
  async #uploadBlob(bytes) {
    const agent = this.#agent;
    if (!agent) return undefined;
    const { data } = await agent.com.atproto.repo.uploadBlob(bytes, {
      encoding: "application/octet-stream",
    });
    return data.blob;
  }

  /**
   * @param {string} cid
   * @returns {Promise<Uint8Array>}
   */
  async #fetchBlob(cid) {
    const agent = this.#agent;
    const did = this.#did.value;
    const space = this.#spaceUri();
    if (!agent || !did || !space) return new Uint8Array();
    const { data } = await agent.com.atproto.space.getBlob({
      space,
      repo: did,
      cid,
    });
    return data;
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
}

export default ATProtoSpaceOutput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ATProtoSpaceOutput;
export const NAME = "dor-atproto-space";

defineElement(NAME, ATProtoSpaceOutput);
