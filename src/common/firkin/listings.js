/**
 * Shared registry-listing cache.
 *
 * A listing is the map of registry names to manifest CIDs for one namespace.
 * Every consumer in the page can reuse the same answer, and a BroadcastChannel
 * carries fresh snapshots and invalidations to other tabs. The listing only
 * contains pointers: manifests are still fetched and verified on read.
 */

/** Channel shared by every Firkin output in the origin. */
const CHANNEL_NAME = "diffuse:firkin:listings";

/** Default freshness window, matching the output's listing reuse. */
const DEFAULT_TTL_MS = 5_000;

/**
 * @typedef {Record<string, string> | null} Names
 * @typedef {{ at: number; names: Names }} Entry
 * @typedef {{ registryNames(namespace: string): Promise<Names> }} Lister
 * @typedef {{
 *   postMessage(message: unknown): void;
 *   addEventListener(type: "message", listener: (event: MessageEvent) => void): void;
 *   removeEventListener(type: "message", listener: (event: MessageEvent) => void): void;
 *   close(): void;
 * }} Channel
 */

/**
 * @param {unknown} value
 * @returns {value is Names}
 */
function isNames(value) {
  if (value === null) return true;
  if (typeof value !== "object" || Array.isArray(value)) return false;

  return Object.values(value).every((entry) => typeof entry === "string");
}

/**
 * The cache key for one account's view of a registry namespace.
 *
 * The registry itself is account-scoped, so a listing from another account (or
 * another server) must never be reused.
 *
 * @param {{ apiBase: string; did: string }} session
 * @param {string} namespace
 */
export function listingKey(session, namespace) {
  return `${session.apiBase}\u0000${session.did}\u0000${namespace}`;
}

/**
 * @param {{ channel?: Channel }} [options] - Injectable for tests.
 */
export function createListingStore(options = {}) {
  /** @type {Map<string, Entry>} */
  const cache = new Map();
  /** @type {Map<string, Promise<Names>>} */
  const inflight = new Map();
  /** @type {Map<string, number>} */
  const revisions = new Map();

  /** @type {Channel | undefined} */
  let channel = options.channel;
  let channelReady = false;

  /** @param {MessageEvent} event */
  const onMessage = (event) => {
    const message = event.data;
    if (!message || typeof message !== "object") return;
    if (typeof message.key !== "string") return;

    if (message.type === "listing") {
      if (typeof message.at !== "number" || !isNames(message.names)) return;

      const entry = { at: message.at, names: message.names };
      const current = cache.get(message.key);
      if (current && current.at >= entry.at) return;

      cache.set(message.key, entry);
      return;
    }

    if (message.type === "invalidate") {
      if (typeof message.at !== "number") return;

      const current = cache.get(message.key);
      if (current && current.at > message.at) return;

      cache.delete(message.key);
    }
  };

  const ensureChannel = () => {
    if (channelReady) return channel;
    channelReady = true;

    if (!channel && typeof BroadcastChannel === "function") {
      try {
        channel = new BroadcastChannel(CHANNEL_NAME);
      } catch {
        // Some privacy modes expose the constructor but refuse to create one.
        channel = undefined;
      }
    }

    channel?.addEventListener("message", onMessage);
    return channel;
  };

  /** @param {unknown} message */
  const post = (message) => {
    ensureChannel();

    try {
      channel?.postMessage(message);
    } catch {
      // A closed or unsupported channel is only a cache loss.
    }
  };

  /**
   * @param {Lister} rb
   * @param {string} key
   * @param {string} namespace
   * @param {number} [ttl]
   * @returns {Promise<Names>}
   */
  const names = (rb, key, namespace, ttl = DEFAULT_TTL_MS) => {
    ensureChannel();

    const cached = cache.get(key);
    if (cached && Date.now() - cached.at < ttl) {
      return Promise.resolve(cached.names);
    }

    const pending = inflight.get(key);
    if (pending) return pending;

    const revision = revisions.get(key) ?? 0;
    const startedAt = Date.now();

    /** @type {Promise<Names>} */
    let promise;

    promise = rb.registryNames(namespace).then(
      (listing) => {
        if (inflight.get(key) === promise) inflight.delete(key);

        const entry = { at: Date.now(), names: listing };

        // A local invalidation means a push or hint landed while this request
        // was in flight; don't repopulate the cache with a possibly stale list.
        if ((revisions.get(key) ?? 0) !== revision) return listing;

        const current = cache.get(key);
        if (current && current.at >= entry.at) return current.names;

        cache.set(key, entry);
        post({ type: "listing", key, ...entry });

        return entry.names;
      },
      (err) => {
        if (inflight.get(key) === promise) inflight.delete(key);

        // Keep a newer listing that arrived from another tab while this
        // request was failing.
        const current = cache.get(key);
        if (!current || current.at <= startedAt) cache.delete(key);

        throw err;
      },
    );

    inflight.set(key, promise);
    return promise;
  };

  /** @param {string} key */
  const invalidate = (key) => {
    ensureChannel();

    const at = Date.now();
    cache.delete(key);
    inflight.delete(key);
    revisions.set(key, (revisions.get(key) ?? 0) + 1);

    post({ type: "invalidate", key, at });
  };

  const close = () => {
    channel?.removeEventListener("message", onMessage);
    channel?.close();
    channel = undefined;
    channelReady = false;
    cache.clear();
    inflight.clear();
    revisions.clear();
  };

  // A store given a channel (tests) starts listening immediately. The default
  // store stays lazy so importing this module has no side effects.
  if (options.channel) ensureChannel();

  return { names, invalidate, close };
}

const listings = createListingStore();

export const names = listings.names;
export const invalidate = listings.invalidate;
