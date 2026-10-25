// Both atproto and rocksky oauth modules import from the same
// @atcute/oauth-browser-client instance, which holds a single global `database`
// and client id that configureOAuth() replaces on every call. When both
// elements are on the same page their concurrent operations interleave:
// module B's configureOAuth fires while module A's request is mid-flight, so
// A's token refresh reads/writes B's storage namespace (and uses B's client
// id). Sessions get "lost" and refreshes fail.
//
// This lock serialises all library operations so only one module at a time
// owns the global state. `withOAuthContext` additionally records which module
// currently owns it, because configureOAuth() allocates a fresh store
// (listeners, BroadcastChannel, cleanup locks) and must not run per request.

/** @type {string | null} */
let activeOwner = null;

/** @type {Promise<unknown>} */
let _chain = Promise.resolve();

/**
 * Run `fn` after all previously-enqueued OAuth operations complete.
 * Errors in `fn` propagate to the caller but do NOT block future operations.
 *
 * @template T
 * @param {() => Promise<T>} fn
 * @returns {Promise<T>}
 */
export function withOAuthLock(fn) {
  /** @type {Promise<T>} */
  const result = _chain.then(fn);
  _chain = result.catch(() => {});
  return result;
}

/**
 * Run `fn` under the OAuth lock, with the library configured for `owner`.
 * The configuration step is skipped when `owner` is already active, so the
 * per-request overhead stays small.
 *
 * @template T
 * @param {string} owner Stable identifier for the caller's OAuth config.
 * @param {() => void} configure Applies the caller's configureOAuth() options.
 * @param {() => T | Promise<T>} fn
 * @returns {Promise<T>}
 */
export function withOAuthContext(owner, configure, fn) {
  return withOAuthLock(async () => {
    if (activeOwner !== owner) {
      configure();
      activeOwner = owner;
    }

    return await fn();
  });
}
