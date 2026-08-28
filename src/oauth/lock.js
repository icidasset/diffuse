// Both atproto and rocksky oauth modules import from the same
// @atcute/oauth-browser-client instance, which holds a single global `database`
// reference that configureOAuth() replaces on every call. When both elements
// are on the same page their concurrent restoreOrFinalize() calls interleave:
// module B's configureOAuth fires while module A's finalizeAuthorization is
// mid-flight, so A's storeSession() ends up writing to B's localStorage
// namespace. The session is "lost" on the next page load.
//
// This lock serialises all library operations so only one module at a time
// owns the global state.

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
