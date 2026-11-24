import * as IDB from "idb-keyval";

/**
 * Write a record only if it is at least as new as the one already stored.
 *
 * The read and write run under a short-lived Web Lock keyed by the IDB key, so
 * two tabs cannot interleave a stale write between another tab's read and
 * write. The lock is released immediately: it is not leadership, and a
 * suspended tab cannot hold it indefinitely.
 *
 * `advance(next, current)` returns whether `next` should replace `current`.
 *
 * @template T
 * @param {string} key
 * @param {T} next
 * @param {(next: T, current: T) => boolean} advance
 * @returns {Promise<T>} The record that ended up in storage.
 */
export async function writeIfNewer(key, next, advance) {
  const run = async () => {
    /** @type {T | undefined} */
    const current = await IDB.get(key);
    if (current !== undefined && !advance(next, current)) return current;

    await IDB.set(key, next);
    return next;
  };

  if (typeof navigator !== "undefined" && navigator.locks?.request) {
    return await navigator.locks.request(`diffuse:idb:${key}`, run);
  }

  return await run();
}
