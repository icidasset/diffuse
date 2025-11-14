import QS from "query-string";

import { announce, define, ostiary } from "@common/worker.js";
import { effect, signal } from "@common/signal.js";
import { arrayShuffle } from "@common/index.js";

/**
 * @import {Actions, Item} from "./types.d.ts"
 * @import {Track} from "@common/types.d.ts"
 */

const QUERY = QS.parse(location.search);
const qFill = QUERY?.["fill"];

/** @type {number | null} */
const FILL = qFill && qFill !== null
  ? Array.isArray(qFill) && qFill[0] !== null
    ? parseInt(qFill[0], 10)
    : parseInt(/** @type {string} */ (qFill), 10)
  : null;

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

export const $future = signal(/** @type {Item[]} */ ([]));
export const $lake = signal(/** @type {Track[]} */ ([]));
export const $now = signal(/** @type {Item | null} */ (null));
export const $past = signal(/** @type {Item[]} */ ([]));

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions['add']}
 */
export function add({ inFront, tracks }) {
  const items = tracks.map((track) => {
    return { ...track, manualEntry: true };
  });

  $future.value = inFront
    ? [...items, ...$future.value]
    : [...$future.value, ...items];
}

/**
 * @type {Actions['pool']}
 */
export function pool(tracks) {
  $lake.value = tracks;

  // TODO: If the pool changes, only remove non-existing tracks
  //       instead of resetting the whole future queue.
  //
  //       What about past queue items?

  // Automatically insert track if there isn't any
  if (!$now.value) _shift(fill([]));
  else $future.value = fill([]);
}

/**
 * @type {Actions['shift']}
 */
export function shift() {
  return _shift();
}

/**
 * @type {Actions['unshift']}
 */
export function unshift() {
  const p = $past.value;
  if (p.length === 0) return;

  const n = $now.value;
  const [last] = p.splice(p.length - 1, 1);

  $now.value = last ?? null;
  if (n) $future.value = [n, ...$future.value];
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((port) => {
  // Setup RPC

  define("future", $future.get, port);
  define("now", $now.get, port);
  define("past", $past.get, port);

  define("add", add, port);
  define("pool", pool, port);
  define("shift", shift, port);
  define("unshift", unshift, port);

  // Communicate state

  effect(() => announce("future", $future.value, port));
  effect(() => announce("now", $now.value, port));
  effect(() => announce("past", $past.value, port));
});

////////////////////////////////////////////
// ⛔️
////////////////////////////////////////////

/**
 * Automatically add non-manual items to the queue.
 *
 * @param {Item[]} future
 * @returns {Item[]}
 */
function fill(future) {
  if (!FILL) return future;

  // Count
  let autoFutureCount = 0;
  let manualFutureCount = 0;

  future.forEach((item) => {
    if (item.manualEntry) manualFutureCount++;
    else autoFutureCount++;
  });

  // Exit early if queue already filled appropriatly
  if (autoFutureCount >= FILL) return future;

  // Fill
  return fillShuffle(future, autoFutureCount);
}

/**
 * @param {Item[]} future
 * @param {number} autoFutureCount
 * @returns {Item[]}
 */
export function fillShuffle(future, autoFutureCount) {
  // Determine pool of available tracks
  /** @type {Item[]} */
  const pool = [];

  const pastSet = new Set($past.value.map((i) => i.id));
  let reducedPool = pool;

  $lake.value.forEach((track) => {
    if (pastSet.delete(track.id) === false) {
      pool.push({
        ...track,
        manualEntry: false,
      });
    }
  });

  if (reducedPool.length === 0) {
    reducedPool = $lake.value;
  }

  const poolSelection = arrayShuffle(reducedPool).slice(
    0,
    Math.max(0, (FILL ?? 0) - autoFutureCount),
  );

  return [...future, ...poolSelection];
}

/**
 * @param {Item[]} [future]
 */
export function _shift(future) {
  const n = $now.value;
  const f = future ?? $future.value;

  $now.value = f[0] ?? null;
  if (n) $past.value = [...$past.value, n];
  $future.value = fill(f.slice(1));
}
