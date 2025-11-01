import QS from "query-string";

import { announce, define, ostiary } from "@common/worker.js";
import { effect, signal } from "@common/signal.js";
import { arrayShuffle } from "@common/index.js";

/**
 * @import {Actions, Item} from "./types.d.ts"
 * @import {Track} from "@components/core/types.d.ts"
 */

const QUERY = QS.parse(location.search);
const qFillSize = QUERY?.["fill-size"];

/** @type {number} */
const FILL_SIZE = qFillSize && qFillSize !== null
  ? Array.isArray(qFillSize) && qFillSize[0] !== null
    ? parseInt(qFillSize[0], 10)
    : parseInt(/** @type {string} */ (qFillSize), 10)
  : 25;

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
export function add({ inFront, items }) {
  // TODO: An entry is always manual and should be added in the correct place
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
 * @param {Item[]} future
 * @returns {Item[]}
 */
function fill(future) {
  let fillFutureCount = 0;
  let manualFutureCount = 0;

  future.forEach((item) => {
    if (item.manualEntry) manualFutureCount++;
    else fillFutureCount++;
  });

  if (fillFutureCount >= FILL_SIZE) return future;

  /** @type {Item[]} */
  const pool = [];

  let p = new Set($past.value.map((t) => t.id));
  let reducedPool = pool;

  $lake.value.forEach((track) => {
    if (p.has(track.id)) {
      p = p.difference(new Set(track.id));
    } else {
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
    FILL_SIZE - fillFutureCount,
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
