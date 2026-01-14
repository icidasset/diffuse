import { announce, ostiary, rpc } from "@common/worker.js";
import { effect, signal } from "@common/signal.js";
import { arrayShuffle, hash } from "@common/utils.js";

/**
 * @import {Actions, Item} from "./types.d.ts"
 * @import {Track} from "@definitions/types.d.ts"
 */

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

export const $lake = signal(/** @type {Track[]} */ ([]));

// Communicated state
export const $future = signal(/** @type {Item[]} */ ([]));
export const $now = signal(/** @type {Item | null} */ (null));
export const $past = signal(/** @type {Item[]} */ ([]));
export const $poolHash = signal(hash([]));

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
 * @type {Actions['fill']}
 */
export function fill({ augment, amount, shuffled }) {
  $future.value = fillQueue(
    shuffled,
    amount +
      (augment
        ? $future.value.filter((i) => i.manualEntry === false).length
        : 0),
    $future.value,
  );
}

/**
 * @type {Actions['pool']}
 */
export function pool(tracks) {
  $lake.value = tracks;
  $poolHash.value = hash(tracks);
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

ostiary((context, _firstConnection, _connectionId) => {
  // Setup RPC

  rpc(context, {
    add,
    fill,
    pool,
    shift,
    unshift,

    // State
    future: $future.get,
    now: $now.get,
    past: $past.get,
    poolHash: $poolHash.get,
  });

  // Effects

  // Communicate state
  effect(() => announce("future", $future.value, context));
  effect(() => announce("now", $now.value, context));
  effect(() => announce("past", $past.value, context));
  effect(() => announce("poolHash", $poolHash.value, context));

  // When the pool changes,
  // make sure all future queue items still exist.
  effect(() => {
    const existing = new Set($lake.value.map((t) => t.id));

    $future.value = $future.value.filter((i) => {
      return existing.has(i.id);
    });
  });
});

////////////////////////////////////////////
// ⛔️
////////////////////////////////////////////

/**
 * Add non-manual items to the queue.
 *
 * @param {boolean} shuffled
 * @param {number | undefined | null} fillAmount
 * @param {Item[]} future
 * @returns {Item[]}
 */
function fillQueue(shuffled, fillAmount, future) {
  if (!fillAmount) return future;

  // Count
  let autoFutureCount = 0;
  let manualFutureCount = 0;

  future.forEach((item) => {
    if (item.manualEntry) manualFutureCount++;
    else autoFutureCount++;
  });

  // Fill
  if (shuffled) {
    if (autoFutureCount >= fillAmount) return future;
    return fillShuffle(fillAmount, future, autoFutureCount);
  } else {
    return fillSequentially(fillAmount, future);
  }
}

/**
 * @param {number} fillAmount
 * @param {Item[]} future
 * @returns {Item[]}
 */
export function fillSequentially(fillAmount, future) {
  const onlyManual = future.filter((i) => i.manualEntry);
  const lastManual = onlyManual.slice(-1)[0];
  const startIndex = lastManual
    ? $lake.value.findIndex((t) => t.id === lastManual.id) + 1
    : $now.value
    ? $lake.value.findIndex((t) => t.id === $now.value?.id) + 1
    : 0;

  const maxIndex = $lake.value.length - 1;
  let currIndex = startIndex;

  /** @type {Item[]} */
  const autoItems = [];

  for (let i = 0; i < fillAmount; i++) {
    if (currIndex > maxIndex) currIndex = 0;
    const item = $lake.value[currIndex];
    if (item) {
      autoItems.push({
        ...item,
        manualEntry: false,
      });
    }
    currIndex++;
  }

  return [...onlyManual, ...autoItems];
}

/**
 * @param {number} fillAmount
 * @param {Item[]} future
 * @param {number} autoFutureCount
 * @returns {Item[]}
 */
export function fillShuffle(fillAmount, future, autoFutureCount) {
  // Determine pool of available queue items
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
    Math.max(0, fillAmount - autoFutureCount),
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
  $future.value = f.slice(1);
}
