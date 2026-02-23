import { announce, ostiary, rpc } from "@common/worker.js";
import { effect, signal } from "@common/signal.js";
import { arrayShuffle } from "@common/utils.js";
import { xxh32 } from "xxh32";

/**
 * @import {Actions, Item} from "./types.d.ts"
 */

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

/** Ordered list of available track IDs. */
export const $lake = signal(/** @type {string[]} */ ([]));

// Communicated state
export const $future = signal(/** @type {Item[]} */ ([]));
export const $now = signal(/** @type {Item | null} */ (null));
export const $past = signal(/** @type {Item[]} */ ([]));
export const $supplyFingerprint = signal(
  /** @type {string | undefined} */ (undefined),
);

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions['add']}
 */
export function add({ inFront, trackIds }) {
  const items = trackIds.map((id) => {
    return { id, manualEntry: true };
  });

  $future.value = inFront
    ? [...items, ...$future.value]
    : [...$future.value, ...items];
}

/**
 * @type {Actions['clear']}
 */
export function clear({ manualOnly }) {
  $future.value = manualOnly
    ? $future.value.filter((i) => i.manualEntry === true)
    : [];
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
 * @type {Actions['shift']}
 */
export function shift() {
  return _shift();
}

/**
 * @type {Actions['supply']}
 */
export function supply({ trackIds }) {
  $lake.value = trackIds;
  $supplyFingerprint.value = trackIds.length
    ? xxh32(trackIds.join("\0")).toString()
    : undefined;
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
    clear,
    fill,
    shift,
    supply,
    unshift,

    // State
    future: $future.get,
    now: $now.get,
    past: $past.get,
    supplyFingerprint: $supplyFingerprint.get,
  });

  // Effects

  // Communicate state
  effect(() => announce("future", $future.value, context));
  effect(() => announce("now", $now.value, context));
  effect(() => announce("past", $past.value, context));
  effect(() =>
    announce("supplyFingerprint", $supplyFingerprint.value, context)
  );
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

  future.forEach((item) => {
    if (item.manualEntry) {}
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
    ? $lake.value.indexOf(lastManual.id) + 1
    : $now.value
    ? $lake.value.indexOf($now.value.id) + 1
    : 0;

  const maxIndex = $lake.value.length - 1;
  let currIndex = startIndex;

  /** @type {Item[]} */
  const autoItems = [];

  for (let i = 0; i < fillAmount; i++) {
    if (currIndex > maxIndex) currIndex = 0;
    const id = $lake.value[currIndex];
    if (id) {
      autoItems.push({ id, manualEntry: false });
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

  $lake.value.forEach((id) => {
    if (pastSet.delete(id) === false) {
      pool.push({ id, manualEntry: false });
    }
  });

  if (reducedPool.length === 0) {
    reducedPool = $lake.value.map((id) => ({ id, manualEntry: false }));
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
