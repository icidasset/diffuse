import { announce, ostiary, rpc } from "~/common/worker.js";
import { effect, signal } from "~/common/signal.js";
import { arrayShuffle } from "~/common/utils.js";
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
 *
 * @example Adds tracks after the last manual entry, before any auto-filled items
 * ```js
 * import { add, $future } from "~/components/engine/queue/worker.js";
 *
 * add({ trackIds: ["a", "b"] });
 *
 * if ($future.value.length !== 2) throw new Error("expected 2 items");
 * if ($future.value[0].id !== "a") throw new Error("wrong first item");
 * if ($future.value[1].id !== "b") throw new Error("wrong second item");
 * if (!$future.value[0].manualEntry) throw new Error("items should be manualEntry: true");
 * ```
 *
 * @example Inserts before auto-filled items when they are present
 * ```js
 * import { add, $future } from "~/components/engine/queue/worker.js";
 *
 * $future.value = [
 *   { id: "manual", manualEntry: true },
 *   { id: "auto", manualEntry: false },
 * ];
 *
 * add({ trackIds: ["new"] });
 *
 * if ($future.value[0].id !== "manual") throw new Error("expected 'manual' first");
 * if ($future.value[1].id !== "new") throw new Error("expected 'new' second");
 * if ($future.value[2].id !== "auto") throw new Error("expected 'auto' last");
 * ```
 *
 * @example Prepends tracks to the front with inFront: true
 * ```js
 * import { add, $future } from "~/components/engine/queue/worker.js";
 *
 * add({ trackIds: ["c"] });
 * add({ inFront: true, trackIds: ["a", "b"] });
 *
 * if ($future.value[0].id !== "a") throw new Error("expected 'a' first");
 * if ($future.value[1].id !== "b") throw new Error("expected 'b' second");
 * if ($future.value[2].id !== "c") throw new Error("expected 'c' last");
 * ```
 */
export function add({ inFront, trackIds }) {
  const items = trackIds.map((id) => {
    return { id, manualEntry: true };
  });

  if (inFront) {
    $future.value = [...items, ...$future.value];
  } else {
    let lastManualIdx = -1;
    for (let i = 0; i < $future.value.length; i++) {
      if ($future.value[i].manualEntry) lastManualIdx = i;
    }
    $future.value = [
      ...$future.value.slice(0, lastManualIdx + 1),
      ...items,
      ...$future.value.slice(lastManualIdx + 1),
    ];
  }
}

/**
 * @type {Actions['clear']}
 *
 * @example Keeps manual entries when keepManual is true
 * ```js
 * import { clear, $future } from "~/components/engine/queue/worker.js";
 *
 * $future.value = [
 *   { id: "manual", manualEntry: true },
 *   { id: "auto", manualEntry: false },
 * ];
 * clear({ keepManual: true });
 *
 * if ($future.value.length !== 1) throw new Error("expected 1 item remaining");
 * if ($future.value[0].id !== "manual") throw new Error("manual entry should remain");
 * ```
 *
 * @example Clears all items when keepManual is false
 * ```js
 * import { clear, $future } from "~/components/engine/queue/worker.js";
 *
 * $future.value = [
 *   { id: "manual", manualEntry: true },
 *   { id: "auto", manualEntry: false },
 * ];
 * clear({ keepManual: false });
 *
 * if ($future.value.length !== 0) throw new Error("expected empty queue");
 * ```
 */
export function clear({ keepManual }) {
  $future.value = keepManual
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
 * @type {Actions['move']}
 *
 * @example Moves an item forward in the flat list
 * ```js
 * import { move, $future } from "~/components/engine/queue/worker.js";
 *
 * $future.value = [
 *   { id: "a", manualEntry: true },
 *   { id: "b", manualEntry: true },
 *   { id: "c", manualEntry: true },
 * ];
 *
 * move({ from: 0, to: 2 });
 *
 * if ($future.value[0].id !== "b") throw new Error("expected 'b' first");
 * if ($future.value[1].id !== "c") throw new Error("expected 'c' second");
 * if ($future.value[2].id !== "a") throw new Error("expected 'a' last");
 * ```
 *
 * @example Moves an item backward in the flat list
 * ```js
 * import { move, $future } from "~/components/engine/queue/worker.js";
 *
 * $future.value = [
 *   { id: "a", manualEntry: true },
 *   { id: "b", manualEntry: true },
 *   { id: "c", manualEntry: true },
 * ];
 *
 * move({ from: 2, to: 0 });
 *
 * if ($future.value[0].id !== "c") throw new Error("expected 'c' first");
 * if ($future.value[1].id !== "a") throw new Error("expected 'a' second");
 * if ($future.value[2].id !== "b") throw new Error("expected 'b' last");
 * ```
 *
 * @example Preserves now identity when reordering across past/future
 * ```js
 * import { move, $past, $now, $future } from "~/components/engine/queue/worker.js";
 *
 * $past.value = [{ id: "a", manualEntry: false }];
 * $now.value = { id: "b", manualEntry: false };
 * $future.value = [{ id: "c", manualEntry: false }];
 *
 * // flat list is [a(0), b(1), c(2)]; moving c to front → [c, a, b]
 * move({ from: 2, to: 0 });
 *
 * if ($now.value?.id !== "b") throw new Error("now should still be 'b'");
 * if ($past.value[0]?.id !== "c") throw new Error("expected 'c' first in past");
 * if ($past.value[1]?.id !== "a") throw new Error("expected 'a' second in past");
 * if ($future.value.length !== 0) throw new Error("future should be empty");
 * ```
 *
 * @example Does nothing when from equals to
 * ```js
 * import { move, $future } from "~/components/engine/queue/worker.js";
 *
 * $future.value = [{ id: "a", manualEntry: true }, { id: "b", manualEntry: true }];
 *
 * move({ from: 1, to: 1 });
 *
 * if ($future.value[0].id !== "a") throw new Error("order should be unchanged");
 * if ($future.value[1].id !== "b") throw new Error("order should be unchanged");
 * ```
 *
 * @example Does nothing for out-of-bounds indices
 * ```js
 * import { move, $future } from "~/components/engine/queue/worker.js";
 *
 * $future.value = [{ id: "a", manualEntry: true }, { id: "b", manualEntry: true }];
 *
 * move({ from: 0, to: 99 });
 *
 * if ($future.value[0].id !== "a") throw new Error("order should be unchanged");
 * if ($future.value[1].id !== "b") throw new Error("order should be unchanged");
 * ```
 */
export function move({ from, to }) {
  const all = [
    ...$past.value,
    ...($now.value ? [$now.value] : []),
    ...$future.value,
  ];

  if (from === to || from < 0 || to < 0 || from >= all.length || to >= all.length) return;

  const [item] = all.splice(from, 1);
  all.splice(to, 0, item);

  const now = $now.value;
  if (now) {
    const nowIdx = all.indexOf(now);
    $past.value = all.slice(0, nowIdx);
    $now.value = all[nowIdx] ?? null;
    $future.value = all.slice(nowIdx + 1);
  } else {
    const pastLen = $past.value.length;
    $past.value = all.slice(0, pastLen);
    $future.value = all.slice(pastLen);
  }
}

/**
 * @type {Actions['shift']}
 */
export function shift() {
  return _shift();
}

/**
 * @type {Actions['supply']}
 *
 * @example Sets the track pool and computes a fingerprint
 * ```js
 * import { supply, $lake, $supplyFingerprint } from "~/components/engine/queue/worker.js";
 *
 * supply({ trackIds: ["a", "b", "c"] });
 *
 * if ($lake.value.join(",") !== "a,b,c") throw new Error("lake not set correctly");
 * if (typeof $supplyFingerprint.value !== "string") throw new Error("fingerprint should be a string");
 * ```
 *
 * @example Returns undefined fingerprint for an empty supply
 * ```js
 * import { supply, $supplyFingerprint } from "~/components/engine/queue/worker.js";
 *
 * supply({ trackIds: [] });
 *
 * if ($supplyFingerprint.value !== undefined) throw new Error("fingerprint should be undefined for empty supply");
 * ```
 *
 * @example Same track IDs produce the same fingerprint
 * ```js
 * import { supply, $supplyFingerprint } from "~/components/engine/queue/worker.js";
 *
 * supply({ trackIds: ["x", "y"] });
 * const first = $supplyFingerprint.value;
 *
 * supply({ trackIds: ["x", "y"] });
 * const second = $supplyFingerprint.value;
 *
 * if (first !== second) throw new Error("same tracks should produce the same fingerprint");
 * ```
 */
export function supply({ trackIds }) {
  $lake.value = trackIds;
  $supplyFingerprint.value = trackIds.length
    ? xxh32(trackIds.join("\0")).toString()
    : undefined;
}

/**
 * @type {Actions['unshift']}
 *
 * @example Moves the last past item back to now, pushing now to the front of future
 * ```js
 * import { unshift, $future, $now, $past } from "~/components/engine/queue/worker.js";
 *
 * $past.value = [{ id: "prev", manualEntry: false }];
 * $now.value = { id: "current", manualEntry: false };
 * $future.value = [];
 *
 * unshift();
 *
 * if ($now.value?.id !== "prev") throw new Error("expected 'prev' as now");
 * if ($past.value.length !== 0) throw new Error("expected empty past");
 * if ($future.value[0]?.id !== "current") throw new Error("expected 'current' back at front of future");
 * ```
 *
 * @example Does nothing when past is empty
 * ```js
 * import { unshift, $now, $past } from "~/components/engine/queue/worker.js";
 *
 * $past.value = [];
 * $now.value = { id: "current", manualEntry: false };
 *
 * unshift();
 *
 * if ($now.value?.id !== "current") throw new Error("now should remain unchanged");
 * ```
 */
export function unshift() {
  const p = $past.value;
  if (p.length === 0) return;

  const n = $now.value;
  const last = p[p.length - 1];

  $past.value = p.slice(0, p.length - 1);
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
    move,
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
 *
 * @example Fills sequentially from the start of the lake
 * ```js
 * import { fillSequentially, $lake, $now } from "~/components/engine/queue/worker.js";
 *
 * $lake.value = ["a", "b", "c", "d"];
 * $now.value = null;
 *
 * const result = fillSequentially(3, []);
 *
 * if (result.length !== 3) throw new Error("expected 3 items");
 * if (result[0].id !== "a") throw new Error("expected to start from 'a'");
 * if (result[1].id !== "b") throw new Error("expected 'b' second");
 * if (result[2].id !== "c") throw new Error("expected 'c' third");
 * if (result[0].manualEntry !== false) throw new Error("auto items should have manualEntry: false");
 * ```
 *
 * @example Continues from after the current now item
 * ```js
 * import { fillSequentially, $lake, $now } from "~/components/engine/queue/worker.js";
 *
 * $lake.value = ["a", "b", "c", "d"];
 * $now.value = { id: "b", manualEntry: false };
 *
 * const result = fillSequentially(2, []);
 *
 * if (result[0].id !== "c") throw new Error("expected to start after now ('c')");
 * if (result[1].id !== "d") throw new Error("expected 'd' second");
 * ```
 *
 * @example Wraps around to the beginning when reaching the end of the lake
 * ```js
 * import { fillSequentially, $lake, $now } from "~/components/engine/queue/worker.js";
 *
 * $lake.value = ["a", "b", "c"];
 * $now.value = { id: "b", manualEntry: false };
 *
 * const result = fillSequentially(3, []);
 *
 * if (result[0].id !== "c") throw new Error("expected 'c'");
 * if (result[1].id !== "a") throw new Error("expected wrap around to 'a'");
 * if (result[2].id !== "b") throw new Error("expected 'b'");
 * ```
 *
 * @example Preserves existing manual entries
 * ```js
 * import { fillSequentially, $lake, $now } from "~/components/engine/queue/worker.js";
 *
 * $lake.value = ["a", "b", "c"];
 * $now.value = null;
 *
 * const future = [{ id: "manual", manualEntry: true }];
 * const result = fillSequentially(2, future);
 *
 * if (result[0].id !== "manual") throw new Error("manual entry should be preserved");
 * if (result.length !== 3) throw new Error("expected manual + 2 auto items");
 * ```
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
 *
 * @example Adds shuffled items to reach the fill amount
 * ```js
 * import { fillShuffle, $lake, $past } from "~/components/engine/queue/worker.js";
 *
 * $lake.value = ["a", "b", "c", "d", "e"];
 * $past.value = [];
 *
 * const result = fillShuffle(3, [], 0);
 *
 * if (result.length !== 3) throw new Error("expected 3 items");
 * if (!result.every((i) => i.manualEntry === false)) throw new Error("all items should be auto");
 * ```
 *
 * @example Only adds enough to reach the fill amount given existing auto items
 * ```js
 * import { fillShuffle, $lake, $past } from "~/components/engine/queue/worker.js";
 *
 * $lake.value = ["a", "b", "c", "d"];
 * $past.value = [];
 *
 * const existing = [
 *   { id: "x", manualEntry: false },
 *   { id: "y", manualEntry: false },
 * ];
 *
 * const result = fillShuffle(4, existing, 2);
 *
 * if (result.length !== 4) throw new Error("expected 4 total items (2 existing + 2 new)");
 * ```
 *
 * @example Does not add tracks that have already played or are now playing
 * ```js
 * import { fillShuffle, $lake, $past, $now } from "~/components/engine/queue/worker.js";
 *
 * $lake.value = ["a", "b", "c", "d"];
 * $past.value = [{ id: "a", manualEntry: false }];
 * $now.value = { id: "b", manualEntry: false };
 *
 * const result = fillShuffle(4, [], 0);
 *
 * if (result.some((i) => i.id === "a" || i.id === "b")) throw new Error("past and now tracks should be excluded");
 * ```
 *
 * @example Falls back to full lake when everything has been played
 * ```js
 * import { fillShuffle, $lake, $past } from "~/components/engine/queue/worker.js";
 *
 * $lake.value = ["a", "b"];
 * $past.value = [{ id: "a", manualEntry: false }, { id: "b", manualEntry: false }];
 *
 * const result = fillShuffle(2, [], 0);
 *
 * if (result.length !== 2) throw new Error("expected 2 items from full lake fallback");
 * ```
 */
export function fillShuffle(fillAmount, future, autoFutureCount) {
  const excludeIds = new Set($past.value.map((i) => i.id));
  if ($now.value) excludeIds.add($now.value.id);
  future.forEach((i) => excludeIds.add(i.id));

  let pool = $lake.value
    .filter((id) => !excludeIds.has(id))
    .map((id) => ({ id, manualEntry: false }));

  // Fallback: if everything has been played/is playing/is queued, use tracks not in past or now
  if (pool.length === 0) {
    const pastAndNowIds = new Set($past.value.map((i) => i.id));
    if ($now.value) pastAndNowIds.add($now.value.id);
    pool = $lake.value
      .filter((id) => !pastAndNowIds.has(id))
      .map((id) => ({ id, manualEntry: false }));
  }

  // Final fallback: everything has been played, use the full lake
  if (pool.length === 0) {
    pool = $lake.value.map((id) => ({ id, manualEntry: false }));
  }

  const poolSelection = arrayShuffle(pool).slice(
    0,
    Math.max(0, fillAmount - autoFutureCount),
  );

  return [...future, ...poolSelection];
}

/**
 * @param {Item[]} [future]
 *
 * @example Moves the first future item to now
 * ```ts
 * import { _shift, $future, $now } from "~/components/engine/queue/worker.js";
 * import type { Item } from "./types.d.ts"
 *
 * $now.value = null as null | Item;
 * $future.value = [{ id: "a", manualEntry: false }, { id: "b", manualEntry: false }];
 *
 * _shift();
 *
 * if ($now.value?.id !== "a") throw new Error("expected 'a' as now");
 * if ($future.value.length !== 1) throw new Error("expected 1 item remaining in future");
 * if ($future.value[0].id !== "b") throw new Error("expected 'b' remaining in future");
 * ```
 *
 * @example Moves previous now to past
 * ```js
 * import { _shift, $future, $now, $past } from "~/components/engine/queue/worker.js";
 *
 * $past.value = [];
 * $now.value = { id: "prev", manualEntry: false };
 * $future.value = [{ id: "next", manualEntry: false }];
 *
 * _shift();
 *
 * if ($now.value?.id !== "next") throw new Error("expected 'next' as now");
 * if ($past.value.length !== 1) throw new Error("expected 1 past item");
 * if ($past.value[0].id !== "prev") throw new Error("expected 'prev' in past");
 * ```
 *
 * @example Does nothing when future is empty
 * ```js
 * import { _shift, $future, $now } from "~/components/engine/queue/worker.js";
 *
 * $future.value = [];
 * $now.value = null;
 *
 * _shift();
 *
 * if ($now.value !== null) throw new Error("now should remain null");
 * ```
 */
export function _shift(future) {
  const n = $now.value;
  const f = future ?? $future.value;
  const v = f[0];

  if (!v) return;
  $now.value = v;
  if (n) $past.value = [...$past.value, n];
  $future.value = f.slice(1);
}
