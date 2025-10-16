import { announce, define } from "@common/worker.js";
import { effect, signal } from "@common/signal.js";
import { arrayShuffle } from "@common/index.js";

/**
 * @import {Item} from "./types.d.ts"
 * @import {Track} from "@component/core/types.d.ts"
 */

const QUEUE_SIZE = 25;

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

const future = signal(/** @type {Item[]} */ ([]));
const lake = signal(/** @type {Track[]} */ ([]));
const now = signal(/** @type {Item | null} */ (null));
const past = signal(/** @type {Item[]} */ ([]));

effect(() => announce("future", future()));
effect(() => announce("now", now()));
effect(() => announce("past", past()));

define("future", () => future());
define("now", () => now());
define("past", () => past());

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

define("add", add);
define("pool", pool);
define("shift", shift);
define("unshift", unshift);

/**
 * @param {Item[]} items
 */
function add(items) {
  future([...future(), ...items]);
}

/**
 * @param {Track[]} tracks
 */
function pool(tracks) {
  lake(tracks);

  // TODO: If the pool changes, only remove non-existing tracks
  //       instead of resetting the whole future queue.
  //
  //       What about past queue items?

  future(fill([]));

  // Automatically insert track if there isn't any
  if (!now()) return shift();
}

function shift() {
  const n = now();
  const f = future();

  now(f[0] ?? null);

  if (n) past([...past(), n]);
  future(fill(f.slice(1)));
}

function unshift() {
  const p = past();
  if (p.length === 0) return;

  const n = now();
  const [last] = p.splice(p.length - 1, 1);

  now(last ?? null);
  if (n) future([n, ...future()]);
}

////////////////////////////////////////////
// PRIVATE
////////////////////////////////////////////

/**
 * @param {Item[]} future
 * @returns {Item[]}
 */
function fill(future) {
  if (future.length >= QUEUE_SIZE) return future;

  /** @type {Track[]} */
  const pool = [];

  let p = new Set(past().map((t) => t.id));
  let reducedPool = pool;

  lake().forEach((track) => {
    if (p.has(track.id)) {
      p = p.difference(new Set(track.id));
    } else {
      pool.push(track);
    }
  });

  if (reducedPool.length === 0) {
    reducedPool = lake();
  }

  const poolSelection = arrayShuffle(reducedPool).slice(
    0,
    QUEUE_SIZE - future.length,
  );
  return [...future, ...poolSelection];
}
