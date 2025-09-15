import { getTransferables } from "@okikio/transferables";

import type { Track } from "@applets/core/types.js";
import type { Item, State } from "./types";
import { arrayShuffle, postMessages, provide } from "@scripts/common.ts";

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const actions = {
  add,
  pool,
  shift,
  unshift,
};

const { ports, tasks } = provide({
  actions,
  tasks: { ...actions, data },
});

export type Actions = typeof actions;
export type Tasks = typeof tasks;

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

const QUEUE_SIZE = 25;

const _internal: Record<string, { pool: Track[] }> = {};
const _state: Record<string, State> = {};

function data(groupId: string) {
  return state(groupId);
}

function emptyState(groupId: string): State {
  return {
    future: [],
    now: null,
    past: [],
  };
}

function notify(groupId: string) {
  const d = data(groupId);

  postMessages({
    data: {
      type: "data",
      data: d,
      groupId,
    },
    ports: ports.applets,
    transfer: getTransferables(d),
  });
}

function internal(groupId: string) {
  _internal[groupId] ??= { pool: [] };
  return _internal[groupId];
}

function state(groupId: string) {
  _state[groupId] ??= emptyState(groupId);
  return _state[groupId];
}

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

function add({ groupId, items }: { groupId: string; items: Item[] }) {
  state(groupId).future = [...state(groupId).future, ...items];
  notify(groupId);
}

function pool({ groupId, tracks }: { groupId: string; tracks: Track[] }) {
  internal(groupId).pool = tracks;
  const queue = state(groupId);

  // TODO: If the pool changes, only remove non-existing tracks
  //       instead of resetting the whole future queue.
  //
  //       What about past queue items?

  queue.future = [];
  fill(groupId);

  // Automatically insert track if there isn't any
  if (!queue.now) return shift({ groupId });
  else notify(groupId);
}

function shift({ groupId }: { groupId: string }) {
  const queue = state(groupId);
  const now = queue.future[0] ?? null;
  queue.now = now;

  queue.future = queue.future.slice(1);
  queue.past = now ? [...queue.past, now] : queue.past;

  fill(groupId);
}

function unshift({ groupId }: { groupId: string }) {
  const queue = state(groupId);
  if (queue.past.length === 0) return;

  const [last] = queue.past.splice(queue.past.length - 1, 1);
  const now = last ?? null;

  queue.now = now;
  queue.future = now ? [now, ...queue.future] : queue.future;

  notify(groupId);
}

// 🛠️

// TODO: Most likely there's a more performant solution
function fill(groupId: string) {
  const queue = state(groupId);
  if (queue.future.length >= QUEUE_SIZE) return;

  const pool: Track[] = [];

  let past = new Set(queue.past.map((t) => t.id));
  let reducedPool = pool;

  internal(groupId).pool.forEach((track: Track) => {
    if (past.has(track.id)) {
      past = past.difference(new Set(track.id));
    } else {
      pool.push(track);
    }
  });

  if (reducedPool.length === 0) {
    reducedPool = internal(groupId).pool;
  }

  const poolSelection = arrayShuffle(reducedPool).slice(0, QUEUE_SIZE - queue.future.length);
  add({ groupId, items: poolSelection });
}
