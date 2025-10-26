import deepDiff from "@fry69/deep-diff";
import {
  endBatch,
  setActiveSub,
  signal as alienSignal,
  startBatch,
} from "alien-signals";

export * from "alien-signals";

/**
 * @import {Signal, SignalReader, SignalWriter} from "./signal.d.ts"
 */

/**
 * @param {function(): void} fn
 */
export const batch = (fn) => {
  startBatch();
  try {
    fn();
  } finally {
    endBatch();
  }
};

/**
 * @template T
 * @param {T} initialValue
 * @param {{ eager?: boolean }} [options]
 * @returns {Signal<T>}
 */
export function signal(initialValue, options) {
  const s = alienSignal(initialValue);
  if (options?.eager === true) {
    return _signal({
      get: () => s(),
      set: (v) => s(v),
    });
  }

  return _signal({
    get: () => s(),
    set: (b) => {
      const a = untracked(() => s());
      const diff = deepDiff(a, b);
      if (diff) s(b);
    },
  });
}

/**
 * @template T
 * @param {function(): T} fn
 * @returns {T}
 */
export const untracked = (fn) => {
  const sub = setActiveSub(void 0);
  try {
    return fn();
  } finally {
    setActiveSub(sub);
  }
};

/**
 * @template T
 * @param {{ get: SignalReader<T>; set: SignalWriter<T> }} _
 * @returns {Signal<T>}
 */
function _signal({ get, set }) {
  return {
    get,
    set,

    get value() {
      return get();
    },

    set value(v) {
      set(v);
    },
  };
}
