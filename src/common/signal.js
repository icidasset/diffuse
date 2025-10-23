import deepDiff from "@fry69/deep-diff";
import { setActiveSub, signal as alienSignal } from "alien-signals";

export * from "alien-signals";

/**
 * @import {Signal, SignalReader, SignalWriter} from "./signal.d.ts"
 */

/**
 * @template T
 * @param {T} initialValue
 * @param {{ unbiased?: boolean }} [options]
 * @returns {Signal<T>}
 */
export function signal(initialValue, options) {
  const s = alienSignal(initialValue);
  const isPrimitive = initialValue !== null &&
    initialValue !== undefined &&
    Object(initialValue) !== initialValue;
  if (isPrimitive || options?.unbiased === true) {
    return _signal({
      get: () => s(),
      set: (v) => s(v),
    });
  }

  return _signal({
    get: () => s(),
    set: (b) => {
      const a = s();
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
