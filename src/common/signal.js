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
 * @param {{ compare?: (a: T, b: T) => boolean }} [options]
 * @returns {Signal<T>}
 */
export function signal(initialValue, options) {
  const s = alienSignal(initialValue);
  if (options?.compare) {
    const compare = options.compare;

    return _signal({
      get: () => s(),
      set: (b) => {
        const a = untracked(() => s());
        if (!compare(a, b)) s(b);
      },
    });
  }

  return _signal({
    get: () => s(),
    set: (v) => s(v),
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
 * @param {function(): Promise<T>} fn
 * @returns {Promise<T>}
 */
export const untrackedAsync = async (fn) => {
  const sub = setActiveSub(void 0);
  try {
    return await fn();
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
