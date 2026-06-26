import {
  effect as alienEffect,
  endBatch,
  setActiveSub,
  signal as alienSignal,
  startBatch,
} from "alien-signals";

export * from "alien-signals";

/**
 * `effect` wrapper that tolerates async effect bodies.
 *
 * alien-signals v3.2.0+ stores `e.cleanup = e.fn()` and invokes
 * `cleanup()` when the effect re-runs or is disposed. An async effect body
 * returns a `Promise`, which is not a function — calling it later throws
 * `TypeError: cleanup is not a function`. This wrapper normalises a
 * `Promise` (or any thenable) return to `undefined` so async effect bodies
 * are safe to use without manual try/catch around the return value.
 *
 * Sync effect bodies that return a real cleanup function are passed
 * through unchanged.
 *
 * @param {() => (unknown | Promise<unknown>)} fn
 * @returns {() => void}
 */
export function effect(fn) {
  return alienEffect(() => {
    const cleanup = fn();
    return cleanup && typeof /** @type {any} */ (cleanup).then === "function"
      ? /** @type {void} */ (undefined)
      : /** @type {(() => void) | void} */ (cleanup);
  });
}

/**
 * @import {Signal, SignalReader, SignalWriter} from "./signal.d.ts"
 */

/**
 * @param {function(): void} fn
 *
 * @example Defers effect execution until batch completes
 * ```js
 * import { signal, effect, batch } from "~/common/signal.js";
 *
 * const a = signal(0);
 * const b = signal(0);
 * const values = [0]; values.length = 0; // typed as number[]
 *
 * effect(() => { values.push(a.get() + b.get()); });
 *
 * const before = [...values]; // [0]
 * batch(() => { a.set(1); b.set(2); });
 *
 * if (before.join(",") !== "0") throw new Error("expected [0] before batch");
 * if (values.join(",") !== "0,3") throw new Error("expected exactly one update after batch, got " + values.join(","));
 * ```
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
 *
 * @example get/set and value getter/setter return and update the value
 * ```js
 * import { signal } from "~/common/signal.js";
 *
 * const num = signal(42);
 * if (num.get() !== 42) throw new Error("get should return initial value");
 * if (num.value !== 42) throw new Error("value getter should return initial value");
 *
 * num.set(99);
 * if (num.get() !== 99) throw new Error("get should return updated value");
 *
 * const str = signal("a");
 * str.value = "b";
 * if (str.value !== "b") throw new Error("value setter should update value");
 * ```
 *
 * @example compare option skips update when values are equal by custom comparator
 * ```js
 * import { signal, effect } from "~/common/signal.js";
 *
 * let runCount = 0;
 * const s = signal({ x: 1 }, { compare: (a, b) => a.x === b.x });
 *
 * effect(() => { s.get(); runCount++; });
 *
 * const before = runCount;
 * s.set({ x: 1 }); // same by compare
 * if (runCount !== before) throw new Error("effect should not re-run when value is equal by compare");
 *
 * s.set({ x: 2 }); // different by compare
 * if (runCount !== before + 1) throw new Error("effect should re-run when value differs by compare");
 * ```
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
 *
 * @example Reads a signal without tracking it as a dependency
 * ```js
 * import { signal, effect, untracked } from "~/common/signal.js";
 *
 * const a = signal(1);
 * const b = signal(10);
 * let runCount = 0;
 *
 * effect(() => {
 *   a.get(); // tracked
 *   untracked(() => b.get()); // not tracked
 *   runCount++;
 * });
 *
 * const before = runCount; // 1
 * b.set(20); // should NOT re-run effect
 * if (runCount !== before) throw new Error("untracked read should not trigger re-run");
 *
 * a.set(2); // SHOULD re-run effect
 * if (runCount !== before + 1) throw new Error("tracked read should trigger re-run");
 *
 * if (untracked(() => a.get()) !== 2) throw new Error("untracked should return the value");
 * ```
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
 *
 * @example Returns the resolved value from the async callback
 * ```js
 * import { untrackedAsync } from "~/common/signal.js";
 *
 * const result = await untrackedAsync(async () => 99);
 * if (result !== 99) throw new Error("untrackedAsync should return resolved value");
 * ```
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
