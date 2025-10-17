import deepDiff from "@fry69/deep-diff";
import { signal as alienSignal } from "alien-signals";

export * from "alien-signals";

/**
 * @import {Signal} from "./signal.d.ts"
 */

/**
 * @template T
 * @param {T} initialValue
 * @returns {Signal<T>}
 */
export function signal(initialValue) {
  const s = alienSignal(initialValue);
  const isPrimitive = Object(initialValue) !== initialValue;
  if (isPrimitive) return s;

  return /** @type {Signal<T>} */ ((b) => {
    const a = s();
    if (b === undefined) return a;

    const diff = deepDiff(a, b);
    if (diff) s(b);
  });
}
