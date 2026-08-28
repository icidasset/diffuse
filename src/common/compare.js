import deepDiff from "@fry69/deep-diff";

/**
 * @param {any} a
 * @param {any} b
 *
 * @example Returns true for identical primitives and false for different ones
 * ```js
 * import { diff } from "~/common/compare.js";
 *
 * if (!diff(1, 1)) throw new Error("identical primitives should be equal");
 * if (diff(1, 2)) throw new Error("different primitives should not be equal");
 * ```
 *
 * @example Returns true for deeply equal objects and false for different ones
 * ```js
 * import { diff } from "~/common/compare.js";
 *
 * if (!diff({ a: 1, b: { c: 2 } }, { a: 1, b: { c: 2 } })) throw new Error("deeply equal objects should be equal");
 * if (diff({ a: 1 }, { a: 2 })) throw new Error("objects with different values should not be equal");
 * if (diff({ a: 1 }, { b: 1 })) throw new Error("objects with different keys should not be equal");
 * ```
 *
 * @example Returns true for identical arrays and false for different ones
 * ```js
 * import { diff } from "~/common/compare.js";
 *
 * if (!diff([1, 2, 3], [1, 2, 3])) throw new Error("identical arrays should be equal");
 * if (diff([1, 2], [1, 3])) throw new Error("arrays with different elements should not be equal");
 * ```
 */
export function diff(a, b) {
  return !deepDiff(a, b);
}

/**
 * @param {any} a
 * @param {any} b
 *
 * @example Returns true for same primitive value, false for different
 * ```js
 * import { strictEquality } from "~/common/compare.js";
 *
 * if (!strictEquality(42, 42)) throw new Error("same primitives should be strictly equal");
 * if (strictEquality(42, 43)) throw new Error("different primitives should not be strictly equal");
 * if (strictEquality(null, undefined)) throw new Error("null and undefined should not be strictly equal");
 * ```
 *
 * @example Returns false for same-content objects (reference inequality)
 * ```js
 * import { strictEquality } from "~/common/compare.js";
 *
 * if (strictEquality({ a: 1 }, { a: 1 })) throw new Error("different object references should not be strictly equal");
 * const obj = { a: 1 };
 * if (!strictEquality(obj, obj)) throw new Error("same object reference should be strictly equal");
 * ```
 */
export function strictEquality(a, b) {
  return a === b;
}
