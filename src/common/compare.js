import deepDiff from "@fry69/deep-diff";

/**
 * @param {any} a
 * @param {any} b
 */
export function diff(a, b) {
  return !deepDiff(a, b);
}

/**
 * @param {any} a
 * @param {any} b
 */
export function strictEquality(a, b) {
  return a === b;
}
