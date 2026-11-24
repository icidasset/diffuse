/**
 * Ephemeral items are device-local: they are kept in a separate store (see
 * `dtor-default`) and must never be written to a real output, published, or
 * exported.
 */

/**
 * Whether an item is ephemeral.
 *
 * @param {{ ephemeral?: boolean }} item
 * @returns {boolean}
 *
 * @example Flags ephemeral items
 * ```js
 * import { isEphemeral } from "~/common/ephemeral.js";
 *
 * if (!isEphemeral({ ephemeral: true })) throw new Error("expected ephemeral");
 * if (isEphemeral({})) throw new Error("expected durable");
 * ```
 */
export function isEphemeral(item) {
  return Boolean(item.ephemeral);
}

/**
 * The durable subset of a collection: everything an output is expected to
 * persist.
 *
 * @template {{ ephemeral?: boolean }} T
 * @param {T[]} items
 * @returns {T[]}
 *
 * @example Removes ephemeral items
 * ```js
 * import { durable } from "~/common/ephemeral.js";
 *
 * const items = [{ id: "a" }, { id: "b", ephemeral: true }];
 * const kept = durable(items);
 *
 * if (kept.length !== 1) throw new Error("expected one item");
 * if (kept[0].id !== "a") throw new Error("expected the durable item");
 * ```
 *
 * @example Returns an empty list when every item is ephemeral
 * ```js
 * import { durable } from "~/common/ephemeral.js";
 *
 * if (durable([{ ephemeral: true }]).length !== 0) {
 *   throw new Error("expected no durable items");
 * }
 * ```
 */
export function durable(items) {
  return items.filter((item) => !isEphemeral(item));
}
