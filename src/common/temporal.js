export const Temporal = /** @type {any} */ (globalThis).Temporal ??
  (await import("temporal-polyfill")).Temporal;

/**
 * @param {string} a
 * @param {string} b
 *
 * @example Returns 0 for equal timestamps, negative for earlier, positive for later
 * ```js
 * import { compareTimestamps } from "~/common/temporal.js";
 *
 * const eq = compareTimestamps("2024-01-01T00:00:00.000Z", "2024-01-01T00:00:00.000Z");
 * if (eq !== 0) throw new Error("equal timestamps should return 0");
 *
 * const earlier = compareTimestamps("2024-01-01T00:00:00.000Z", "2024-06-01T00:00:00.000Z");
 * if (earlier >= 0) throw new Error("earlier timestamp should return negative");
 *
 * const later = compareTimestamps("2024-06-01T00:00:00.000Z", "2024-01-01T00:00:00.000Z");
 * if (later <= 0) throw new Error("later timestamp should return positive");
 *
 * const ms = compareTimestamps("2024-01-01T00:00:00.001Z", "2024-01-01T00:00:00.000Z");
 * if (ms <= 0) throw new Error("millisecond precision should be respected");
 * ```
 */
export function compareTimestamps(a, b) {
  return Temporal.Instant.compare(
    Temporal.Instant.from(a),
    Temporal.Instant.from(b),
  );
}
