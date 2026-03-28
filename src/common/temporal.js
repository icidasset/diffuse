export const Temporal = /** @type {any} */ (globalThis).Temporal ??
  (await import("temporal-polyfill")).Temporal;

/**
 * @param {string} a
 * @param {string} b
 */
export function compareTimestamps(a, b) {
  return Temporal.Instant.compare(
    Temporal.Instant.from(a),
    Temporal.Instant.from(b),
  );
}
