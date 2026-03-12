export const Temporal = /** @type {any} */ (globalThis).Temporal ??
  (await import("temporal-polyfill")).Temporal;
