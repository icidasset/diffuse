/**
 * The output combinations that use Firkin as their backing store.
 *
 * Each method owns a `namespace`, which is what keeps them apart: separate
 * registry manifests (`diffuse.output.firkin.<ns>`), separate rollback floors,
 * separate hint channels, separate Automerge actor and push cursor, and a
 * separate local `dop-indexed-db` cache. They share the account, the blobs
 * (content-addressed, so identical entries coalesce) and the encryption key.
 *
 * Adding a combination is one entry here — the output bundle derives every
 * element id from it, and the connect page offers a chooser.
 *
 * Tag names stay as strings so importing this module doesn't pull the model
 * elements (the Automerge WASM and the Firkin SDK) into every page.
 */

export const FIRKIN_OUTPUT_NAME = "dob-firkin";
export const ENCRYPT_NAME = "dtob-encrypt";
export const AUTOMERGE_NAME = "dtob-automerge";

/** The connect facet these methods point at. */
export const CONNECT_URL = "facets/connect/firkin/index.tile";

/**
 * @typedef {Object} FirkinMethod
 * @property {string} id - Stable slug; also the namespace and id suffix.
 * @property {string} label - Output option label; must be unique.
 * @property {string} provider - Display name shared by a provider's methods;
 *   the userdata facet collapses them into one row.
 * @property {"log" | "value"} mode - What the backing output's `get` returns.
 * @property {number} keep - Entries retained per device and collection.
 * @property {string[]} chain - Transformers above the log, outermost last.
 */

/** @type {FirkinMethod[]} */
export const FIRKIN_METHODS = [
  {
    id: "firkin-automerge",
    label: "Firkin (Automerge)",
    provider: "Firkin",
    // The log is replayed, so a save appends only the changes since the last
    // push. `keep` must exceed the transformer's snapshot cadence (32).
    mode: "log",
    keep: 64,
    chain: [ENCRYPT_NAME, AUTOMERGE_NAME],
  },
];

/**
 * The id of the backing output for a method.
 *
 * @param {FirkinMethod} method
 */
export function storageId(method) {
  return `do-output__${FIRKIN_OUTPUT_NAME}--${method.id}`;
}

/**
 * The id of one transformer in the chain.
 *
 * @param {FirkinMethod} method
 * @param {string} name
 */
export function stepId(method, name) {
  return `do-output__${name}--${method.id}`;
}

/**
 * The id of the selectable option (the outermost transformer).
 *
 * @param {FirkinMethod} method
 */
export function optionId(method) {
  return `do-output__dc-output__${method.id}`;
}

/**
 * @param {string} id
 * @returns {FirkinMethod | undefined}
 */
export function methodForOption(id) {
  return FIRKIN_METHODS.find((method) => optionId(method) === id);
}
