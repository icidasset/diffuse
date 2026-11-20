/**
 * The userdata output method backed by Firkin.
 *
 * The output itself is a single self-contained element
 * (`~/components/output/raw/firkin/encrypted/incremental-automerge/element.js`);
 * this module only holds the identity the facets need to agree on: the option
 * id (persisted in `localStorage` as the selected output), the label the
 * connect facet waits for, the provider row the userdata facet shows, and the
 * log's namespace.
 *
 * The id is stable: an existing selection keeps resolving after the element was
 * merged from its former transformer chain.
 */

/** The connect facet this output points at. */
export const CONNECT_URL = "facets/connect/firkin/index.tile";

/**
 * @typedef {Object} FirkinMethod
 * @property {"automerge" | "wnfs"} kind - The output implementation.
 * @property {string} id - Stable slug; also the log namespace and id suffix.
 * @property {string} label - Output option label; must be unique.
 * @property {string} provider - Display name shared by a provider's methods;
 *   the userdata facet collapses them into one row.
 * @property {number} [keep] - Entries retained per device and collection. Must
 *   exceed the element's snapshot cadence (32). Only used by the Automerge
 *   output.
 */

/** @type {FirkinMethod[]} */
export const FIRKIN_METHODS = [
  {
    kind: "automerge",
    id: "firkin-automerge",
    label: "Firkin (Automerge)",
    provider: "Firkin",
    keep: 64,
  },
  {
    kind: "wnfs",
    id: "wnfs-firkin",
    label: "Firkin (WNFS)",
    provider: "Firkin",
  },
];

/**
 * The id of the selectable output option.
 *
 * @param {FirkinMethod} method
 */
export function optionId(method) {
  return `do-output__dc-output__${method.id}`;
}
