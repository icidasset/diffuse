/** Collections every output stores. */
export const COLLECTIONS = /** @type {import("~/common/self-describing.js").CollectionName[]} */ ([
  "collaborations",
  "facets",
  "playlistItems",
  "settings",
  "tracks",
]);

/**
 * Append this many entries to a log before collapsing it into one snapshot.
 * Manifests are rewritten on every append, so keeping them short keeps saves
 * cheap.
 */
export const MAX_PACKS = 32;

/** IndexedDB prefix for the heads each collection was last pushed at. */
export const IDB_PREFIX = "diffuse/transformer/output/bytes/automerge";
