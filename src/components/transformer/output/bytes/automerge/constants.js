/** Collections every output stores. */
export const COLLECTIONS = /** @type {import("~/common/self-describing.js").CollectionName[]} */ ([
  "collaborations",
  "facets",
  "playlistItems",
  "settings",
  "tracks",
]);

/**
 * Write a whole document instead of a delta this often.
 *
 * The transformer only sees its output through the standard interface, so it
 * can't ask an output to compact a log. Instead it keeps the log self-sufficient
 * by periodically appending a snapshot: any window that still contains the last
 * snapshot can be replayed from scratch.
 */
export const SNAPSHOT_EVERY = 32;

/** IndexedDB prefix for the push cursor of each collection. */
export const IDB_PREFIX = "diffuse/transformer/output/bytes/automerge";
