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

/**
 * How long to wait for the backing output to become usable before showing an
 * empty library.
 *
 * A collection stays `loading` until it has an answer, which is what keeps the
 * UI from flashing empty. But if the output can never answer — signed out, or
 * no encryption key on this browser — waiting forever just spins.
 */
export const NOT_READY_GRACE_MS = 750;

/**
 * How long the first read of a collection may take before the UI shows what it
 * has (usually nothing) instead of waiting.
 *
 * The collections are local-first, so this only matters when the local cache is
 * empty and the output has to be read over the network.
 */
export const FIRST_READ_TIMEOUT_MS = 1_500;

/**
 * How long to wait for the local cache to answer. It reads through a worker, so
 * a stuck worker would otherwise leave every collection loading forever.
 */
export const LOCAL_READ_TIMEOUT_MS = 1_000;

/** IndexedDB prefix for the push cursor of each collection. */
export const IDB_PREFIX = "diffuse/transformer/output/bytes/automerge";
