import type { OutputElement } from "@specs/components/output/types.d.ts";

/**
 * One immutable log entry.
 *
 * `at` is the wall clock at append time. It exists so "the newest entry" is
 * meaningful across writers, whose manifests are stored independently. It only
 * has to be roughly right: a consumer that needs convergence merges every entry
 * rather than picking one.
 */
export type LogEntry = {
  /** Content address of the entry's bytes. */
  cid: string;
  /** Unix milliseconds. */
  at: number;
};

/**
 * One writer's index of its log entries, stored as a dag-cbor blob and pointed
 * at by the account registry. Entries are immutable; appending only rewrites
 * this manifest.
 */
export type LogManifest = {
  /** Manifest shape version. */
  v: number;
  /** The writer's entries, in append order. */
  packs: LogEntry[];
};

/** An entry's bytes, paired with the address they were read from. */
export type FirkinEntry = {
  cid: string;
  bytes: Uint8Array;
};

/**
 * The Firkin bytes output is an **append-only** log.
 *
 * - `put` appends.
 * - `get` returns the **newest entry**. That is only a complete value for
 *   writers that append whole values (a JSON collection, a DASL container). A
 *   writer that appends deltas — Automerge, for example — must read `packs()`
 *   and replay the entries instead, which is why `appends` exists.
 * - `packs` exposes the history.
 */
export type FirkinOutputElement = OutputElement<Uint8Array | undefined> & {
  /** Marks the append semantics for transformers that care. */
  appends: true;
  /**
   * How many entries to retain per log.
   *
   * Unset keeps everything, which is what delta-based consumers need. A
   * value-shaped transformer only reads the newest entry, so `keep="1"` bounds
   * the per-device manifest without losing anything it can see.
   */
  keep?: number;
  /** CIDs of every entry for `name`, oldest first. Reads manifests only. */
  entryCids(name: string): Promise<string[]>;
  /**
   * Entry bytes for `name`, oldest first. Pass `cids` to fetch only those; the
   * result keeps log order and skips entries whose blob is gone.
   */
  packs(name: string, cids?: string[]): Promise<FirkinEntry[]>;
  /** How many entries this browser has appended for `name`. */
  packCount(name: string): Promise<number>;
  /** Replace this browser's entries for `name` with a single snapshot. */
  compact(name: string, snapshot: Uint8Array): Promise<void>;
};
