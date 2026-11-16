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
 * The signed content of a manifest.
 *
 * Anyone with the account's `/firkin` capability can write any registry name,
 * so a manifest has to say which device and log it belongs to and be signed by
 * that device's key. `seq` is monotonic per device, so replaying an old
 * manifest is detectable.
 */
export type LogManifestPayload = {
  /** Payload shape version. */
  v: number;
  /** Base64url-encoded collection name the manifest belongs to. */
  collection: string;
  /** Hex-encoded public key of the writer; also the ref suffix. */
  device: string;
  /** Monotonic per device, starting at 1. */
  seq: number;
  /** The writer's entries, in append order. */
  packs: LogEntry[];
};

/**
 * What is actually stored as a blob: the exact signed bytes plus their
 * signature, so verification never depends on re-encoding being canonical.
 */
export type LogManifestEnvelope = {
  p: Uint8Array;
  s: Uint8Array;
};

/** An entry's bytes, paired with the address they were read from. */
export type FirkinEntry = {
  cid: string;
  bytes: Uint8Array;
};

/**
 * The Firkin bytes output is an **append-only** store.
 *
 * `put` appends; what `get` returns is the element's `mode`:
 *
 * - `mode="value"` (default) — the newest entry, for whole-value formats.
 * - `mode="log"` — every entry concatenated, oldest first, for formats built
 *   from self-delimiting chunks.
 *
 * Consumers never need to know which member wrote what, or that the store is a
 * log at all: both modes are plain `OutputElement<Uint8Array>`.
 */
export type FirkinOutputElement = OutputElement<Uint8Array | undefined> & {
  /** `"value"` returns the newest entry; `"log"` returns every entry. */
  mode: "value" | "log";
  /**
   * How many entries to retain per log.
   *
   * Unset keeps everything. A value-shaped consumer only reads the newest
   * entry, so `keep="1"` bounds the per-device manifest without losing anything
   * it can see; a log-shaped consumer needs a window that still contains the
   * writer's periodic snapshots.
   */
  keep?: number;
  /**
   * Every entry for `name`, oldest first — the history behind `get`, for
   * tooling and inspection.
   */
  packs(name: string): Promise<FirkinEntry[]>;
};
