/**
 * A self-describing container of records in a DASL-sync output.
 *
 * Carries the lexicon NSID that produced `data` and the ordered history of schema
 * transitions, so the stored container is interpretable and migratable across
 * app versions without external knowledge.
 *
 * @template {Record<string, any>} T
 */
export type Container<T> = {
  /** The lexicon NSID the `data` records conform to. */
  $schema?: string;

  /**
   * Ordered history of schema transitions that produced the current shape;
   * each carries a portable lens document and the complement threaded through
   * the forward projection, so older apps can read and write newer data.
   */
  $schemaHistory?: {
    from: string;
    to: string;
    lens: unknown | null;
    complement?: Uint8Array | string | null;
  }[];

  /**
   * CID of the inventory,
   * which in turns represents the current state of the data.
   */
  cid?: string;
  data: T[];
  inventory: Inventory;
};

export type Inventory = {
  /**
   * `id` to `cid` map.
   */
  current: Record<string, string>;

  /**
   * List of `id`s
   */
  removed: string[];
};
