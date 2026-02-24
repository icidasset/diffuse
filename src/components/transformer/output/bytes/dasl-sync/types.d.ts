export type Container<T> = {
  /**
   * CID of the inventory,
   * which in turns represents the current state of the data.
   */
  cid: string;
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
