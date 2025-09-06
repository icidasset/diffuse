/** The counter that is incremented for `cid()` */
let _cid = 0;

/**
 * Get an auto-incrementing client-side ID value.
 * IDs are NOT guaranteed to be stable across page refreshes.
 */
export const cid = (): string => `cid${_cid++}`;

/** Index an iterable of items by key, returning a map. */
export const index = <Key, Item>(
  iter: Iterable<Item>,
  getKey: (item: Item) => Key,
): Map<Key, Item> => {
  const indexed = new Map<Key, Item>();
  for (const item of iter) {
    indexed.set(getKey(item), item);
  }
  return indexed;
};

/** An item that exposes an ID field that is unique within its collection */
export interface Identifiable {
  id: any;
}

export const getId = <Key, Item extends Identifiable>(item: Item) => item.id;

/** Index a collection by ID */
export const indexById = <Key, Item extends Identifiable>(iter: Iterable<Item>): Map<Key, Item> =>
  index(iter, getId);
