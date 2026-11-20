/**
 * A partition of a collection, keyed by some property of its items.
 *
 * Bundles are the unit Diffuse publishes to AT Protocol: one record per
 * playlist (`sh.diffuse.output.playlistItemBundle`) or per source
 * (`sh.diffuse.output.trackBundle`).
 *
 * @template T
 */
export type Bundle<T> = {
  /** Stable identifier derived from `key`. */
  id: string;

  /** The value the items were grouped by (a playlist name, a URI scheme, …). */
  key: string;

  /** The items in this bundle, in their original relative order. */
  items: T[];
};

/**
 * Return the key (or keys) of the bundle(s) an item belongs to.
 *
 * Returning `undefined`, `null`, `false` or an empty value excludes the item
 * from every bundle.
 *
 * @template T
 */
export type BundleSelector<T> = (
  item: T,
) => string | readonly string[] | null | undefined | false;

export type BundleOptions<T> = {
  /** How to group the items. */
  by: BundleSelector<T>;

  /**
   * How to derive a bundle's `id` from its `key`.
   *
   * Defaults to {@link bundleId}, which matches the derivation used for
   * Diffuse's AT Protocol record keys.
   */
  id?: (key: string) => string;

  /** Items for which this returns true are left out of every bundle. */
  skip?: (item: T) => boolean;
};
