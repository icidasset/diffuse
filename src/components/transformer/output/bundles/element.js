import { computed } from "~/common/signal.js";
import {
  bundle,
  byField,
  byUriScheme,
  flattenBundles,
  isBundle,
} from "~/common/bundles.js";
import { OutputTransformer } from "../base.js";
import { defineElement } from "~/common/element.js";

/**
 * @import {BundleSelector} from "~/common/bundles.d.ts"
 * @import {Track} from "~/definitions/types.d.ts"
 */

/** @typedef {"collaborations" | "facets" | "playlistItems" | "settings" | "tracks"} CollectionName */

/**
 * The collections that have an obvious "smaller piece" and how to find it.
 *
 * Playlist items belong to a playlist. Tracks are grouped by their URI scheme
 * by default; a composer that knows about inputs can set `tracksBy` to
 * `bySourceProvider(inputs)` (see `~/common/bundles.js`) to group by the
 * input's source instead.
 *
 * The other collections are already one record per item (a facet, a
 * collaboration, a setting), so grouping them would only produce one-item
 * bundles.
 *
 * @type {Record<string, BundleSelector<any>>}
 */
const BUNDLEABLE = {
  playlistItems: byField("playlist"),
  tracks: byUriScheme(),
};

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Exposes an output's collections as bundles.
 *
 * A bundle is a group of items that belong together: one per playlist, or one
 * per track source. Bundles are the unit Diffuse publishes to AT Protocol, but
 * they are useful anywhere a consumer wants smaller, stable pieces instead of
 * one flat collection.
 *
 * The element bundles automatically wherever that is meaningful:
 *
 * - `playlistItems` by their `playlist`
 * - `tracks` by their URI scheme
 *
 * A composer that knows about inputs can set `tracksBy` to a selector that
 * groups tracks by their input source instead (for example
 * `bySourceProvider(inputs)` from `~/common/bundles.js`). The element itself
 * never looks for inputs.
 *
 * Every other collection passes through unchanged. A bundled collection is
 * returned from `collection()` as `Bundle<T>[]`; its `save()` accepts bundles
 * and flattens them before writing to the wrapped output.
 *
 * Because the bundleable collections change shape, this element is meant to sit
 * *between* a consumer and an output (for example, between the AT Protocol
 * publisher and the output the user selected), not inside the orchestrator's
 * main output chain where the UI expects flat collections.
 *
 * ```html
 * <dto-bundles output-selector="#do-output__output"></dto-bundles>
 * ```
 *
 * @extends {OutputTransformer}
 */
class BundlesTransformer extends OutputTransformer {
  static NAME = "diffuse/transformer/output/bundles";

  /**
   * Optional grouping rule for tracks, supplied by the composition layer.
   * When absent, tracks are grouped by their URI scheme.
   *
   * @type {BundleSelector<Track> | undefined}
   */
  tracksBy = undefined;

  constructor() {
    super();

    const base = this.base();

    /** @param {CollectionName} name */
    const wrap = (name) => {
      if (!BUNDLEABLE[name]) return base[name];

      return {
        ...base[name],
        collection: computed(() => {
          const collection = base[name].collection();
          if (collection.state !== "loaded") return collection;

          const data = Array.isArray(collection.data)
            ? /** @type {any[]} */ (collection.data)
            : [];

          const by = name === "tracks"
            ? this.#trackSelector()
            : BUNDLEABLE[name];
          const bundles = bundle(data, { by });

          // A key that matches nothing silently turns a populated collection
          // into an empty one. Both fields are required by their lexicons, so
          // this only happens with unexpected data — make it visible instead of
          // losing the items quietly.
          const bundled = bundles.reduce((n, b) => n + b.items.length, 0);
          if (data.length > 0 && bundled !== data.length) {
            console.warn(
              `Bundles: ${data.length - bundled} item(s) in '${name}' have no grouping key`,
            );
          }

          return /** @type {any} */ ({ state: "loaded", data: bundles });
        }),
        save: async (/** @type {any} */ data) => {
          const items = Array.isArray(data) ? data : [];

          // Only flatten when the caller actually passed bundles; a flat list
          // is still valid and should round-trip unchanged.
          if (items.length > 0 && items.every(isBundle)) {
            await base[name].save(flattenBundles(items));
          } else {
            await base[name].save(data);
          }
        },
      };
    };

    this.collaborations = base.collaborations;
    this.facets = base.facets;
    this.playlistItems = wrap("playlistItems");
    this.settings = base.settings;
    this.tracks = wrap("tracks");

    this.ready = base.ready;
  }

  /** Resolves the track grouping rule, preferring the composer's override. */
  #trackSelector() {
    const custom = this.tracksBy;
    if (!custom) return BUNDLEABLE.tracks;

    const fallback = BUNDLEABLE.tracks;

    /** @type {BundleSelector<Track>} */
    const selector = (track) => custom(track) ?? fallback(track);
    return selector;
  }
}

export default BundlesTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = BundlesTransformer;
export const NAME = "dto-bundles";

defineElement(NAME, CLASS);
