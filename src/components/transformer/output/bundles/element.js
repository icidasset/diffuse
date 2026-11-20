import { computed, signal } from "~/common/signal.js";
import {
  bundle,
  byField,
  byUriScheme,
  flattenBundles,
  isBundle,
} from "~/common/bundles.js";
import { OutputTransformer } from "../base.js";
import { defineElement, queryOptional } from "~/common/element.js";

/**
 * @import {BundleSelector} from "~/common/bundles.d.ts"
 * @import {InputElement} from "@specs/components/input/types.d.ts"
 * @import {Track} from "~/definitions/types.d.ts"
 */

/** @typedef {"collaborations" | "facets" | "playlistItems" | "settings" | "tracks"} CollectionName */

/**
 * The collections that have an obvious "smaller piece" and how to find it.
 *
 * Playlist items belong to a playlist. Tracks are grouped by the source of the
 * input that owns them (see {@link bySource}), falling back to their URI scheme
 * when no input provider is available.
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
 * - `tracks` by the source of the input that owns them
 *
 * A track's source is resolved through the input itself (an S3 bucket, a
 * Subsonic server, a WebDAV directory, a local folder, …). The input configurator
 * is found through the optional `input-selector` attribute, or the document's
 * `dc-input` when there is one. Without an input (for example in a standalone
 * page), tracks fall back to their URI scheme.
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
   * @override
   */
  static observedAttributes = ["output-selector", "input-selector"];

  /** Bumped when `input-selector` changes, so the bundle view re-resolves. */
  #inputsRevision = signal(0);

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

          // Re-resolve the input provider when `input-selector` is rewired.
          this.#inputsRevision.get();

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

  /**
   * @override
   * @param {string} name
   * @param {string} oldValue
   * @param {string} newValue
   */
  attributeChangedCallback(name, oldValue, newValue) {
    super.attributeChangedCallback(name, oldValue, newValue);

    if (name === "input-selector") {
      this.#inputsRevision.value = this.#inputsRevision.value + 1;
    }
  }

  /**
   * The input elements that can name a track's source.
   *
   * The explicit `input-selector` wins; otherwise the document's `dc-input`
   * configurator is used (the foundation mounts one). Returns `undefined` when
   * there is no usable provider, in which case tracks fall back to their URI
   * scheme.
   *
   * @returns {Record<string, InputElement> | undefined}
   */
  #inputs() {
    const provider = queryOptional(this, "input-selector") ??
      document.querySelector("dc-input");
    if (!provider) return undefined;

    const element = /** @type {any} */ (provider);

    if (typeof element.inputs === "function") {
      const inputs = element.inputs();
      return inputs && typeof inputs === "object" ? inputs : undefined;
    }

    if (
      typeof element.SCHEME === "string" &&
      typeof element.sources === "function"
    ) {
      return { [element.SCHEME]: /** @type {InputElement} */ (provider) };
    }

    return undefined;
  }

  /** @returns {BundleSelector<Track>} */
  #trackSelector() {
    const inputs = this.#inputs();
    return inputs ? bySource(inputs) : byUriScheme();
  }
}

/**
 * Derive a track's bundle key from the input that owns it.
 *
 * Each input is asked for the source of one track at a time, so the answer
 * always comes from that input's own parser. Two S3 buckets, two Subsonic
 * servers, two WebDAV directories or two local folders therefore stay distinct
 * even when they share a URI scheme. Tracks whose input cannot name a source
 * fall back to their scheme, so nothing is dropped from the bundle view.
 *
 * @param {Record<string, InputElement>} inputs
 * @returns {BundleSelector<Track>}
 */
function bySource(inputs) {
  return (track) => {
    const uri = track?.uri;
    if (typeof uri !== "string") return undefined;

    const colon = uri.indexOf(":");
    if (colon <= 0) return undefined;

    const scheme = uri.slice(0, colon);
    const input = inputs[scheme];

    if (!input || typeof input.sources !== "function") return scheme;

    try {
      const source = input.sources([track])[0];
      return typeof source?.uri === "string" && source.uri.length > 0
        ? source.uri
        : scheme;
    } catch {
      return scheme;
    }
  };
}

export default BundlesTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = BundlesTransformer;
export const NAME = "dto-bundles";

defineElement(NAME, CLASS);
