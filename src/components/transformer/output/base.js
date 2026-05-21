import { BroadcastableDiffuseElement, query } from "~/common/element.js";
import { computed, signal } from "~/common/signal.js";

/**
 * @import { OutputElement, OutputManagerDeputy } from "@specs/components/output/types.d.ts"
 */

/**
 * @template [T=null]
 */
export class OutputTransformer extends BroadcastableDiffuseElement {
  // SIGNALS

  #output = signal(/** @type {OutputElement<T> | undefined} */ (undefined));
  #outputWhenDefined = Promise.withResolvers();

  output = {
    whenDefined: this.#outputWhenDefined.promise,
    signal: this.#output.get,
  };

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    /** @type {OutputElement<T>} */
    const output = query(this, "output-selector");

    // When defined
    customElements.whenDefined(output.localName).then(() => {
      this.#output.value = output;
      this.#outputWhenDefined.resolve(null);
    });
  }

  // MANAGER

  base() {
    /** @type {OutputManagerDeputy<T>} */
    const m = {
      facets: {
        collection: computed(() => {
          return this.output.signal()?.facets?.collection() ??
            { state: "loading" };
        }),
        reload: () => {
          return this.output.signal()?.facets?.reload() ??
            Promise.resolve();
        },
        save: async (newFacets) => {
          await this.output.whenDefined;
          await this.output.signal()?.facets.save(newFacets);
        },
      },
      playlistItems: {
        collection: computed(() => {
          return this.output.signal()?.playlistItems?.collection() ??
            { state: "loading" };
        }),
        reload: () => {
          return this.output.signal()?.playlistItems?.reload() ??
            Promise.resolve();
        },
        save: async (newPlaylistItems) => {
          await this.output.whenDefined;
          await this.output.signal()?.playlistItems.save(newPlaylistItems);
        },
      },
      settings: {
        collection: computed(() => {
          return this.output.signal()?.settings?.collection() ??
            { state: "loading" };
        }),
        reload: () => {
          return this.output.signal()?.settings?.reload() ??
            Promise.resolve();
        },
        save: async (newSettings) => {
          await this.output.whenDefined;
          await this.output.signal()?.settings.save(newSettings);
        },
      },
      tracks: {
        collection: computed(() => {
          return this.output.signal()?.tracks?.collection() ??
            { state: "loading" };
        }),
        reload: () => {
          return this.output.signal()?.tracks?.reload() ?? Promise.resolve();
        },
        save: async (newTracks) => {
          await this.output.whenDefined;
          await this.output.signal()?.tracks.save(newTracks);
        },
      },

      // Other non-data related state
      ready: computed(() => this.output.signal()?.ready() ?? false),
    };

    return m;
  }
}
