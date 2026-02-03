import { DiffuseElement, query } from "@common/element.js";
import { computed, signal } from "@common/signal.js";

/**
 * @import { OutputElement, OutputManagerDeputy } from "../../output/types.d.ts"
 */

/**
 * @template [T=null]
 */
export class OutputTransformer extends DiffuseElement {
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
    /** @type {OutputManagerDeputy<T | undefined>} */
    const m = {
      constituents: {
        collection: computed(() => {
          return this.output.signal()?.constituents?.collection();
        }),
        reload: () => {
          return this.output.signal()?.constituents?.reload() ??
            Promise.resolve();
        },
        save: async (newConstituents) => {
          if (newConstituents === undefined) return;
          await this.output.whenDefined;
          await this.output.signal()?.constituents.save(newConstituents);
        },
        state: computed(() => {
          return this.output.signal()?.constituents.state() ?? "sleeping";
        }),
      },
      tracks: {
        collection: computed(() => {
          return this.output.signal()?.tracks?.collection();
        }),
        reload: () => {
          return this.output.signal()?.tracks?.reload() ?? Promise.resolve();
        },
        save: async (newTracks) => {
          if (newTracks === undefined) return;
          await this.output.whenDefined;
          await this.output.signal()?.tracks.save(newTracks);
        },
        state: computed(() => {
          return this.output.signal()?.tracks.state() ?? "sleeping";
        }),
      },
    };

    return m;
  }
}
