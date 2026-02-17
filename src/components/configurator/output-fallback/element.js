import { DiffuseElement } from "@common/element.js";
import { computed, signal } from "@common/signal.js";

/**
 * @import {OutputManagerDeputy, OutputElement} from "@components/output/types.d.ts"
 * @import {OutputFallbackConfiguratorElement} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Output fallback configurator.
 *
 * Checks child output elements in order and delegates
 * to the first one whose `.ready()` signal returns `true`.
 *
 * @template [Encoding=null]
 * @implements {OutputManagerDeputy<Encoding | undefined>}
 * @implements {OutputFallbackConfiguratorElement<Encoding>}
 */
class OutputFallbackConfigurator extends DiffuseElement {
  static NAME = "diffuse/configurator/output-fallback";

  constructor() {
    super();

    /** @type {OutputManagerDeputy<Encoding | undefined>} */
    const manager = {
      facets: {
        collection: computed(() => {
          return this.#activeOutput.value?.facets.collection();
        }),
        reload: () => {
          const out = this.#activeOutput.value;
          if (out) return out.facets.reload();
          return Promise.resolve();
        },
        save: async (newFacets) => {
          if (newFacets !== undefined) {
            await Promise.all(
              this.#outputs.map((o) => o.facets.save(newFacets)),
            );
          }
        },
        state: computed(() => {
          return this.#activeOutput.value?.facets.state() ?? "sleeping";
        }),
      },
      playlists: {
        collection: computed(() => {
          return this.#activeOutput.value?.playlists.collection();
        }),
        reload: () => {
          const out = this.#activeOutput.value;
          if (out) return out.playlists.reload();
          return Promise.resolve();
        },
        save: async (newPlaylists) => {
          if (newPlaylists !== undefined) {
            await Promise.all(
              this.#outputs.map((o) => o.playlists.save(newPlaylists)),
            );
          }
        },
        state: computed(() => {
          return this.#activeOutput.value?.playlists.state() ?? "sleeping";
        }),
      },
      themes: {
        collection: computed(() => {
          return this.#activeOutput.value?.themes.collection();
        }),
        reload: () => {
          const out = this.#activeOutput.value;
          if (out) return out.themes.reload();
          return Promise.resolve();
        },
        save: async (newThemes) => {
          if (newThemes !== undefined) {
            await Promise.all(
              this.#outputs.map((o) => o.themes.save(newThemes)),
            );
          }
        },
        state: computed(() => {
          return this.#activeOutput.value?.themes.state() ?? "sleeping";
        }),
      },
      tracks: {
        collection: computed(() => {
          return this.#activeOutput.value?.tracks.collection();
        }),
        reload: () => {
          const out = this.#activeOutput.value;
          if (out) return out.tracks.reload();
          return Promise.resolve();
        },
        save: async (newTracks) => {
          if (newTracks !== undefined) {
            await Promise.all(
              this.#outputs.map((o) => o.tracks.save(newTracks)),
            );
          }
        },
        state: computed(() => {
          return this.#activeOutput.value?.tracks.state() ?? "sleeping";
        }),
      },

      // Other
      ready: computed(() => {
        if (this.#activeOutput.value) return true;
        return this.#setupFinished.value;
      }),
    };

    this.facets = manager.facets;
    this.playlists = manager.playlists;
    this.themes = manager.themes;
    this.tracks = manager.tracks;
    this.ready = manager.ready;

    this.effect(this.#setActiveOutput);
  }

  #setActiveOutput = () => {
    const _trigger = this.#setupFinished.value;

    /** @type {OutputElement<Encoding> | null} */
    let activeOutput = null;

    for (const output of this.#outputs) {
      if (output.ready()) {
        activeOutput = output;
        break;
      }
    }

    this.#activeOutput.value = activeOutput;
  };

  // SIGNALS

  #activeOutput = signal(/** @type {OutputElement<Encoding> | null} */ (null), {
    eager: true,
  });
  #setupFinished = signal(false);

  // STATE

  #outputs = /** @type {OutputElement<Encoding>[]} */ ([]);

  activeOutput = this.#activeOutput.get;

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();

    const children = Array.from(this.root().children);

    /** @type {OutputElement<Encoding>[]} */
    const outputs = [];

    for (const el of children) {
      await customElements.whenDefined(el.localName);

      if ("nameWithGroup" in el && "tracks" in el) {
        outputs.push(
          /** @type {OutputElement<Encoding>} */ (/** @type {unknown} */ (el)),
        );
      }
    }

    this.#outputs = outputs;
    this.#setupFinished.value = true;
  }

  // MISC

  /**
   * @override
   */
  dependencies = () => {
    return Object.fromEntries(
      Array.from(this.root().children).flatMap((element) => {
        if (element.hasAttribute("id") === false) {
          console.warn(
            "Missing `id` for output-fallback configurator child element with `localName` '" +
              element.localName + "'",
          );
          return [];
        }

        const d = /** @type {DiffuseElement} */ (element);
        return [[d.id, d]];
      }),
    );
  };
}

export default OutputFallbackConfigurator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = OutputFallbackConfigurator;
export const NAME = "dc-output-fallback";

customElements.define(NAME, CLASS);
