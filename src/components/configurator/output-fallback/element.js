import { DiffuseElement } from "@common/element.js";
import { batch, computed, signal } from "@common/signal.js";

/**
 * @import {OutputManagerDeputy, OutputElement} from "@components/output/types.d.ts"
 */

/**
 * @typedef {OutputElement} Output
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
 * @implements {OutputManagerDeputy}
 */
class OutputFallbackConfigurator extends DiffuseElement {
  static NAME = "diffuse/configurator/output-fallback";

  constructor() {
    super();

    /** @type {OutputManagerDeputy} */
    const manager = {
      facets: {
        collection: computed(() => {
          return this.activeOutput()?.facets.collection() ?? [];
        }),
        reload: () => {
          const out = this.activeOutput();
          if (out) return out.facets.reload();
          return Promise.resolve();
        },
        save: async (newFacets) => {
          await Promise.all(this.#outputs.value.map((o) => o.facets.save(newFacets)));
        },
        state: computed(() => {
          return this.activeOutput()?.facets.state() ?? "sleeping";
        }),
      },
      playlists: {
        collection: computed(() => {
          return this.activeOutput()?.playlists.collection() ?? [];
        }),
        reload: () => {
          const out = this.activeOutput();
          if (out) return out.playlists.reload();
          return Promise.resolve();
        },
        save: async (newPlaylists) => {
          await Promise.all(this.#outputs.value.map((o) => o.playlists.save(newPlaylists)));
        },
        state: computed(() => {
          return this.activeOutput()?.playlists.state() ?? "sleeping";
        }),
      },
      themes: {
        collection: computed(() => {
          return this.activeOutput()?.themes.collection() ?? [];
        }),
        reload: () => {
          const out = this.activeOutput();
          if (out) return out.themes.reload();
          return Promise.resolve();
        },
        save: async (newThemes) => {
          await Promise.all(this.#outputs.value.map((o) => o.themes.save(newThemes)));
        },
        state: computed(() => {
          return this.activeOutput()?.themes.state() ?? "sleeping";
        }),
      },
      tracks: {
        collection: computed(() => {
          return this.activeOutput()?.tracks.collection() ?? [];
        }),
        reload: () => {
          const out = this.activeOutput();
          if (out) return out.tracks.reload();
          return Promise.resolve();
        },
        save: async (newTracks) => {
          await Promise.all(this.#outputs.value.map((o) => o.tracks.save(newTracks)));
        },
        state: computed(() => {
          return this.activeOutput()?.tracks.state() ?? "sleeping";
        }),
      },

      // Other
      ready: this.ready,
    };

    this.facets = manager.facets;
    this.playlists = manager.playlists;
    this.themes = manager.themes;
    this.tracks = manager.tracks;
    this.ready = manager.ready;
  }

  // SIGNALS

  #outputs = signal(/** @type {Output[]} */ ([]));
  #setupFinished = signal(false);

  // STATE

  /**
   * The first child output element whose `.ready()` returns `true`.
   */
  activeOutput = computed(() => {
    const outputs = this.#outputs.value;
    for (const output of outputs) {
      if (output.ready()) return output;
    }
    return null;
  });

  ready = computed(() => {
    if (this.activeOutput()) return true;
    return this.#setupFinished.value;
  });

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();

    const children = Array.from(this.root().children);

    /** @type {Output[]} */
    const outputs = [];

    for (const el of children) {
      await customElements.whenDefined(el.localName);

      if ("nameWithGroup" in el && "tracks" in el) {
        outputs.push(/** @type {Output} */ (/** @type {unknown} */ (el)));
      }
    }

    batch(() => {
      this.#outputs.value = outputs;
      this.#setupFinished.value = true;
    });
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
