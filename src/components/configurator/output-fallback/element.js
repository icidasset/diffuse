import { DiffuseElement } from "@common/element.js";
import { batch, computed, signal } from "@common/signal.js";

/**
 * @import {Facet, Playlist, Theme, Track} from "@definitions/types.d.ts"
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
 * If none are ready, falls back to in-memory storage.
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
          const out = this.activeOutput();
          if (out) return out.facets.collection();
          return this.#memory.facets.value;
        }),
        reload: () => {
          const out = this.activeOutput();
          if (out) return out.facets.reload();
          return Promise.resolve();
        },
        save: async (newFacets) => {
          const out = this.activeOutput();
          if (out) return await out.facets.save(newFacets);
          this.#memory.facets.value = newFacets;
        },
        state: computed(() => {
          const out = this.activeOutput();
          if (out) return out.facets.state();
          return this.#setupFinished.value ? "loaded" : "sleeping";
        }),
      },
      playlists: {
        collection: computed(() => {
          const out = this.activeOutput();
          if (out) return out.playlists.collection();
          return this.#memory.playlists.value;
        }),
        reload: () => {
          const out = this.activeOutput();
          if (out) return out.playlists.reload();
          return Promise.resolve();
        },
        save: async (newPlaylists) => {
          const out = this.activeOutput();
          if (out) return await out.playlists.save(newPlaylists);
          this.#memory.playlists.value = newPlaylists;
        },
        state: computed(() => {
          const out = this.activeOutput();
          if (out) return out.playlists.state();
          return this.#setupFinished.value ? "loaded" : "sleeping";
        }),
      },
      themes: {
        collection: computed(() => {
          const out = this.activeOutput();
          if (out) return out.themes.collection();
          return this.#memory.themes.value;
        }),
        reload: () => {
          const out = this.activeOutput();
          if (out) return out.themes.reload();
          return Promise.resolve();
        },
        save: async (newThemes) => {
          const out = this.activeOutput();
          if (out) return await out.themes.save(newThemes);
          this.#memory.themes.value = newThemes;
        },
        state: computed(() => {
          const out = this.activeOutput();
          if (out) return out.themes.state();
          return this.#setupFinished.value ? "loaded" : "sleeping";
        }),
      },
      tracks: {
        collection: computed(() => {
          const out = this.activeOutput();
          if (out) return out.tracks.collection();
          return this.#memory.tracks.value;
        }),
        reload: () => {
          const out = this.activeOutput();
          if (out) return out.tracks.reload();
          return Promise.resolve();
        },
        save: async (newTracks) => {
          const out = this.activeOutput();
          if (out) return await out.tracks.save(newTracks);
          this.#memory.tracks.value = newTracks;
        },
        state: computed(() => {
          const out = this.activeOutput();
          if (out) return out.tracks.state();
          return this.#setupFinished.value ? "loaded" : "sleeping";
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

  #memory = {
    facets: signal(/** @type {Facet[]} */ ([])),
    playlists: signal(/** @type {Playlist[]} */ ([])),
    themes: signal(/** @type {Theme[]} */ ([])),
    tracks: signal(/** @type {Track[]} */ ([])),
  };

  #outputs = signal(/** @type {Output[]} */ ([]));
  #setupFinished = signal(false);

  // STATE

  /**
   * The first child output element whose `.ready()` returns `true`.
   */
  activeOutput = computed(() => {
    const outputs = this.#outputs.value;
    // TODO: Not sure if this will cause a signal change too often.
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
