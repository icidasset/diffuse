import { DiffuseElement } from "@common/element.js";
import { batch, computed, signal } from "@common/signal.js";

/**
 * @import {Facet, Playlist, Theme, Track} from "@definitions/types.d.ts"
 * @import {OutputManagerDeputy, OutputElement} from "@components/output/types.d.ts"
 */

/**
 * @typedef {OutputElement} Output
 */

const STORAGE_PREFIX = "diffuse/configurator/output";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {OutputElement}
 */
class OutputConfigurator extends DiffuseElement {
  static NAME = "diffuse/configurator/output";

  constructor() {
    super();

    /** @type {OutputManagerDeputy} */
    const manager = {
      facets: {
        collection: computed(() => {
          const out = this.#selectedOutput.value;
          if (out) return out.facets.collection();

          const def = this.#defaultOutput.value;
          if (def) return def.facets.collection();

          return this.#memory.facets.value;
        }),
        reload: () => {
          const def = this.#defaultOutput.value;
          if (def) def.facets.reload();

          const out = this.#selectedOutput.value;
          if (out) return out.facets.reload();

          return Promise.resolve();
        },
        save: async (newFacets) => {
          const def = this.#defaultOutput.value;
          if (def) await def.facets.save(newFacets);

          const out = this.#selectedOutput.value;
          if (out) return await out.facets.save(newFacets);

          this.#memory.facets.value = newFacets;
        },
        state: computed(() => {
          const out = this.#selectedOutput.value;
          if (out) return out.facets.state();

          const def = this.#defaultOutput.value;
          if (def) return def.facets.state();

          return this.#setupFinished.value ? "loaded" : "sleeping";
        }),
      },
      themes: {
        collection: computed(() => {
          const out = this.#selectedOutput.value;
          if (out) return out.themes.collection();

          const def = this.#defaultOutput.value;
          if (def) return def.themes.collection();

          return this.#memory.themes.value;
        }),
        reload: () => {
          const def = this.#defaultOutput.value;
          if (def) def.themes.reload();

          const out = this.#selectedOutput.value;
          if (out) return out.themes.reload();

          return Promise.resolve();
        },
        save: async (newThemes) => {
          const def = this.#defaultOutput.value;
          if (def) await def.themes.save(newThemes);

          const out = this.#selectedOutput.value;
          if (out) return await out.themes.save(newThemes);

          this.#memory.themes.value = newThemes;
        },
        state: computed(() => {
          const out = this.#selectedOutput.value;
          if (out) return out.themes.state();

          const def = this.#defaultOutput.value;
          if (def) return def.themes.state();

          return this.#setupFinished.value ? "loaded" : "sleeping";
        }),
      },
      tracks: {
        collection: computed(() => {
          const out = this.#selectedOutput.value;
          if (out) return out.tracks.collection();

          const def = this.#defaultOutput.value;
          if (def) return def.tracks.collection();

          return this.#memory.tracks.value;
        }),
        reload: () => {
          const def = this.#defaultOutput.value;
          if (def) def.tracks.reload();

          const out = this.#selectedOutput.value;
          if (out) return out.tracks.reload();

          return Promise.resolve();
        },
        save: async (newTracks) => {
          const def = this.#defaultOutput.value;
          if (def) await def.tracks.save(newTracks);

          const out = this.#selectedOutput.value;
          if (out) return await out.tracks.save(newTracks);

          this.#memory.tracks.value = newTracks;
        },
        state: computed(() => {
          const out = this.#selectedOutput.value;
          if (out) return out.tracks.state();

          const def = this.#defaultOutput.value;
          if (def) return def.tracks.state();

          return this.#setupFinished.value ? "loaded" : "sleeping";
        }),
      },
      playlists: {
        collection: computed(() => {
          const out = this.#selectedOutput.value;
          if (out) return out.playlists.collection();

          const def = this.#defaultOutput.value;
          if (def) return def.playlists.collection();

          return this.#memory.playlists.value;
        }),
        reload: () => {
          const def = this.#defaultOutput.value;
          if (def) def.playlists.reload();

          const out = this.#selectedOutput.value;
          if (out) return out.playlists.reload();

          return Promise.resolve();
        },
        save: async (newPlaylists) => {
          const def = this.#defaultOutput.value;
          if (def) await def.playlists.save(newPlaylists);

          const out = this.#selectedOutput.value;
          if (out) return await out.playlists.save(newPlaylists);

          this.#memory.playlists.value = newPlaylists;
        },
        state: computed(() => {
          const out = this.#selectedOutput.value;
          if (out) return out.playlists.state();

          const def = this.#defaultOutput.value;
          if (def) return def.playlists.state();

          return this.#setupFinished.value ? "loaded" : "sleeping";
        }),
      },
    };

    // Assign manager properties to class
    this.facets = manager.facets;
    this.playlists = manager.playlists;
    this.themes = manager.themes;
    this.tracks = manager.tracks;
  }

  // SIGNALS

  #defaultOutput = signal(
    /** @type {Output | null | undefined} */ (undefined),
  );

  #memory = {
    facets: signal(/** @type {Facet[]} */ ([])),
    playlists: signal(/** @type {Playlist[]} */ ([])),
    themes: signal(/** @type {Theme[]} */ ([])),
    tracks: signal(/** @type {Track[]} */ ([])),
  };

  #selectedOutput = signal(
    /** @type {Output | null | undefined} */ (undefined),
  );

  #setupFinished = signal(false);

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();

    /** @type {Output | null | undefined} */
    let defaultOutput = undefined;

    const def_ault = this.getAttribute("default");
    if (def_ault) {
      defaultOutput = await this.#findOutput(def_ault);
    }

    const selectedOutput = await this.#findSelectedOutput();

    batch(() => {
      this.#defaultOutput.value = defaultOutput;
      this.#selectedOutput.value = selectedOutput;
      this.#setupFinished.value = true;
    });
  }

  // MISC

  /**
   * @param {string} id
   */
  async #findOutput(id) {
    const el = id ? this.root().querySelector(`#${id}`) : null;
    if (!el) return null;

    await customElements.whenDefined(el.localName);

    if (
      "nameWithGroup" in el === false ||
      "tracks" in el === false
    ) {
      return null;
    }

    return /** @type {Output} */ (/** @type {unknown} */ (el));
  }

  async #findSelectedOutput() {
    const id = localStorage.getItem(`${STORAGE_PREFIX}/selected/id`);
    if (id) return this.#findOutput(id);
    return undefined;
  }

  /**
   * @override
   */
  dependencies() {
    return Object.fromEntries(
      Array.from(this.children).flatMap((element) => {
        if (element.hasAttribute("id") === false) {
          console.warn(
            "Missing `id` for output configurator child element with `localName` '" +
              element.localName + "'",
          );
          return [];
        }

        const d = /** @type {DiffuseElement} */ (element);
        return [[d.id, d]];
      }),
    );
  }

  // ADDITIONAL ACTIONS

  async deselect() {
    localStorage.removeItem(`${STORAGE_PREFIX}/selected/id`);
    this.#selectedOutput.value = await this.#findSelectedOutput();
  }

  async options() {
    const deps = this.dependencies();
    const entries = Object.entries(deps);

    await Promise.all(
      entries.map(([_k, v]) => customElements.whenDefined(v.localName)),
    );

    return entries.map(([k, v]) => {
      return {
        id: k,
        label: v.label,
        element: v,
      };
    });
  }

  /**
   * @param {string} id
   */
  async select(id) {
    localStorage.setItem(`${STORAGE_PREFIX}/selected/id`, id);
    this.#selectedOutput.value = await this.#findSelectedOutput();
  }
}

export default OutputConfigurator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = OutputConfigurator;
export const NAME = "dc-output";

customElements.define(NAME, CLASS);
