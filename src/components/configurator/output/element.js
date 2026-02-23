import { BroadcastableDiffuseElement } from "@common/element.js";
import { batch, computed, signal, trigger } from "@common/signal.js";

/**
 * @import {DiffuseElement} from "@common/element.js"
 * @import {Facet, PlaylistItem, Theme, Track} from "@definitions/types.d.ts"
 * @import {OutputManagerDeputy, OutputElement} from "@components/output/types.d.ts"
 *
 * @import {OutputConfiguratorElement} from "./types.d.ts"
 */

/**
 * @typedef {OutputElement} Output
 */

const STORAGE_PREFIX = "diffuse/configurator/output";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {OutputConfiguratorElement}
 */
class OutputConfigurator extends BroadcastableDiffuseElement {
  static NAME = "diffuse/configurator/output";

  constructor() {
    super();

    /** @type {OutputManagerDeputy} */
    const manager = {
      facets: {
        collection: computed(() => {
          const out = this.#selected.value;
          if (out) return out.facets.collection();

          const def = this.#defaultOutput.value;
          if (def) return def.facets.collection();

          return this.#memory.facets.value;
        }),
        reload: () => {
          const def = this.#defaultOutput.value;
          if (def) def.facets.reload();

          const out = this.#selected.value;
          if (out) return out.facets.reload();

          return Promise.resolve();
        },
        save: async (newFacets) => {
          const out = this.#selected.value;
          if (out) return await out.facets.save(newFacets);

          const def = this.#defaultOutput.value;
          if (def) return await def.facets.save(newFacets);

          this.#memory.facets.value = newFacets;
        },
        state: computed(() => {
          const out = this.#selected.value;
          if (out) return out.facets.state();

          const def = this.#defaultOutput.value;
          if (def) return def.facets.state();

          return this.#setupFinished.value ? "loaded" : "sleeping";
        }),
      },
      playlistItems: {
        collection: computed(() => {
          const out = this.#selected.value;
          if (out) return out.playlistItems.collection();

          const def = this.#defaultOutput.value;
          if (def) return def.playlistItems.collection();

          return this.#memory.playlistItems.value;
        }),
        reload: () => {
          const def = this.#defaultOutput.value;
          if (def) def.playlistItems.reload();

          const out = this.#selected.value;
          if (out) return out.playlistItems.reload();

          return Promise.resolve();
        },
        save: async (newPlaylistItems) => {
          const out = this.#selected.value;
          if (out) return await out.playlistItems.save(newPlaylistItems);

          const def = this.#defaultOutput.value;
          if (def) return await def.playlistItems.save(newPlaylistItems);

          this.#memory.playlistItems.value = newPlaylistItems;
        },
        state: computed(() => {
          const out = this.#selected.value;
          if (out) return out.playlistItems.state();

          const def = this.#defaultOutput.value;
          if (def) return def.playlistItems.state();

          return this.#setupFinished.value ? "loaded" : "sleeping";
        }),
      },
      themes: {
        collection: computed(() => {
          const out = this.#selected.value;
          if (out) return out.themes.collection();

          const def = this.#defaultOutput.value;
          if (def) return def.themes.collection();

          return this.#memory.themes.value;
        }),
        reload: () => {
          const def = this.#defaultOutput.value;
          if (def) def.themes.reload();

          const out = this.#selected.value;
          if (out) return out.themes.reload();

          return Promise.resolve();
        },
        save: async (newThemes) => {
          const out = this.#selected.value;
          if (out) return await out.themes.save(newThemes);

          const def = this.#defaultOutput.value;
          if (def) return await def.themes.save(newThemes);

          this.#memory.themes.value = newThemes;
        },
        state: computed(() => {
          const out = this.#selected.value;
          if (out) return out.themes.state();

          const def = this.#defaultOutput.value;
          if (def) return def.themes.state();

          return this.#setupFinished.value ? "loaded" : "sleeping";
        }),
      },
      tracks: {
        collection: computed(() => {
          const out = this.#selected.value;
          if (out) return out.tracks.collection();

          const def = this.#defaultOutput.value;
          if (def) return def.tracks.collection();

          return this.#memory.tracks.value;
        }),
        reload: () => {
          const def = this.#defaultOutput.value;
          if (def) def.tracks.reload();

          const out = this.#selected.value;
          if (out) return out.tracks.reload();

          return Promise.resolve();
        },
        save: async (newTracks) => {
          const out = this.#selected.value;
          if (out) return await out.tracks.save(newTracks);

          const def = this.#defaultOutput.value;
          if (def) return await def.tracks.save(newTracks);

          this.#memory.tracks.value = newTracks;
        },
        state: computed(() => {
          const out = this.#selected.value;
          if (out) return out.tracks.state();

          const def = this.#defaultOutput.value;
          if (def) return def.tracks.state();

          return this.#setupFinished.value ? "loaded" : "sleeping";
        }),
      },

      // Other
      ready: computed(() => {
        const out = this.#selected.value;
        if (out) return out.ready();

        const def = this.#defaultOutput.value;
        if (def) return def.ready();

        return this.#setupFinished.value;
      }),
    };

    // Assign manager properties to class
    this.facets = manager.facets;
    this.playlistItems = manager.playlistItems;
    this.themes = manager.themes;
    this.tracks = manager.tracks;
    this.ready = manager.ready;
  }

  // SIGNALS

  #activated = signal(/** @type {Set<string>} */ (new Set()), { eager: true });

  #defaultOutput = signal(
    /** @type {Output | null | undefined} */ (undefined),
  );

  #memory = {
    facets: signal(/** @type {Facet[]} */ ([])),
    playlistItems: signal(/** @type {PlaylistItem[]} */ ([])),
    themes: signal(/** @type {Theme[]} */ ([])),
    tracks: signal(/** @type {Track[]} */ ([])),
  };

  #selected = signal(
    /** @type {Output | null | undefined} */ (undefined),
  );

  #setupFinished = signal(false);

  // STATE

  activated = this.#activated.get;
  selected = computed(() => this.#selected.value ?? null);

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(this.nameWithGroup, {
        selectOutput: {
          strategy: "replicate",
          fn: this.#selectOutput,
        },
      });

      if (actions) {
        this.#selectOutput = actions.selectOutput;
      }
    }

    // Super
    super.connectedCallback();

    // Outputs
    const def_ault = this.getAttribute("default");
    const selectedOutputId = localStorage.getItem(
      `${STORAGE_PREFIX}/selected/id`,
    );

    batch(() => {
      /** @type {Set<string>} */
      const activated = new Set();

      if (def_ault) {
        activated.add(def_ault);
      }

      if (selectedOutputId) {
        activated.add(selectedOutputId);
      }

      this.#activated.value = activated;
    });

    /** @type {Output | null} */
    const defaultOutput = def_ault ? await this.#findOutput(def_ault) : null;
    const selectedOutput = await this.#findOutput(selectedOutputId);

    batch(() => {
      this.#selected.value = selectedOutput;
      this.#defaultOutput.value = defaultOutput;
      this.#setupFinished.value = true;
    });
  }

  // MISC

  /**
   * @param {string | null} id
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

  /**
   * @param {string | null} id
   */
  #selectOutput = async (id) => {
    if (id) {
      this.#activated.value = new Set([...this.#activated.value.values(), id]);
    }

    this.#selected.value = await this.#findOutput(id);
  };

  /**
   * @override
   */
  dependencies = () => {
    return Object.fromEntries(
      Array.from(this.root().children).flatMap((element) => {
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
  };

  // ADDITIONAL ACTIONS

  deselect = async () => {
    localStorage.removeItem(`${STORAGE_PREFIX}/selected/id`);
    await this.#selectOutput(null);
  };

  options = async () => {
    const deps = this.dependencies();
    const entries = Object.entries(deps);

    return entries.map(([k, v]) => {
      return {
        id: k,
        label: v.label ?? v.getAttribute("label"),
        element: /** @type {OutputElement} */ (v),
      };
    });
  };

  /**
   * @param {string} id
   */
  select = async (id) => {
    localStorage.setItem(`${STORAGE_PREFIX}/selected/id`, id);
    await this.#selectOutput(id);
  };
}

export default OutputConfigurator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = OutputConfigurator;
export const NAME = "dc-output";

customElements.define(NAME, CLASS);
