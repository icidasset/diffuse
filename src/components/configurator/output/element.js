import { BroadcastableDiffuseElement, defineElement } from "~/common/element.js";
import { batch, computed, signal } from "~/common/signal.js";

/**
 * @import {DiffuseElement} from "~/common/element.js"
 * @import {Facet, PlaylistItem, Setting, Track} from "~/definitions/types.d.ts"
 * @import {OutputManagerDeputy, OutputElement} from "@specs/components/output/types.d.ts"
 *
 * @import {OutputConfiguratorElement} from "@specs/components/configurator/output/types.d.ts"
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
          if (this.hasDefault()) return { state: "loading" };

          return this.#setupFinished.value
            ? { state: "loaded", data: this.#memory.facets.value }
            : { state: "loading" };
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
      },
      playlistItems: {
        collection: computed(() => {
          const out = this.#selected.value;
          if (out) return out.playlistItems.collection();

          const def = this.#defaultOutput.value;
          if (def) return def.playlistItems.collection();
          if (this.hasDefault()) return { state: "loading" };

          return this.#setupFinished.value
            ? { state: "loaded", data: this.#memory.playlistItems.value }
            : { state: "loading" };
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
      },
      settings: {
        collection: computed(() => {
          const out = this.#selected.value;
          if (out) return out.settings.collection();

          const def = this.#defaultOutput.value;
          if (def) return def.settings.collection();
          if (this.hasDefault()) return { state: "loading" };

          return this.#setupFinished.value
            ? { state: "loaded", data: this.#memory.settings.value }
            : { state: "loading" };
        }),
        reload: () => {
          const def = this.#defaultOutput.value;
          if (def) def.settings.reload();

          const out = this.#selected.value;
          if (out) return out.settings.reload();

          return Promise.resolve();
        },
        save: async (newSettings) => {
          const out = this.#selected.value;
          if (out) return await out.settings.save(newSettings);

          const def = this.#defaultOutput.value;
          if (def) return await def.settings.save(newSettings);

          this.#memory.settings.value = newSettings;
        },
      },
      tracks: {
        collection: computed(() => {
          const out = this.#selected.value;
          if (out) return out.tracks.collection();

          const def = this.#defaultOutput.value;
          if (def) return def.tracks.collection();
          if (this.hasDefault()) return { state: "loading" };

          return this.#setupFinished.value
            ? { state: "loaded", data: this.#memory.tracks.value }
            : { state: "loading" };
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
    this.settings = manager.settings;
    this.tracks = manager.tracks;
    this.ready = manager.ready;

    // Effects

    /**
     * When there's a selected output and its collection changes,
     * save it to the default output or memory.
     */
    this.effect(() => {
      const out = this.#selected.value;
      if (!out) return;

      const col = out.facets.collection();
      if (col.state !== "loaded") return;

      const def = this.#defaultOutput.value;
      if (def) def.facets.save(col.data);
      else this.#memory.facets.set(col.data);
    });

    this.effect(() => {
      const out = this.#selected.value;
      if (!out) return;

      const col = out.playlistItems.collection();
      if (col.state !== "loaded") return;

      const def = this.#defaultOutput.value;
      if (def) def.playlistItems.save(col.data);
      else this.#memory.playlistItems.set(col.data);
    });

    this.effect(() => {
      const out = this.#selected.value;
      if (!out) return;

      const col = out.settings.collection();
      if (col.state !== "loaded") return;

      const def = this.#defaultOutput.value;
      if (def) def.settings.save(col.data);
      else this.#memory.settings.set(col.data);
    });

    this.effect(() => {
      const out = this.#selected.value;
      if (!out) return;

      const col = out.tracks.collection();
      if (col.state !== "loaded") return;

      const def = this.#defaultOutput.value;
      if (def) def.tracks.save(col.data);
      else this.#memory.tracks.set(col.data);
    });
  }

  // SIGNALS

  #activated = signal(/** @type {Set<string>} */ (new Set()));

  #defaultOutput = signal(
    /** @type {Output | null | undefined} */ (undefined),
  );

  #memory = {
    facets: signal(/** @type {Facet[]} */ ([])),
    playlistItems: signal(/** @type {PlaylistItem[]} */ ([])),
    settings: signal(/** @type {Setting[]} */ ([])),
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
      const actions = this.broadcast(this.identifier, {
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
    const selectedOutputId = this.#selectedOutputId();

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
      "identifier" in el === false ||
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

  #selectedOutputId() {
    return localStorage.getItem(`${STORAGE_PREFIX}/selected/id`);
  }

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

  hasDefault() {
    return this.hasAttribute("default");
  }

  hasSelected() {
    return this.#selectedOutputId() !== null;
  }

  loadSelected = async () => {
    const selectedOutput = await this.#findOutput(this.#selectedOutputId());
    this.#selected.value = selectedOutput;
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

  /**
   * @param {string} label
   * @returns {Promise<{ id: string, label: string, element: OutputElement }>}
   */
  waitForOption = (label) => {
    return new Promise((resolve) => {
      const check = async () => {
        const opt = (await this.options()).find((o) => o.label === label);
        if (opt) {
          observer.disconnect();
          resolve(opt);
        }
      };

      const observer = new MutationObserver(check);
      observer.observe(this, { childList: true });
      check();
    });
  };
}

export default OutputConfigurator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = OutputConfigurator;
export const NAME = "dc-output";

defineElement(NAME, CLASS);
