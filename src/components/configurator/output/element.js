import { BroadcastableDiffuseElement, defineElement } from "~/common/element.js";
import { batch, computed, signal } from "~/common/signal.js";

import { PRELUDE_BASE, STARTING_SET_URIS } from "~/common/facets/constants.js";
import { buildFacets } from "~/common/facets/utils.js";
import {
  data as outputData,
  isOutputInitialized,
  markOutputInitialized,
} from "~/common/output.js";

/**
 * @import {DiffuseElement} from "~/common/element.js"
 * @import {Collaboration, Facet, PlaylistItem, Setting, Track} from "~/definitions/types.d.ts"
 * @import {OutputManagerDeputy, OutputElement} from "@specs/components/output/types.d.ts"
 *
 * @import {OutputConfiguratorElement} from "@specs/components/configurator/output/types.d.ts"
 */

/**
 * @typedef {OutputElement} Output
 */

const STORAGE_PREFIX = "diffuse/configurator/output";

const COLLECTIONS = /** @type {const} */ ([
  "collaborations",
  "facets",
  "playlistItems",
  "settings",
  "tracks",
]);

/**
 * An empty indexed-db output reports `loaded` with `undefined` data, so treat
 * anything that isn't an array as an empty collection.
 *
 * @param {unknown} data
 * @returns {any[]}
 */
function asArray(data) {
  return Array.isArray(data) ? data : [];
}

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
      collaborations: {
        collection: computed(() => {
          const out = this.#selected.value;
          if (out) return out.collaborations.collection();

          const def = this.#defaultOutput.value;
          if (def) return def.collaborations.collection();
          if (this.hasDefault()) return { state: "loading" };

          return this.#setupFinished.value
            ? { state: "loaded", data: this.#memory.collaborations.value }
            : { state: "loading" };
        }),
        reload: () => {
          const def = this.#defaultOutput.value;
          if (def) def.collaborations.reload();

          const out = this.#selected.value;
          if (out) return out.collaborations.reload();

          return Promise.resolve();
        },
        save: async (newCollaborations) => {
          const out = this.#selected.value;
          if (out) return await out.collaborations.save(newCollaborations);

          const def = this.#defaultOutput.value;
          if (def) return await def.collaborations.save(newCollaborations);

          this.#memory.collaborations.value = newCollaborations;
        },
      },
      facets: {
        collection: computed(() => {
          const out = this.#selected.value;
          if (out) return out.facets.collection();

          const def = this.#defaultOutput.value;
          if (def) {
            const col = def.facets.collection();

            // Recover from the bootstrap deadlock: a non-default output is
            // selected (but its elements aren't created yet, hence `out` is
            // null) and the local bucket has no prelude facets to inject the
            // output-bundle prelude from. Seed the base infrastructure
            // preludes so the page can bootstrap the selected output.
            if (col.state === "loaded" && this.#needsPreludeRecovery(col.data)) {
              return {
                state: "loaded",
                data: [...col.data, ...buildFacets(PRELUDE_BASE)],
              };
            }

            return col;
          }

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
    this.collaborations = manager.collaborations;
    this.facets = manager.facets;
    this.playlistItems = manager.playlistItems;
    this.settings = manager.settings;
    this.tracks = manager.tracks;
    this.ready = manager.ready;

    // Effects

    /**
     * When there's a selected output and its collection changes,
     * save it to the default output or memory.
     *
     * An empty selected output that has never been initialized is skipped:
     * its data is about to be carried over from the default (see below), and
     * mirroring the empty value would erase the local copy first.
     */
    this.effect(() => {
      const out = this.#selected.value;
      if (!out) return;

      const col = out.collaborations.collection();
      if (col?.state !== "loaded") return;

      const def = this.#defaultOutput.value;
      if (def) {
        if (!this.#shouldMirror(out)) return;
        def.collaborations.save(col.data);
      } else {
        this.#memory.collaborations.set(col.data);
      }
    });

    this.effect(() => {
      const out = this.#selected.value;
      if (!out) return;

      const col = out.facets.collection();
      if (col?.state !== "loaded") return;

      const def = this.#defaultOutput.value;
      if (def) {
        if (!this.#shouldMirror(out)) return;
        def.facets.save(col.data);
      } else {
        this.#memory.facets.set(col.data);
      }
    });

    this.effect(() => {
      const out = this.#selected.value;
      if (!out) return;

      const col = out.playlistItems.collection();
      if (col?.state !== "loaded") return;

      const def = this.#defaultOutput.value;
      if (def) {
        if (!this.#shouldMirror(out)) return;
        def.playlistItems.save(col.data);
      } else {
        this.#memory.playlistItems.set(col.data);
      }
    });

    this.effect(() => {
      const out = this.#selected.value;
      if (!out) return;

      const col = out.settings.collection();
      if (col?.state !== "loaded") return;

      const def = this.#defaultOutput.value;
      if (def) {
        if (!this.#shouldMirror(out)) return;
        def.settings.save(col.data);
      } else {
        this.#memory.settings.set(col.data);
      }
    });

    this.effect(() => {
      const out = this.#selected.value;
      if (!out) return;

      const col = out.tracks.collection();
      if (col?.state !== "loaded") return;

      const def = this.#defaultOutput.value;
      if (def) {
        if (!this.#shouldMirror(out)) return;
        def.tracks.save(col.data);
      } else {
        this.#memory.tracks.set(col.data);
      }
    });

    /**
     * Copy the default (Local) output's contents into a selected output that
     * has never been used.
     *
     * This is what makes a new remote bucket start with the data already
     * stored locally instead of an empty library. Facets fall back to the
     * starting set when the local store has none, matching the initial-contents
     * refiner.
     */
    this.effect(() => {
      const out = this.#selected.value;
      const def = this.#defaultOutput.value;
      if (!out || !def || out === def) return;

      const id = out.id;
      if (isOutputInitialized(id) || this.#carryingOver.has(id)) return;
      if (!this.#isEmptyAndUninitialized(out)) return;

      this.#carryingOver.add(id);
      void this.#carryOver(out, def, id);
    });
  }

  /**
   * Whether the selected output is completely empty and has never been used.
   * Only then should its contents be carried over from the default output.
   *
   * @param {Output} out
   * @returns {boolean}
   */
  #isEmptyAndUninitialized(out) {
    if (isOutputInitialized(out.id)) return false;

    for (const name of COLLECTIONS) {
      const collection = out[name].collection();
      // A transformer can still be mid-evaluation when a dependency it sets
      // during its own first read flushes this effect. Treat that as "not
      // ready yet"; the change that caused it re-runs this effect.
      if (!collection || collection.state !== "loaded") return false;
      if (asArray(collection.data).length > 0) return false;
    }

    return true;
  }

  /**
   * Mirroring a selected output into the default store is only safe once the
   * output has been used. Before that, an empty collection would overwrite the
   * local copy it is about to be seeded from.
   *
   * @param {Output} out
   * @returns {boolean}
   */
  #shouldMirror(out) {
    return isOutputInitialized(out.id) || this.#hasData(out);
  }

  /**
   * @param {Output} out
   * @returns {boolean}
   */
  #hasData(out) {
    return COLLECTIONS.some((name) => {
      const collection = out[name].collection();
      return collection?.state === "loaded" &&
        asArray(collection.data).length > 0;
    });
  }

  /**
   * Read the default output and write it into an empty selected output. Runs
   * once per output; the initialized flag prevents a later carry-over after
   * the user intentionally emptied it.
   *
   * @param {Output} out
   * @param {Output} def
   * @param {string} id
   */
  async #carryOver(out, def, id) {
    try {
      const data = {
        collaborations: asArray(await outputData(def.collaborations)),
        facets: asArray(await outputData(def.facets)),
        playlistItems: asArray(await outputData(def.playlistItems)),
        settings: asArray(await outputData(def.settings)),
        tracks: asArray(await outputData(def.tracks)),
      };

      // Nothing stored locally yet (a fresh install): leave the output
      // uninitialized so the initial-contents refiner keeps showing the
      // starting set.
      if (!Object.values(data).some((items) => items.length > 0)) return;

      await Promise.all([
        out.collaborations.save(data.collaborations),
        out.facets.save(
          data.facets.length > 0 ? data.facets : buildFacets(STARTING_SET_URIS),
        ),
        out.playlistItems.save(data.playlistItems),
        out.settings.save(data.settings),
        out.tracks.save(data.tracks),
      ]);

      markOutputInitialized(id);
    } catch (err) {
      console.error("Could not carry the local output over:", err);
    } finally {
      this.#carryingOver.delete(id);
    }
  }

  // SIGNALS

  #activated = signal(/** @type {Set<string>} */ (new Set()));

  #defaultOutput = signal(
    /** @type {Output | null | undefined} */ (undefined),
  );

  #memory = {
    collaborations: signal(/** @type {Collaboration[]} */ ([])),
    facets: signal(/** @type {Facet[]} */ ([])),
    playlistItems: signal(/** @type {PlaylistItem[]} */ ([])),
    settings: signal(/** @type {Setting[]} */ ([])),
    tracks: signal(/** @type {Track[]} */ ([])),
  };

  #selected = signal(
    /** @type {Output | null | undefined} */ (undefined),
  );

  /**
   * Output ids whose contents are being copied from the default output, so the
   * effect above doesn't start a second copy while the first is in flight.
   *
   * @type {Set<string>}
   */
  #carryingOver = new Set();

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
   * Whether the local bucket needs the base infrastructure preludes seeded
   * into the fallback.
   *
   * @param {Facet[]} localFacets
   * @returns {boolean}
   */
  #needsPreludeRecovery(localFacets) {
    const selectedId = this.#selectedOutputId();
    const defaultId = this.getAttribute("default");

    // Only a non-default selection depends on the prelude to be created.
    if (!selectedId || selectedId === defaultId) return false;

    // If the local bucket already has preludes, the normal path works.
    return !localFacets.some((f) => f.kind === "prelude");
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
