import { BroadcastableDiffuseElement, defineElement } from "~/common/element.js";
import { batch, computed, signal } from "~/common/signal.js";

import { PRELUDE_BASE, STARTING_SET_URIS } from "~/common/facets/constants.js";
import { buildFacets } from "~/common/facets/utils.js";

/**
 * @import {DiffuseElement} from "~/common/element.js"
 * @import {Collaboration, Facet, PlaylistItem, Setting, Track} from "~/definitions/types.d.ts"
 * @import {OutputManagerDeputy, OutputElement} from "@specs/components/output/types.d.ts"
 *
 * @import {OutputConfiguratorElement} from "@specs/components/configurator/output/types.d.ts"
 * @import {CollectionName} from "~/common/self-describing.js"
 */

/**
 * @typedef {OutputElement} Output
 */

/** The five collections every output persists. */
const COLLECTIONS = /** @type {CollectionName[]} */ ([
  "collaborations",
  "facets",
  "playlistItems",
  "settings",
  "tracks",
]);

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
      collaborations: {
        collection: computed(() => {
          const out = this.#activeOutput();
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
          const out = this.#activeOutput();
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
          const out = this.#activeOutput();
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
          const out = this.#activeOutput();
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
          const out = this.#activeOutput();
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
     */
    this.effect(() => {
      if (this.#switching.value) return;

      const out = this.#selected.value;
      if (!out) return;

      // Only mirror an output that can actually read. An unusable output (e.g.
      // signed out) reports an empty library, and mirroring that would wipe
      // the local copy every other output falls back to.
      if (!out.ready()) return;

      const col = out.collaborations.collection();
      if (col.state !== "loaded") return;

      const def = this.#defaultOutput.value;
      if (def) def.collaborations.save(col.data);
      else this.#memory.collaborations.set(col.data);
    });

    this.effect(() => {
      if (this.#switching.value) return;

      const out = this.#selected.value;
      if (!out) return;

      // Only mirror an output that can actually read. An unusable output (e.g.
      // signed out) reports an empty library, and mirroring that would wipe
      // the local copy every other output falls back to.
      if (!out.ready()) return;

      const col = out.facets.collection();
      if (col.state !== "loaded") return;

      const def = this.#defaultOutput.value;
      if (def) def.facets.save(col.data);
      else this.#memory.facets.set(col.data);
    });

    this.effect(() => {
      if (this.#switching.value) return;

      const out = this.#selected.value;
      if (!out) return;

      // Only mirror an output that can actually read. An unusable output (e.g.
      // signed out) reports an empty library, and mirroring that would wipe
      // the local copy every other output falls back to.
      if (!out.ready()) return;

      const col = out.playlistItems.collection();
      if (col.state !== "loaded") return;

      const def = this.#defaultOutput.value;
      if (def) def.playlistItems.save(col.data);
      else this.#memory.playlistItems.set(col.data);
    });

    this.effect(() => {
      if (this.#switching.value) return;

      const out = this.#selected.value;
      if (!out) return;

      // Only mirror an output that can actually read. An unusable output (e.g.
      // signed out) reports an empty library, and mirroring that would wipe
      // the local copy every other output falls back to.
      if (!out.ready()) return;

      const col = out.settings.collection();
      if (col.state !== "loaded") return;

      const def = this.#defaultOutput.value;
      if (def) def.settings.save(col.data);
      else this.#memory.settings.set(col.data);
    });

    this.effect(() => {
      if (this.#switching.value) return;

      const out = this.#selected.value;
      if (!out) return;

      // Only mirror an output that can actually read. An unusable output (e.g.
      // signed out) reports an empty library, and mirroring that would wipe
      // the local copy every other output falls back to.
      if (!out.ready()) return;

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
    collaborations: signal(/** @type {Collaboration[]} */ ([])),
    facets: signal(/** @type {Facet[]} */ ([])),
    playlistItems: signal(/** @type {PlaylistItem[]} */ ([])),
    settings: signal(/** @type {Setting[]} */ ([])),
    tracks: signal(/** @type {Track[]} */ ([])),
  };

  #selected = signal(
    /** @type {Output | null | undefined} */ (undefined),
  );

  #setupFinished = signal(false);

  /**
   * True while a selected output is being wired in and the local copy is being
   * carried into it. The mirror effects must stay out of the way until then: a
   * freshly selected output reports empty collections, and mirroring those
   * would erase the local copy `#carryOver` reads from.
   */
  #switching = signal(false);

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

    // A restored selection skips `#selectOutput`, so carry the local copy here
    // as well — a stored Firkin selection otherwise never receives it. Snapshot
    // the mirror before wiring the selection in: the mirror effects would
    // otherwise be free to write the selected output's still-empty collections
    // over the local copy this reads from.
    const carry = selectedOutput && selectedOutput !== defaultOutput
      ? {
        output: selectedOutput,
        mirror: await this.#mirrorSnapshot(defaultOutput),
      }
      : null;

    batch(() => {
      this.#switching.value = carry !== null;
      this.#selected.value = selectedOutput;
      this.#defaultOutput.value = defaultOutput;
      this.#setupFinished.value = true;
    });

    if (carry) {
      void this.#carryOver(carry.output, carry.mirror).finally(() => {
        this.#switching.value = false;
      });
    }
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

    // The local output mirrors whichever output is selected, so switching to a
    // fresh one would replace the mirror with nothing. Snapshot it first and
    // carry it into the new output when that output turns out to be empty —
    // otherwise selecting a new storage method looks like losing your data.
    const mirror = await this.#mirrorSnapshot(this.#defaultOutput.value);

    const selected = await this.#findOutput(id);

    batch(() => {
      this.#switching.value = true;
      this.#selected.value = selected;
    });

    try {
      if (selected) await this.#carryOver(selected, mirror);
    } finally {
      this.#switching.value = false;
    }
  };

  /**
   * What the local mirror currently holds, collection by collection.
   *
   * @param {Output | null | undefined} [mirrorOutput]
   * @returns {Promise<Array<{ name: CollectionName; data: any[] | undefined }>>}
   */
  async #mirrorSnapshot(mirrorOutput = this.#defaultOutput.value) {
    if (!mirrorOutput) return [];

    return await Promise.all(COLLECTIONS.map(async (name) => {
      // Give a lazy local read a moment to answer, so a page load doesn't
      // snapshot an unanswered collection as empty.
      const collection = await this.#settledCollection(mirrorOutput, name, 20);

      return {
        name,
        data: collection?.state === "loaded" && Array.isArray(collection.data)
          ? collection.data
          : undefined,
      };
    }));
  }

  /**
   * Copy the local mirror into a freshly selected output, collection by
   * collection, but only where that output has nothing. Anything already there
   * (e.g. synced from another device) wins.
   *
   * @param {Output} output
   * @param {Array<{ name: CollectionName; data: any[] | undefined }>} mirror
   */
  async #carryOver(output, mirror) {
    for (const { name, data: stored } of mirror) {
      // Facets are the one collection the interface synthesises: an empty local
      // store still shows the starting set. Carry that, or switching storage
      // empties the facet list the user is looking at.
      const data = stored && stored.length > 0
        ? stored
        : name === "facets"
        ? buildFacets(STARTING_SET_URIS)
        : stored;

      const label = `carry-over '${name}'`;

      if (!data || data.length === 0) {
        console.info(`${label}: nothing to carry`);
        continue;
      }

      const target = await this.#settledCollection(output, name);
      if (!target) {
        console.info(`${label}: target never answered`);
        continue;
      }
      if (Array.isArray(target.data) && target.data.length > 0) {
        console.info(`${label}: target already has ${target.data.length}`);
        continue;
      }

      try {
        await /** @type {any} */ (output)[name].save(data);
        console.info(`${label}: carried ${data.length}`);
      } catch (err) {
        console.warn(`${label}: failed`, err);
      }
    }
  }

  /**
   * Wait (briefly) for one collection of an output to report its state.
   *
   * @param {Output} output
   * @param {CollectionName} name
   */
  async #settledCollection(output, name, attempts = 60) {
    for (let attempt = 0; attempt < attempts; attempt++) {
      const collection = /** @type {any} */ (output)[name].collection();
      if (collection.state !== "loading") return collection;

      await new Promise((resolve) => setTimeout(resolve, 50));
    }

    return undefined;
  }

  /**
   * The output the interface should read from right now.
   *
   * While a switch is in progress the newly selected output is still empty —
   * reporting that reads as "the library and the preludes are gone", and the
   * loader reloads the page for a changed prelude set, which would abort the
   * carry-over. So the local copy stays authoritative until the carry-over has
   * run; afterwards it is either the same data or the output's own.
   *
   * @returns {Output | null | undefined}
   */
  #activeOutput() {
    return this.#switching.value ? null : this.#selected.value;
  }

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
