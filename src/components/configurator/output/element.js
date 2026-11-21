import {
  BroadcastableDiffuseElement,
  defineElement,
} from "~/common/element.js";
import { batch, computed, effect, signal } from "~/common/signal.js";

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

/**
 * How long a newly selected output gets to finish its initial connection
 * check before it is treated as disconnected. Outputs that were already
 * usable fall back immediately.
 */
const DISCONNECT_GRACE_MS = 60_000;

/**
 * How long to wait after the output options settle before treating a stored
 * selection that still doesn't resolve as stale.
 *
 * Options are appended asynchronously by the output preludes during bootstrap,
 * so the check can't run immediately. A selection whose element exists is
 * never dropped: resolving it awaits the element's upgrade, so a slow module
 * load can't be mistaken for a removed option.
 */
const STALE_SELECTION_SETTLE_MS = 1_000;

/**
 * Upper bound for `waitForSelected()`. The stale-selection check normally
 * settles it first; the bound only matters when that check can't complete.
 */
const SELECTION_WAIT_MS = 5_000;

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
        if (!this.#shouldMirror(out, col)) return;
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
        if (!this.#shouldMirror(out, col)) return;
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
        if (!this.#shouldMirror(out, col)) return;
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
        if (!this.#shouldMirror(out, col)) return;
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
        if (!this.#shouldMirror(out, col)) return;
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

      this.#scheduleCarryOver(out, def);
    });

    /**
     * Fall back to the default output when the selected one can't read or
     * write: a remote output reports `ready: false` when its credentials or
     * session are gone, or when the browser is offline. Without this the UI
     * keeps presenting a disconnected store as the one in use.
     *
     * A previously usable output is replaced immediately. On boot the output
     * gets a grace period, because its connection check is asynchronous and
     * `ready` starts out false.
     */
    this.effect(() => {
      const out = this.#selected.value;
      const online = this.#isOnline.value;

      if (out !== this.#readinessOutput) {
        this.#readinessOutput = out;
        this.#readiness = undefined;
      }

      const ready = out ? (out.ready?.() ?? true) : true;
      const becameUnready = this.#readiness === true && !ready;
      this.#readiness = ready;

      this.#clearDisconnectTimer();

      if (!out || out === this.#defaultOutput.value) return;
      if (ready || !online) return;

      if (becameUnready) {
        void this.#fallBackToDefault(out);
        return;
      }

      this.#disconnectTimer = setTimeout(() => {
        this.#disconnectTimer = undefined;

        const current = this.#selected.value;
        if (!current || current === this.#defaultOutput.value) return;
        if (current.ready?.() ?? true) return;
        if (!this.#isOnline.value) return;

        void this.#fallBackToDefault(current);
      }, DISCONNECT_GRACE_MS);
    });
  }

  /**
   * Stop using a selected output that isn't usable, falling back to the
   * default (local) output. Clearing the selection matches a manual
   * disconnect, so the choice isn't silently re-applied while the output is
   * still unavailable.
   *
   * @param {Output} out
   */
  async #fallBackToDefault(out) {
    if (this.#selected.value !== out) return;
    if (out.ready?.() ?? true) return;

    console.warn(
      `Output "${out.id}" is not ready; falling back to the default output.`,
    );

    await this.deselect();
  }

  #clearDisconnectTimer() {
    if (this.#disconnectTimer === undefined) return;

    clearTimeout(this.#disconnectTimer);
    this.#disconnectTimer = undefined;
  }

  /**
   * Watch the configurator's children, which is where output options are
   * appended. A stored selection that points at an option missing from this
   * build can then be re-resolved if the option shows up, and dropped once
   * the options have settled.
   */
  #watchOptions() {
    this.#optionsObserver = new MutationObserver(() =>
      this.#scheduleStaleCheck()
    );

    this.#optionsObserver.observe(this, { childList: true });
    this.#scheduleStaleCheck();
  }

  #stopWatchingOptions() {
    this.#optionsObserver?.disconnect();
    this.#optionsObserver = undefined;

    this.#clearStaleSelectionTimer();
  }

  #clearStaleSelectionTimer() {
    if (this.#staleSelectionTimer === undefined) return;

    clearTimeout(this.#staleSelectionTimer);
    this.#staleSelectionTimer = undefined;
  }

  /**
   * (Re)start the timer that drops a stored selection which can't be resolved
   * to an output element. Restarting on every option change treats a burst of
   * appended options as one settle instead of checking between them.
   */
  #scheduleStaleCheck() {
    this.#clearStaleSelectionTimer();

    if (!this.isConnected) return;
    if (!this.#selectedOutputId() || this.#selected.value) return;

    this.#staleSelectionTimer = setTimeout(() => {
      this.#staleSelectionTimer = undefined;
      void this.#dropStaleSelection();
    }, STALE_SELECTION_SETTLE_MS);
  }

  /**
   * Drop a stored selection whose option no longer exists, leaving the app on
   * the default output instead of a selection that can never resolve.
   *
   * Resolving here rather than checking `localStorage` / `querySelector` also
   * covers an option element that was appended but whose module is still
   * loading, because `#findOutput()` waits for the element to upgrade.
   */
  async #dropStaleSelection() {
    const id = this.#selectedOutputId();
    if (!id || this.#selected.value) return;
    if (await this.#findOutput(id)) return;

    // The selection may have changed while the lookup was pending.
    if (this.#selectedOutputId() !== id || this.#selected.value) return;

    console.warn(
      `Stored output selection "${id}" no longer exists; falling back to the default output.`,
    );

    await this.deselect();
  }

  /**
   * Mirroring a selected output into the default store is only safe once the
   * output has been used, or for a collection that actually has data. An empty
   * collection from a fresh output must not overwrite the local copy it is
   * about to be seeded from — even when another collection already has data.
   *
   * @param {Output} out
   * @param {{ data: unknown }} collection
   * @returns {boolean}
   */
  #shouldMirror(out, collection) {
    return isOutputInitialized(out.id) || asArray(collection.data).length > 0;
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
  /**
   * Start the initial carry-over once the selected output's collections have
   * answers. The collections load asynchronously (local cache, pull, or both),
   * so waiting here is what keeps a save that races the selection from skipping
   * the collections that are still empty.
   *
   * @param {Output} out
   * @param {Output} def
   */
  #scheduleCarryOver(out, def) {
    const id = out.id;
    if (isOutputInitialized(id) || this.#carryingOver.has(id)) return;

    this.#carryingOver.add(id);

    void (async () => {
      try {
        const deadline = Date.now() + 10_000;

        while (Date.now() < deadline) {
          if (this.#selected.value !== out) return;

          const collections = COLLECTIONS.map((name) =>
            out[name].collection()
          );

          if (collections.every((collection) =>
            collection?.state === "loaded"
          )) {
            await this.#carryOver(out, def, id);
            return;
          }

          await new Promise((resolve) => setTimeout(resolve, 50));
        }
      } finally {
        this.#carryingOver.delete(id);
      }
    })();
  }

  /**
   * Read the default output and fill the collections of an uninitialized
   * selected output. Collections that already have data are left alone: a save
   * can race the selection, and the carry-over must not overwrite it.
   *
   * Runs once per output; the initialized flag prevents a later carry-over after
   * the user intentionally emptied a collection.
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

      /** @type {Promise<void>[]} */
      const saves = [];

      for (const name of COLLECTIONS) {
        const current = out[name].collection();
        if (current?.state !== "loaded") continue;
        if (asArray(current.data).length > 0) continue;

        // A fresh install still needs the starting-set facets, even though the
        // local store has no facets of its own.
        const items = name === "facets" && data.facets.length === 0
          ? buildFacets(STARTING_SET_URIS)
          : data[name];

        saves.push(
          /** @type {Promise<void>} */ (out[name].save(items)),
        );
      }

      await Promise.all(saves);
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
   * Bumped when the stored selection changes, so `waitForSelected()` re-checks
   * even when the selected element stays `null` (a stale selection being
   * dropped doesn't change the `#selected` signal's value).
   */
  #selectionChanged = signal(0);

  /**
   * Output ids whose contents are being copied from the default output, so the
   * effect above doesn't start a second copy while the first is in flight.
   *
   * @type {Set<string>}
   */
  #carryingOver = new Set();

  #setupFinished = signal(false);

  #isOnline = signal(navigator.onLine);

  /** @type {ReturnType<typeof setTimeout> | undefined} */
  #disconnectTimer;

  /** @type {ReturnType<typeof setTimeout> | undefined} */
  #staleSelectionTimer;

  /** @type {MutationObserver | undefined} */
  #optionsObserver;

  /** @type {Output | null | undefined} */
  #readinessOutput;

  /** @type {boolean | undefined} */
  #readiness;

  #handleOnline = () => this.#isOnline.value = true;
  #handleOffline = () => this.#isOnline.value = false;;

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

    globalThis.addEventListener("online", this.#handleOnline);
    globalThis.addEventListener("offline", this.#handleOffline);

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

    // The element may have been disconnected while its initial lookups were
    // pending; only watch for option changes while it is still connected.
    if (this.isConnected) this.#watchOptions();
  }

  /** @override */
  disconnectedCallback() {
    globalThis.removeEventListener("online", this.#handleOnline);
    globalThis.removeEventListener("offline", this.#handleOffline);

    this.#clearDisconnectTimer();
    this.#stopWatchingOptions();
    super.disconnectedCallback();
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

    const out = this.#selected.value;
    const def = this.#defaultOutput.value;
    if (out && def && out !== def) this.#scheduleCarryOver(out, def);
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
    this.#selectionChanged.value++;
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

    this.#scheduleStaleCheck();
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
    this.#selectionChanged.value++;
    await this.#selectOutput(id);

    this.#scheduleStaleCheck();
  };

  /**
   * Wait for the selected output element to resolve.
   *
   * Resolves with the selected element, or with `null` once the stored
   * selection has been dropped as stale or when `timeoutMs` elapses. Callers
   * should prefer this over waiting on `selected()` directly, which never
   * resolves when the stored selection points at an option that no longer
   * exists in this build.
   *
   * @param {number} [timeoutMs]
   * @returns {Promise<Output | null>}
   */
  waitForSelected = (timeoutMs = SELECTION_WAIT_MS) => {
    /** @returns {boolean} */
    const settled = () => this.selected() !== null || !this.hasSelected();

    if (settled()) return Promise.resolve(this.selected());

    return new Promise((resolve) => {
      let done = false;
      /** @type {ReturnType<typeof setTimeout> | undefined} */
      let timer = undefined;
      /** @type {() => void} */
      let stop = () => {};

      const finish = () => {
        if (done) return;
        done = true;

        if (timer !== undefined) clearTimeout(timer);
        stop();
        resolve(this.selected());
      };

      stop = effect(() => {
        this.#selectionChanged.get();

        // Re-check at the end of the effect so the dependency reads above are
        // registered before it can settle.
        if (this.selected() !== null || !this.hasSelected()) finish();
      });

      timer = setTimeout(finish, timeoutMs);
    });
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
