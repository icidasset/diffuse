import { BroadcastableDiffuseElement } from "~/common/element.js";
import { batch, computed, signal, untracked } from "~/common/signal.js";
import { strictEquality } from "~/common/compare.js";

/**
 * @import {Collaboration, Facet, PlaylistItem, Setting, Track} from "~/definitions/types.d.ts"
 * @import {SignalWriter} from "~/common/signal.d.ts";
 * @import {OutputManager, OutputManagerProperties} from "@specs/components/output/types.d.ts"
 */

export class BroadcastedOutputElement extends BroadcastableDiffuseElement {
  /**
   * @param {OutputManager<any>} manager
   */
  replicateSavedData(manager) {
    // Broadcast if needed
    if (!this.hasAttribute("group")) return;

    /**
     * @template T
     * @param {{ save: (data: T) => Promise<void>; set: SignalWriter<T> }} _
     * @returns {(data: T) => Promise<void>}
     */
    const fn = ({ save, set }) => async (data) => {
      await untracked(async () => {
        if (await this.isLeader()) {
          return save(data);
        } else {
          return set(data);
        }
      });
    };

    const ogFacetsSave = manager.facets.save.bind(this);
    const ogCollaborationsSave = manager.collaborations.save.bind(this);
    const ogPlaylistItemsSave = manager.playlistItems.save.bind(this);
    const ogSettingsSave = manager.settings.save.bind(this);
    const ogTracksSave = manager.tracks.save.bind(this);

    const actions = this.broadcast(this.identifier, {
      saveFacets: {
        strategy: "replicate",
        fn: fn({ save: ogFacetsSave, set: manager.signals.facets.set }),
      },
      saveCollaborations: {
        strategy: "replicate",
        fn: fn({
          save: ogCollaborationsSave,
          set: manager.signals.collaborations.set,
        }),
      },
      savePlaylistItems: {
        strategy: "replicate",
        fn: fn({
          save: ogPlaylistItemsSave,
          set: manager.signals.playlistItems.set,
        }),
      },
      saveSettings: {
        strategy: "replicate",
        fn: fn({ save: ogSettingsSave, set: manager.signals.settings.set }),
      },
      saveTracks: {
        strategy: "replicate",
        fn: fn({ save: ogTracksSave, set: manager.signals.tracks.set }),
      },
    });

    if (actions) {
      manager.facets.save = actions.saveFacets;
      manager.collaborations.save = actions.saveCollaborations;
      manager.playlistItems.save = actions.savePlaylistItems;
      manager.settings.save = actions.saveSettings;
      manager.tracks.save = actions.saveTracks;
    }
  }
}

/**
 * @template [Encoding=null]
 * @param {OutputManagerProperties<Encoding>} _
 * @returns {OutputManager<Encoding>}
 */
export function outputManager(
  { init, collaborations, facets, playlistItems, settings, tracks },
) {
  // Older/custom outputs may not implement collaborations yet. Expose an empty
  // collection instead of failing, so adding the feature stays backwards
  // compatible.
  const collaborationStore = collaborations ?? {
    /** @returns {any} */
    empty: () => [],
    /** @returns {Promise<any>} */
    get: async () => [],
    /** @returns {Promise<void>} */
    put: async () => {},
  };

  const cl = signal(
    /** @type {Encoding extends null ? Collaboration[] : Encoding} */ (
      collaborationStore.empty()
    ),
  );
  const cls = signal(
    /** @type {"loading" | "loaded" | "sleeping" | "error"} */ ("sleeping"),
    { compare: strictEquality },
  );

  const c = signal(
    /** @type {Encoding extends null ? Facet[] : Encoding} */ (facets
      .empty()),
  );
  const cs = signal(
    /** @type {"loading" | "loaded" | "sleeping" | "error"} */ ("sleeping"),
    { compare: strictEquality },
  );

  const pl = signal(
    /** @type {Encoding extends null ? PlaylistItem[] : Encoding} */ (playlistItems
      .empty()),
  );
  const pls = signal(
    /** @type {"loading" | "loaded" | "sleeping" | "error"} */ ("sleeping"),
    { compare: strictEquality },
  );

  const s = signal(
    /** @type {Encoding extends null ? Setting[] : Encoding} */ (settings
      .empty()),
  );
  const ss = signal(
    /** @type {"loading" | "loaded" | "sleeping" | "error"} */ ("sleeping"),
    { compare: strictEquality },
  );

  const t = signal(
    /** @type {Encoding extends null ? Track[] : Encoding} */ (tracks.empty()),
  );
  const ts = signal(
    /** @type {"loading" | "loaded" | "sleeping" | "error"} */ ("sleeping"),
    { compare: strictEquality },
  );

  // Revision counters so a racing load never overwrites a value that was just
  // saved (or a newer load was started) while it was awaiting `get()`.
  let cRev = 0;
  let clRev = 0;
  let plRev = 0;
  let sRev = 0;
  let tRev = 0;

  async function loadCollaborations() {
    if (init && (await init()) === false) return;
    const rev = clRev;
    cls.value = "loading";
    try {
      const data = await collaborationStore.get();
      if (clRev === rev) cl.value = data;
      cls.value = "loaded";
    } catch (err) {
      console.error("Failed to load collaborations:", err);
      cls.value = "error";
    }
  }

  async function loadFacets() {
    if (init && (await init()) === false) return;
    const rev = cRev;
    cs.value = "loading";
    try {
      const data = await facets.get();
      if (cRev === rev) c.value = data;
      cs.value = "loaded";
    } catch (err) {
      console.error("Failed to load facets:", err);
      cs.value = "error";
    }
  }

  async function loadPlaylistItems() {
    if (init && (await init()) === false) return;
    const rev = plRev;
    pls.value = "loading";
    try {
      const data = await playlistItems.get();
      if (plRev === rev) pl.value = data;
      pls.value = "loaded";
    } catch (err) {
      console.error("Failed to load playlist items:", err);
      pls.value = "error";
    }
  }

  async function loadSettings() {
    if (init && (await init()) === false) return;
    const rev = sRev;
    ss.value = "loading";
    try {
      const data = await settings.get();
      if (sRev === rev) s.value = data;
      ss.value = "loaded";
    } catch (err) {
      console.error("Failed to load settings:", err);
      ss.value = "error";
    }
  }

  async function loadTracks() {
    if (init && (await init()) === false) return;
    const rev = tRev;
    ts.value = "loading";
    try {
      const data = await tracks.get();
      if (tRev === rev) t.value = data;
      ts.value = "loaded";
    } catch (err) {
      console.error("Failed to load tracks:", err);
      ts.value = "error";
    }
  }

  return {
    collaborations: {
      collection: computed(() => {
        if (untracked(() => cls.value === "sleeping")) loadCollaborations();
        return cls.value === "loaded"
          ? { state: "loaded", data: cl.value }
          : cls.value === "error"
          ? { state: "error" }
          : { state: "loading" };
      }),
      reload: loadCollaborations,
      save: async (newCollaborations) => {
        batch(() => {
          clRev++;
          if (untracked(() => cls.value === "sleeping")) cls.value = "loaded";
          cl.value = newCollaborations;
        });
        await collaborationStore.put(newCollaborations);
      },
    },
    facets: {
      collection: computed(() => {
        if (untracked(() => cs.value === "sleeping")) loadFacets();
        return cs.value === "loaded"
          ? { state: "loaded", data: c.value }
          : cs.value === "error"
          ? { state: "error" }
          : { state: "loading" };
      }),
      reload: loadFacets,
      save: async (newFacets) => {
        batch(() => {
          cRev++;
          if (untracked(() => cs.value === "sleeping")) cs.value = "loaded";
          c.value = newFacets;
        });
        await facets.put(newFacets);
      },
    },
    playlistItems: {
      collection: computed(() => {
        if (untracked(() => pls.value === "sleeping")) loadPlaylistItems();
        return pls.value === "loaded"
          ? { state: "loaded", data: pl.value }
          : pls.value === "error"
          ? { state: "error" }
          : { state: "loading" };
      }),
      reload: loadPlaylistItems,
      save: async (newPlaylistItems) => {
        batch(() => {
          plRev++;
          if (untracked(() => pls.value === "sleeping")) pls.value = "loaded";
          pl.value = newPlaylistItems;
        });
        await playlistItems.put(newPlaylistItems);
      },
    },
    settings: {
      collection: computed(() => {
        if (untracked(() => ss.value === "sleeping")) loadSettings();
        return ss.value === "loaded"
          ? { state: "loaded", data: s.value }
          : ss.value === "error"
          ? { state: "error" }
          : { state: "loading" };
      }),
      reload: loadSettings,
      save: async (newSettings) => {
        batch(() => {
          sRev++;
          if (untracked(() => ss.value === "sleeping")) ss.value = "loaded";
          s.value = newSettings;
        });
        await settings.put(newSettings);
      },
    },
    tracks: {
      collection: computed(() => {
        if (untracked(() => ts.value === "sleeping")) loadTracks();
        return ts.value === "loaded"
          ? { state: "loaded", data: t.value }
          : ts.value === "error"
          ? { state: "error" }
          : { state: "loading" };
      }),
      reload: loadTracks,
      save: async (newTracks) => {
        batch(() => {
          tRev++;
          if (untracked(() => ts.value === "sleeping")) ts.value = "loaded";
          t.value = newTracks;
        });
        await tracks.put(newTracks);
      },
    },
    signals: {
      collaborations: cl,
      facets: c,
      playlistItems: pl,
      settings: s,
      tracks: t,
    },
  };
}
