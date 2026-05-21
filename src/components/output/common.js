import { BroadcastableDiffuseElement } from "~/common/element.js";
import { batch, computed, signal, untracked } from "~/common/signal.js";
import { strictEquality } from "~/common/compare.js";

/**
 * @import {Facet, PlaylistItem, Setting, Track} from "~/definitions/types.d.ts"
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
    const ogPlaylistItemsSave = manager.playlistItems.save.bind(this);
    const ogSettingsSave = manager.settings.save.bind(this);
    const ogTracksSave = manager.tracks.save.bind(this);

    const actions = this.broadcast(this.identifier, {
      saveFacets: {
        strategy: "replicate",
        fn: fn({ save: ogFacetsSave, set: manager.signals.facets.set }),
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
  { init, facets, playlistItems, settings, tracks },
) {
  const c = signal(
    /** @type {Encoding extends null ? Facet[] : Encoding} */ (facets
      .empty()),
  );
  const cs = signal(
    /** @type {"loading" | "loaded" | "sleeping"} */ ("sleeping"),
    { compare: strictEquality },
  );

  const pl = signal(
    /** @type {Encoding extends null ? PlaylistItem[] : Encoding} */ (playlistItems
      .empty()),
  );
  const pls = signal(
    /** @type {"loading" | "loaded" | "sleeping"} */ ("sleeping"),
    { compare: strictEquality },
  );

  const s = signal(
    /** @type {Encoding extends null ? Setting[] : Encoding} */ (settings
      .empty()),
  );
  const ss = signal(
    /** @type {"loading" | "loaded" | "sleeping"} */ ("sleeping"),
    { compare: strictEquality },
  );

  const t = signal(
    /** @type {Encoding extends null ? Track[] : Encoding} */ (tracks.empty()),
  );
  const ts = signal(
    /** @type {"loading" | "loaded" | "sleeping"} */ ("sleeping"),
    { compare: strictEquality },
  );

  async function loadFacets() {
    if (init && (await init()) === false) return;
    cs.value = "loading";
    c.value = await facets.get();
    cs.value = "loaded";
  }

  async function loadPlaylistItems() {
    if (init && (await init()) === false) return;
    pls.value = "loading";
    pl.value = await playlistItems.get();
    pls.value = "loaded";
  }

  async function loadSettings() {
    if (init && (await init()) === false) return;
    ss.value = "loading";
    s.value = await settings.get();
    ss.value = "loaded";
  }

  async function loadTracks() {
    if (init && (await init()) === false) return;
    ts.value = "loading";
    t.value = await tracks.get();
    ts.value = "loaded";
  }

  return {
    facets: {
      collection: computed(() => {
        if (untracked(() => cs.value === "sleeping")) loadFacets();
        return cs.value === "loaded"
          ? { state: "loaded", data: c.value }
          : { state: "loading" };
      }),
      reload: loadFacets,
      save: async (newFacets) => {
        batch(() => {
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
          : { state: "loading" };
      }),
      reload: loadPlaylistItems,
      save: async (newPlaylistItems) => {
        batch(() => {
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
          : { state: "loading" };
      }),
      reload: loadSettings,
      save: async (newSettings) => {
        batch(() => {
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
          : { state: "loading" };
      }),
      reload: loadTracks,
      save: async (newTracks) => {
        batch(() => {
          if (untracked(() => ts.value === "sleeping")) ts.value = "loaded";
          t.value = newTracks;
        });
        await tracks.put(newTracks);
      },
    },
    signals: {
      facets: c,
      playlistItems: pl,
      settings: s,
      tracks: t,
    },
  };
}
