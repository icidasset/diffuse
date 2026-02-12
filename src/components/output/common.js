import { computed, signal, untracked } from "@common/signal.js";

/**
 * @import {Facet, Playlist, Theme, Track} from "@definitions/types.d.ts"
 * @import {OutputManager, OutputManagerProperties} from "./types.d.ts"
 */

/**
 * @template Encoding
 * @param {OutputManagerProperties<Encoding>} _
 * @returns {OutputManager<Encoding>}
 */
export function outputManager({ init, facets, playlists, themes, tracks }) {
  const c = signal(
    /** @type {Encoding extends null ? Facet[] : Encoding} */ (facets
      .empty()),
  );
  const cs = signal(
    /** @type {"loading" | "loaded" | "sleeping"} */ ("sleeping"),
  );

  const t = signal(
    /** @type {Encoding extends null ? Track[] : Encoding} */ (tracks.empty()),
  );
  const ts = signal(
    /** @type {"loading" | "loaded" | "sleeping"} */ ("sleeping"),
  );

  const pl = signal(
    /** @type {Encoding extends null ? Playlist[] : Encoding} */ (playlists
      .empty()),
  );
  const pls = signal(
    /** @type {"loading" | "loaded" | "sleeping"} */ ("sleeping"),
  );

  const th = signal(
    /** @type {Encoding extends null ? Theme[] : Encoding} */ (themes.empty()),
  );
  const ths = signal(
    /** @type {"loading" | "loaded" | "sleeping"} */ ("sleeping"),
  );

  async function loadFacets() {
    if (init && (await init()) === false) return;
    cs.value = "loading";
    c.value = await facets.get();
    cs.value = "loaded";
  }

  async function loadPlaylists() {
    if (init && (await init()) === false) return;
    pls.value = "loading";
    pl.value = await playlists.get();
    pls.value = "loaded";
  }

  async function loadThemes() {
    if (init && (await init()) === false) return;
    ths.value = "loading";
    th.value = await themes.get();
    ths.value = "loaded";
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
        return c.value;
      }),
      reload: loadFacets,
      save: async (newFacets) => {
        if (untracked(() => cs.value === "sleeping")) loadFacets();
        c.value = newFacets;
        await facets.put(newFacets);
      },
      state: cs.get,
    },
    playlists: {
      collection: computed(() => {
        if (untracked(() => pls.value === "sleeping")) loadPlaylists();
        return pl.value;
      }),
      reload: loadPlaylists,
      save: async (newPlaylists) => {
        if (untracked(() => pls.value === "sleeping")) loadPlaylists();
        pl.value = newPlaylists;
        await playlists.put(newPlaylists);
      },
      state: pls.get,
    },
    themes: {
      collection: computed(() => {
        if (untracked(() => ths.value === "sleeping")) loadThemes();
        return th.value;
      }),
      reload: loadThemes,
      save: async (newThemes) => {
        if (untracked(() => ths.value === "sleeping")) loadThemes();
        th.value = newThemes;
        await themes.put(newThemes);
      },
      state: ths.get,
    },
    tracks: {
      collection: computed(() => {
        if (untracked(() => ts.value === "sleeping")) loadTracks();
        return t.value;
      }),
      reload: loadTracks,
      save: async (newTracks) => {
        if (untracked(() => ts.value === "sleeping")) loadTracks();
        t.value = newTracks;
        await tracks.put(newTracks);
      },
      state: ts.get,
    },
    signals: {
      facets: c,
      playlists: pl,
      themes: th,
      tracks: t,
    },
  };
}
