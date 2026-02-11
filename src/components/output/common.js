import { computed, signal, untracked } from "@common/signal.js";

/**
 * @import {Facet, Track} from "@definitions/types.d.ts"
 * @import {OutputManager, OutputManagerProperties} from "./types.d.ts"
 */

/**
 * @template Encoding
 * @param {OutputManagerProperties<Encoding>} _
 * @returns {OutputManager<Encoding>}
 */
export function outputManager({ init, facets, tracks }) {
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

  async function loadFacets() {
    if (init && (await init()) === false) return;
    cs.value = "loading";
    c.value = await facets.get();
    cs.value = "loaded";
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
      tracks: t,
    },
  };
}
