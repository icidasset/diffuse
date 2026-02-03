import { computed, signal, untracked } from "@common/signal.js";

/**
 * @import {Constituent, Track} from "@definitions/types.d.ts"
 * @import {OutputManager, OutputManagerProperties} from "./types.d.ts"
 */

/**
 * @template Encoding
 * @param {OutputManagerProperties<Encoding>} _
 * @returns {OutputManager<Encoding>}
 */
export function outputManager({ init, constituents, tracks }) {
  const c = signal(
    /** @type {Encoding extends null ? Constituent[] : Encoding} */ (constituents
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

  async function loadConstituents() {
    if (init && (await init()) === false) return;
    cs.value = "loading";
    c.value = await constituents.get();
    cs.value = "loaded";
  }

  async function loadTracks() {
    if (init && (await init()) === false) return;
    ts.value = "loading";
    t.value = await tracks.get();
    ts.value = "loaded";
  }

  return {
    constituents: {
      collection: computed(() => {
        if (untracked(() => cs.value === "sleeping")) loadConstituents();
        return c.value;
      }),
      reload: loadConstituents,
      save: async (newConstituents) => {
        if (untracked(() => cs.value === "sleeping")) loadConstituents();
        c.value = newConstituents;
        await constituents.put(newConstituents);
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
      constituents: c,
      tracks: t,
    },
  };
}
