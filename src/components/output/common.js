import { computed, signal, untracked } from "@common/signal.js";

/**
 * @import {Track} from "@definitions/types.d.ts"
 * @import {OutputManager, OutputManagerProperties} from "./types.d.ts"
 */

/**
 * @template Encoding
 * @param {OutputManagerProperties<Encoding>} _
 * @returns {OutputManager<Encoding>}
 */
export function outputManager({ init, tracks }) {
  const t = signal(
    /** @type {Encoding extends null ? Track[] : Encoding} */ (tracks.empty()),
  );
  const ts = signal(
    /** @type {"loading" | "loaded" | "sleeping"} */ ("sleeping"),
  );

  async function loadTracks() {
    if (init && (await init()) === false) return;
    ts.value = "loading";
    t.value = await tracks.get();
    ts.value = "loaded";
  }

  return {
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
      tracks: t,
    },
  };
}
