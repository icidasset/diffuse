import { signal } from "@common/signal.js";

/**
 * @import {OutputManager, OutputManagerProperties} from "./types.d.ts"
 */

/**
 * @template Tracks
 * @param {OutputManagerProperties<Tracks>} _
 * @returns {OutputManager<Tracks>}
 */
export function outputManager({ init, tracks }) {
  const t = signal(/** @type {Tracks} */ (tracks.empty()));
  const ts = signal(/** @type {"loading" | "loaded"} */ ("loading"));

  async function loadTracks() {
    if (init && (await init()) === false) return;
    t.value = await tracks.get();
    ts.value = "loaded";
  }

  loadTracks();

  return {
    tracks: {
      collection: t.get,
      reload: loadTracks,
      save: async (newTracks) => {
        t.value = newTracks;
        await tracks.put(newTracks);
      },
      state: ts.get,
    },
  };
}
