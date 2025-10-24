import { effect, signal } from "@common/signal.js";

/**
 * @import {OutputManager, Track} from "@component/core/types.d.ts"
 */

/**
 * @param {{ init?: () => Promise<boolean>; tracks: { get(): Promise<Track[]>; put(tracks: Track[]): Promise<void>; } }} _
 * @returns {OutputManager}
 */
export function outputManager({ init, tracks }) {
  const t = signal(/** @type {Track[]} */ ([]));
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
