import { signal } from "@common/signal.js";

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
    console.log("...");
    if (init && (await init()) === false) return;
    console.log("start load");
    const a = await tracks.get();
    console.log(a);
    t.value = a;
    ts.value = "loaded";
  }

  loadTracks();

  return {
    tracks: {
      collection: t.get,
      reload: loadTracks,
      state: ts.get,
      store: async (newTracks) => {
        t.value = newTracks;
        await tracks.put(newTracks);
      },
    },
  };
}
