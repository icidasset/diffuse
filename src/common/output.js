import { effect } from "~/common/signal.js";

/**
 * @import {SignalReader} from "~/common/signal.d.ts";
 */

/**
 * @template T
 * @param {{ collection: SignalReader<{ state: "loading" } | { state: "loaded"; data: T }> }} output
 * @returns {Promise<T>}
 */
export async function data(output) {
  return await new Promise((resolve) => {
    let resolved = false;

    const stop = effect(() => {
      if (resolved) {
        stop();
        return;
      }

      const col = output.collection();

      if (col.state === "loaded") {
        resolved = true;
        resolve(col.data);
      }
    });
  });
}
