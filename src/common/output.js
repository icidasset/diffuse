import { effect } from "~/common/signal.js";

/**
 * @import {SignalReader} from "~/common/signal.d.ts";
 */

/**
 * @template T
 * @param {{ collection: SignalReader<{ state: "loading" } | { state: "loaded"; data: T }> }} output
 * @returns {Promise<T>}
 *
 * @example Resolves immediately when collection is already loaded
 * ```js
 * import { data } from "~/common/output.js";
 * import { signal } from "~/common/signal.js";
 *
 * const col = signal({ state: "loaded", data: ["a", "b"] });
 * const result = await data({ collection: col.get });
 * if (result.join(",") !== "a,b") throw new Error("expected ['a', 'b']");
 * ```
 *
 * @example Waits for collection to transition to loaded
 * ```js
 * import { data } from "~/common/output.js";
 * import { signal } from "~/common/signal.js";
 *
 * const col = signal({ state: "loading" });
 * const promise = data({ collection: col.get });
 *
 * await Promise.resolve();
 * col.set({ state: "loaded", data: [1, 2, 3] });
 *
 * const result = await promise;
 * if (result.join(",") !== "1,2,3") throw new Error("expected [1, 2, 3]");
 * ```
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
