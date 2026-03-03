import { effect } from "~/common/signal.js";

/**
 * @import {SignalReader} from "~/common/signal.d.ts";
 */

/**
 * @param {{ collection: SignalReader<any>; state: SignalReader<"loading" | "loaded" | "sleeping"> }} output
 */
export async function waitUntilLoaded(output) {
  return await new Promise((resolve) => {
    let resolved = false;

    const stop = effect(() => {
      if (resolved) {
        stop();
        return;
      }

      if (output.state() === "loaded") {
        resolved = true;
        resolve(void 0);
      } else if (output.state() === "sleeping") {
        output.collection();
      }
    });
  });
}
