import { announce, define } from "@common/worker.js";
import { effect, signal } from "@common/signal.js";

/**
 * @import {Track} from "@component/core/types.d.ts"
 */

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

const pools = signal(/** @type {Record<string, { pool: Track[] }>} */ ({}));

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

define("pool", pool);

/**
 * @param {{ groupId: string; tracks: Track[] }} _
 */
function pool({ groupId, tracks }) {
  fill();

  // TODO:
  // Create a signal for each group: future, past, now
  // Whenever that state changes it should create an annoucement.
  // Custom elements on the main thread can then listen for those.
}

// PRIVATE

function fill() {
}

function todo() {
  effect(() => {
    const data = groupSignal();
    announce("some-name", data);
  });
}
