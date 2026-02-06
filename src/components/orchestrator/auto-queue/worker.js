import { ostiary, rpc, workerProxy } from "@common/worker.js";

/**
 * @import {Track} from "@definitions/types.d.ts"
 * @import {ActionsWithTunnel, ProxiedActions} from "@common/worker.d.ts"
 * @import {InputActions} from "@components/input/types.d.ts"
 * @import {Actions as QueueEngineActions} from "@components/engine/queue/types.d.ts"
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {ActionsWithTunnel<Actions>["poolAvailable"]}
 */
export async function poolAvailable({ data, ports }) {
  const cachedTracks = data.tracks.filter((t) => t.kind !== "placeholder");

  /** @type {ProxiedActions<InputActions>} */
  const input = workerProxy(() => ports.input);

  /** @type {ProxiedActions<QueueEngineActions>} */
  const queue = workerProxy(() => ports.queue);

  ports.input.start();
  ports.queue.start();

  // Consult input
  const groups = await input.groupConsult(cachedTracks);

  /** @type {Track[]} */
  let availableTracks = [];

  Object.values(groups).forEach((value) => {
    if (value.available === false) return;
    availableTracks = availableTracks.concat(value.tracks);
  }, []);

  // Set pool
  await queue.supply({ tracks: availableTracks });
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { poolAvailable });
});
