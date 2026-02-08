import { ostiary, rpc, workerProxy } from "@common/worker.js";

/**
 * @import {Track} from "@definitions/types.d.ts"
 * @import {ActionsWithTunnel, ProxiedActions} from "@common/worker.d.ts"
 * @import {InputActions} from "@components/input/types.d.ts"
 * @import {Actions as SearchProcessorActions} from "@components/processor/search/types.d.ts"
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {ActionsWithTunnel<Actions>["supplyAvailable"]}
 */
export async function supplyAvailable({ data, ports }) {
  const cachedTracks = data.filter((t) => t.kind !== "placeholder");

  /** @type {ProxiedActions<InputActions>} */
  const input = workerProxy(() => ports.input);

  /** @type {ProxiedActions<SearchProcessorActions>} */
  const search = workerProxy(() => ports.search);

  ports.input.start();
  ports.search.start();

  // Consult input
  const groups = await input.groupConsult(cachedTracks);

  /** @type {Track[]} */
  let availableTracks = [];

  Object.values(groups).forEach((value) => {
    if (value.available === false) return;
    availableTracks = availableTracks.concat(value.tracks);
  }, []);

  // Set pool
  await search.supply({ tracks: availableTracks });
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { supplyAvailable });
});
