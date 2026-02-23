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
 * @type {ActionsWithTunnel<Actions>["supply"]}
 */
export async function supply({ data, ports }) {
  const cachedTracks = data.filter((t) => t.kind !== "placeholder");

  /** @type {ProxiedActions<InputActions>} */
  const input = workerProxy(() => ports.input);

  /** @type {ProxiedActions<SearchProcessorActions>} */
  const search = workerProxy(() => ports.search);

  ports.input.start();
  ports.search.start();

  // Consult input
  const groups = await input.groupConsult(
    cachedTracks.map((t) => t.uri),
  );

  /** @type {Set<string>} */
  const availableUris = new Set();

  Object.values(groups).forEach((value) => {
    if (value.available === false) return;
    for (const uri of value.uris) {
      availableUris.add(uri);
    }
  });

  const availableTracks = cachedTracks.filter((t) => availableUris.has(t.uri));

  // Set pool
  search.supply({ tracks: availableTracks });

  // Fin
  return { availableTracks };
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { supply });
});
