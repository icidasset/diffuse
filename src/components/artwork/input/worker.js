import { ostiary, rpc, workerProxy } from "~/common/worker.js";

/**
 * @import {ActionsWithTunnel, ProxiedActions} from "~/common/worker.d.ts"
 * @import {InputActions} from "@specs/components/input/types.d.ts"
 * @import {Actions} from "@specs/components/artwork/types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {ActionsWithTunnel<Actions>['get']}
 */
export async function get({ data: track, ports }) {
  /** @type {ProxiedActions<InputActions>} */
  const input = workerProxy(() => {
    ports.input.start();
    return ports.input;
  });

  return await input.artwork(track.uri);
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { get });
});
