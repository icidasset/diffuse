import { ostiary, rpc, workerProxy } from "~/common/worker.js";

/**
 * @import {ActionsWithTunnel, ProxiedActions} from "~/common/worker.d.ts"
 * @import {Actions} from "~/components/artwork/types.d.ts"
 * @import {Actions as ConfiguratorActions} from "./types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {ActionsWithTunnel<ConfiguratorActions>['get']}
 */
export async function get({ data, ports }) {
  const track = data;

  for (const port of Object.values(ports)) {
    /** @type {ProxiedActions<Actions>} */
    const artwork = workerProxy(() => {
      port.start();
      return port;
    });

    const bytes = await artwork.get(track);
    if (bytes !== null) return bytes;
  }

  return null;
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { get });
});
