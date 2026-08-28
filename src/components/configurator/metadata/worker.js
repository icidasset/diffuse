import { ostiary, rpc, workerProxy } from "~/common/worker.js";

/**
 * @import {ActionsWithTunnel, ProxiedActions} from "~/common/worker.d.ts"
 * @import {Actions} from "@specs/components/metadata/types.d.ts"
 * @import {Actions as ConfiguratorActions} from "@specs/components/configurator/metadata/types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {ActionsWithTunnel<ConfiguratorActions>['patch']}
 */
export async function patch({ data: track, ports }) {
  let result = track;

  for (const port of Object.values(ports)) {
    /** @type {ProxiedActions<Actions>} */
    const metadata = workerProxy(() => {
      port.start();
      return port;
    });

    result = await metadata.patch(result);
  }

  return result;
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { patch });
});
