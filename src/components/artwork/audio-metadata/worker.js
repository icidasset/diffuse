import { musicMetadataTags } from "~/components/processor/metadata/common.js";
import { ostiary, rpc, workerProxy } from "~/common/worker.js";

/**
 * @import {Extraction} from "~/components/processor/metadata/types.d.ts"
 * @import {ActionsWithTunnel, ProxiedActions} from "~/common/worker.d.ts"
 * @import {InputActions} from "~/components/input/types.d.ts"
 * @import {Actions} from "~/components/artwork/types.d.ts"
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

  const resGet = await input.resolve({ method: "GET", uri: track.uri });
  if (!resGet) return null;

  const resHead = "stream" in resGet
    ? undefined
    : await input.resolve({ method: "HEAD", uri: track.uri });

  const meta = await musicMetadataTags({
    includeArtwork: true,
    stream: "stream" in resGet ? resGet.stream : undefined,
    mimeType: "stream" in resGet ? resGet.mimeType : undefined,
    urls: "url" in resGet
      ? {
        get: resGet.url,
        head: resHead && "url" in resHead ? resHead.url : resGet.url,
      }
      : undefined,
  }).catch(/** @param {Error} err */ (err) => {
    console.error("music-metadata error", err);
    return /** @type {Extraction} */ ({});
  });

  const pictures = meta.artwork ?? [];
  if (!pictures.length) return null;

  return pictures[0].data;
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { get });
});
