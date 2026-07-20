import * as URI from "fast-uri";
import { musicMetadataTags } from "~/components/metadata/common.js";
import { ostiary, rpc, workerProxy } from "~/common/worker.js";

/**
 * @import {Extraction} from "@specs/components/metadata/audio-file/types.d.ts"
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

  const resGet = await input.resolve({ method: "GET", uri: track.uri });
  if (!resGet) return null;

  const resHead = "stream" in resGet
    ? undefined
    : await input.resolve({ method: "HEAD", uri: track.uri });

  const trackUri = URI.parse(track.uri);
  const trackPathParts = trackUri.path?.split("/");
  const filename = trackPathParts?.[trackPathParts.length - 1];

  const meta = await musicMetadataTags({
    filename,
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
