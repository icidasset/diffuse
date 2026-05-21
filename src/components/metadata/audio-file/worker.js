import { ostiary, rpc, workerProxy } from "~/common/worker.js";
import { musicMetadataTags } from "~/components/metadata/common.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {ActionsWithTunnel, ProxiedActions} from "~/common/worker.d.ts"
 * @import {InputActions} from "@specs/components/input/types.d.ts"
 * @import {Actions} from "@specs/components/metadata/types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {ActionsWithTunnel<Actions>['patch']}
 */
export async function patch({ data: track, ports }) {
  /** @type {ProxiedActions<InputActions>} */
  const input = workerProxy(() => {
    ports.input.start();
    return ports.input;
  });

  const resGet = await input.resolve({ method: "GET", uri: track.uri });
  if (!resGet) return track;

  const resHead = "stream" in resGet
    ? undefined
    : await input.resolve({ method: "HEAD", uri: track.uri });

  const { stats, tags } = await musicMetadataTags({
    stream: "stream" in resGet ? resGet.stream : undefined,
    mimeType: "stream" in resGet ? resGet.mimeType : undefined,
    urls: "url" in resGet
      ? {
        get: resGet.url,
        head: resHead && "url" in resHead ? resHead.url : resGet.url,
      }
      : undefined,
  }).catch(/** @param {Error} err */ (err) => {
    console.warn("audio-file metadata error", err);
    return /** @type {import("@specs/components/metadata/audio-file/types.d.ts").Extraction} */ ({});
  });

  if (!tags && !stats) return track;

  return {
    ...track,
    stats,
    tags,
    updatedAt: new Date().toISOString(),
  };
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { patch });
});
