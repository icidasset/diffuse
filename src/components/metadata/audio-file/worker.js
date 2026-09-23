import * as URI from "fast-uri";
import { ostiary, rpc, workerProxy } from "~/common/worker.js";
import { musicMetadataTags } from "~/components/metadata/common.js";

/**
 * @import {ActionsWithTunnel, ProxiedActions} from "~/common/worker.d.ts"
 * @import {InputActions} from "@specs/components/input/types.d.ts"
 * @import {Actions} from "@specs/components/metadata/types.d.ts"
 */

/**
 * Time budget for metadata extraction. On timeout, in-flight requests are
 * cancelled so a slow source doesn't keep occupying server resources (or
 * wedge the whole process-tracks run). `musicMetadataTags`' range requests
 * (`@tokenizer/http`) are only cancellable through this signal — the library
 * itself has no timeout.
 */
const PROVIDER_TIMEOUT_MS = 60_000;

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

  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), PROVIDER_TIMEOUT_MS);

  try {
    const resGet = await input.resolve({ method: "GET", uri: track.uri });
    if (!resGet) return track;

    const resHead = "stream" in resGet
      ? undefined
      : await input.resolve({ method: "HEAD", uri: track.uri });

    const trackUri = URI.parse(track.uri);
    const trackPathParts = trackUri.path?.split("/");
    const filename = trackPathParts?.[trackPathParts.length - 1];

    const { stats, tags } = await musicMetadataTags({
      filename,
      stream: "stream" in resGet ? resGet.stream : undefined,
      mimeType: "stream" in resGet ? resGet.mimeType : undefined,
      urls: "url" in resGet
        ? {
          get: resGet.url,
          head: resHead && "url" in resHead ? resHead.url : resGet.url,
        }
        : undefined,
      signal: controller.signal,
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
  } finally {
    clearTimeout(timer);
  }
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { patch });
});
