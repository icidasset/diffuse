import * as URI from "fast-uri";
import { musicMetadataTags } from "~/components/metadata/common.js";
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
 * Time budget before this provider aborts the underlying HTTP extraction.
 * On timeout, in-flight requests are cancelled so a slow source doesn't keep
 * occupying server resources (or hold up the configurator chain).
 */
const PROVIDER_TIMEOUT_MS = 60_000;

/**
 * @type {ActionsWithTunnel<Actions>['get']}
 */
export async function get({ data: track, ports }) {
  /** @type {ProxiedActions<InputActions>} */
  const input = workerProxy(() => {
    ports.input.start();
    return ports.input;
  });

  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), PROVIDER_TIMEOUT_MS);

  try {
    const resGet = await input.resolve({ method: "GET", uri: track.uri });
    if (!resGet) {
      // Couldn't obtain the stream/url — a transient input failure, NOT a
      // confirmation that the track has no artwork. Throw so the artwork
      // configurator tries the next provider (e.g. an online source) instead of
      // letting us report "no art" which the orchestrator would latch forever.
      throw new Error("audio-metadata: input.resolve returned no result");
    }

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
      signal: controller.signal,
    });

    // NOTE: no `.catch` here. If extraction fails (or is aborted) it throws,
    // which lets the configurator fall through to the next provider rather than
    // treating a processing failure as "this track has no artwork"

    const pictures = meta.artwork ?? [];
    if (!pictures.length) return null;

    return pictures[0].data;
  } finally {
    clearTimeout(timer);
  }
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { get });
});
