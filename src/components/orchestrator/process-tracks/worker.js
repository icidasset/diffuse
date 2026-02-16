import deepDiff from "@fry69/deep-diff";

import { effect, signal } from "@common/signal.js";
import { announce, ostiary, rpc, workerProxy } from "@common/worker.js";

/**
 * @import {Track} from "@definitions/types.d.ts"
 * @import {ActionsWithTunnel, ProxiedActions} from "@common/worker.d.ts"
 * @import {InputActions} from "@components/input/types.d.ts"
 * @import {Actions as MetadataProcessorActions} from "@components/processor/metadata/types.d.ts"
 *
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

/** @type {import("@common/signal.d.ts").Signal<{processed: number, total: number}>} */
const $progress = signal({ processed: 0, total: 0 });

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {ActionsWithTunnel<Actions>["process"]}
 */
export async function process({ data, ports }) {
  const cachedTracks = data;

  // Reset progress
  $progress.value = { processed: 0, total: 0 };

  /** @type {ProxiedActions<InputActions>} */
  const input = workerProxy(() => ports.input);

  /** @type {ProxiedActions<MetadataProcessorActions>} */
  const metadataProcessor = workerProxy(() => ports.metadataProcessor);

  ports.input.start();
  ports.metadataProcessor.start();

  // List
  const tracks = await input.list(cachedTracks);

  // Reset progress
  $progress.value = { processed: 0, total: tracks.length };

  // Fetch metadata if needed
  let processed = 0;

  const tracksWithMetadata = await tracks.reduce(
    /**
     * @param {Promise<Track[]>} promise
     * @param {Track} track
     */
    async (promise, track) => {
      const acc = await promise;

      if (track.tags && track.stats) {
        processed++;
        $progress.value = { processed, total: tracks.length };
        return [...acc, track];
      }

      const resGet = await input.resolve({
        method: "GET",
        uri: track.uri,
      });

      if (!resGet) {
        processed++;
        $progress.value = { processed, total: tracks.length };
        return [...acc, track];
      }

      const resHead = "stream" in resGet ? undefined : await input.resolve({
        method: "HEAD",
        uri: track.uri,
      });

      const { stats, tags } = await metadataProcessor.supply({
        stream: "stream" in resGet ? resGet.stream : undefined,
        urls: "url" in resGet
          ? {
            get: resGet.url,
            head: resHead && "url" in resHead ? resHead.url : resGet.url,
          }
          : undefined,
      });

      processed++;
      $progress.value = { processed, total: tracks.length };

      return [...acc, { ...track, stats, tags }];
    },
    Promise.resolve([]),
  );

  // Changed?
  const diff = deepDiff.diff(tracksWithMetadata, cachedTracks);
  const changed = !!diff;

  // Save if changed
  if (changed) return tracksWithMetadata;
  return null;
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { process, progress: $progress.get });

  // Communicate state
  effect(() => announce("progress", $progress.value, context));
});
