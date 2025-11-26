import deepDiff from "@fry69/deep-diff";

import { ostiary, rpc, workerProxy } from "@common/worker.js";

/**
 * @import {Track} from "@definitions/types.d.ts"
 * @import {ProxiedActions} from "@common/worker.d.ts"
 * @import {InputActions} from "@components/input/types.d.ts"
 * @import {Actions as MetadataProcessorActions} from "@components/processor/metadata/types.d.ts"
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions["process"]}
 */
export async function process(args) {
  const { ports } = args;
  const cachedTracks = args.tracks;

  /** @type {ProxiedActions<InputActions>} */
  const input = workerProxy(() => ports.input);

  /** @type {ProxiedActions<MetadataProcessorActions>} */
  const metadataProcessor = workerProxy(() => ports.metadataProcessor);

  ports.input.start();
  ports.metadataProcessor.start();

  // Contextualize
  await input.contextualize(cachedTracks);

  // List
  const tracks = await input.list(cachedTracks);

  // Fetch metadata if needed
  const tracksWithMetadata = await tracks.reduce(
    /**
     * @param {Promise<Track[]>} promise
     * @param {Track} track
     */
    async (promise, track) => {
      const acc = await promise;

      if (track.tags && track.stats) return [...acc, track];

      const resGet = await input.resolve({
        method: "GET",
        uri: track.uri,
      });

      if (!resGet) return [...acc, track];

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
  rpc(context, { process });
});
