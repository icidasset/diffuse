import deepDiff from "@fry69/deep-diff";
import { define, ostiary, proxyProvider, use } from "@common/worker.js";
import { INPUT_ACTIONS } from "@common/constants.js";

/**
 * @import {InputActions, Track} from "@common/types.d.ts"
 * @import {ProxyProvider} from "@common/worker.d.ts"
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

  /** @type {ProxyProvider<InputActions>} */
  const inputProvider = proxyProvider(INPUT_ACTIONS);
  const input = inputProvider(ports.input);

  /** @type {ProxyProvider<MetadataProcessorActions>} */
  const metadataProcessorProvider = proxyProvider(["supply"]);
  const metadataProcessor = metadataProcessorProvider(ports.metadataProcessor);

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

      const resHead = await input.resolve({
        method: "HEAD",
        uri: track.uri,
      });

      if (!resGet) return [...acc, track];

      const { stats, tags } = await metadataProcessor.supply({
        urls: { get: resGet.url, head: resHead?.url || resGet.url },
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

ostiary((port) => {
  define("process", process, port);
});
