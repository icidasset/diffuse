import deepDiff from "@fry69/deep-diff";

import { effect, signal } from "~/common/signal.js";
import { announce, ostiary, rpc, workerProxy } from "~/common/worker.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {ActionsWithTunnel, ProxiedActions} from "~/common/worker.d.ts"
 * @import {InputActions} from "~/components/input/types.d.ts"
 * @import {Actions as MetadataActions} from "~/components/metadata/types.d.ts"
 *
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

/** @type {import("~/common/signal.d.ts").Signal<{processed: number, total: number}>} */
const $progress = signal({ processed: 0, total: 0 }, {
  compare: (a, b) => !deepDiff(a, b),
});

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

  /** @type {ProxiedActions<MetadataActions>} */
  const metadata = workerProxy(() => ports.metadata);

  ports.input.start();
  ports.metadata.start();

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

      if ((track.tags && track.stats) || track.kind === "placeholder") {
        processed++;
        $progress.value = { processed, total: tracks.length };
        return [...acc, track];
      }

      const patched = await metadata.patch(track);

      processed++;
      $progress.value = { processed, total: tracks.length };

      return [...acc, patched];
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
