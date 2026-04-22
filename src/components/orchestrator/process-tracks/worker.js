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
 * @param {any} context
 * @returns {ActionsWithTunnel<Actions>["process"]}
 */
const process = (
  context,
) => /** @type {ActionsWithTunnel<Actions>["process"]} */ (async (
  { data, ports },
) => {
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

  // Persist the full track list immediately so that an interrupted metadata
  // processing run doesn't lose discovered tracks. On next run they'll come
  // back as cachedTracks and only the ones without metadata need reprocessing.
  announce("list", tracks, context);

  // Reset progress
  $progress.value = { processed: 0, total: tracks.length };

  // Fetch metadata if needed
  let processed = 0;
  const BATCH_SIZE = 100;

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
        const result = [...acc, track];
        if (processed % BATCH_SIZE === 0) announce("batch", result, context);
        return result;
      }

      const patched = await metadata.patch(track);

      processed++;
      $progress.value = { processed, total: tracks.length };

      const result = [...acc, patched];
      if (processed % BATCH_SIZE === 0) announce("batch", result, context);
      return result;
    },
    Promise.resolve([]),
  );

  // Changed?
  const diff = deepDiff.diff(tracksWithMetadata, cachedTracks);
  const changed = !!diff;

  // Save if changed
  if (changed) return tracksWithMetadata;
  return null;
});

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { process: process(context), progress: $progress.get });

  // Communicate state
  effect(() => announce("progress", $progress.value, context));
});
