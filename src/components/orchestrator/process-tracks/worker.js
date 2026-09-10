import deepDiff from "@fry69/deep-diff";

import { effect, signal } from "~/common/signal.js";
import { announce, ostiary, rpc, workerProxy } from "~/common/worker.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {ActionsWithTunnel, ProxiedActions} from "~/common/worker.d.ts"
 * @import {InputActions} from "@specs/components/input/types.d.ts"
 * @import {Actions as MetadataActions} from "@specs/components/metadata/types.d.ts"
 *
 * @import {Actions} from "@specs/components/orchestrator/process-tracks/types.d.ts"
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
  const { tracks: cachedTracks, disabledUris, onlyUris } = data;

  // Reset progress
  $progress.value = { processed: 0, total: 0 };

  /** @type {ProxiedActions<InputActions>} */
  const input = workerProxy(() => ports.input);

  /** @type {ProxiedActions<MetadataActions>} */
  const metadata = workerProxy(() => ports.metadata);

  ports.input.start();
  ports.metadata.start();

  // When a set of source URIs is provided, only those sources are (re)processed;
  // tracks belonging to any other source are preserved as-is. An empty list
  // reprocesses everything, matching the original behaviour.
  const inScope = (/** @type {Track} */ t) =>
    !onlyUris || onlyUris.length === 0 ||
      onlyUris.some((/** @type {string} */ uri) => t.uri.startsWith(uri));

  // Split disabled tracks out — they are preserved as-is and skipped for listing/metadata
  const isDisabled = (/** @type {Track} */ t) =>
    disabledUris.some((/** @type {string} */ uri) => t.uri.startsWith(uri));

  const scopedTracks = cachedTracks.filter(inScope);
  const otherTracks = cachedTracks.filter((t) => !inScope(t));

  const disabledTracks = scopedTracks.filter(isDisabled);
  const enabledCachedTracks = scopedTracks.filter((t) => !isDisabled(t));

  // List from enabled sources only (within scope)
  const tracks = await input.list(enabledCachedTracks);

  // Persist the full track list immediately so that an interrupted metadata
  // processing run doesn't lose discovered tracks. On next run they'll come
  // back as cachedTracks and only the ones without metadata need reprocessing.
  // Untouched out-of-scope tracks are kept so a scoped run never clobbers them.
  announce("list", [...tracks, ...disabledTracks, ...otherTracks], context);

  // Reset progress
  $progress.value = { processed: 0, total: tracks.length };

  // Fetch metadata if needed
  let processed = 0;
  let patchedCount = 0;
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
        return [...acc, track];
      }

      const patched = await metadata.patch(track);

      processed++;
      patchedCount++;
      $progress.value = { processed, total: tracks.length };

      const result = [...acc, patched];
      if (patchedCount % BATCH_SIZE === 0) announce("patch", result, context);
      return result;
    },
    Promise.resolve([]),
  );

  const allTracks = [...tracksWithMetadata, ...disabledTracks, ...otherTracks];

  // Changed?
  const diff = deepDiff.diff(allTracks, cachedTracks);
  const changed = !!diff;

  // Save if changed
  if (changed) return allTracks;
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
