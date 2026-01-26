import * as Orama from "@orama/orama";
import { xxh32 } from "xxh32";
// import { pluginQPS } from "@orama/plugin-qps";

import { SCHEMA } from "./constants.js";
import { announce, ostiary, rpc } from "@common/worker.js";
import { effect, signal } from "@common/signal.js";

/**
 * @import {SearchParams} from "@orama/orama";
 *
 * @import {Track} from "@definitions/types.d.ts"
 * @import {Actions, Schema} from "./types.d.ts"
 */

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

export const $inserted = signal(/** @type {Set<string>} */ (new Set()), {
  eager: true,
});

// Communicated state
export const $supplyFingerprint = signal(
  /** @type {string | undefined} */ (undefined),
);

////////////////////////////////////////////
// DATABASE
////////////////////////////////////////////

// TODO:
// * pluginEmbeddings
// * pluginQPS

/**
 * @type {Orama.OramaPlugin[]}
 */
const PLUGINS = [];

const db = Orama.create({
  schema: SCHEMA,
  plugins: PLUGINS,
  // components: {
  // TODO:
  // https://docs.orama.com/open-source/usage/insert#remote-document-storing
  // documentStore: { ... }
  // },
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions['search']}
 */
export async function search(params) {
  return await _search(
    "term" in params && typeof params.term === "string"
      ? { ...params, term: params.term.trim() }
      : params,
    [],
  );
}

/**
 * @type {Actions['supply']}
 */
export async function supply({ tracks }) {
  // TODO: Generate a hash based on the track itself,
  //       so we can detect changes to tags or other data.

  /** @type {string[]} */
  const ids = [];

  /** @type {Record<string, Track>} */
  const tracksMap = {};

  tracks.forEach((track) => {
    ids.push(track.id);
    tracksMap[track.id] = track;
  });

  const currentSet = $inserted.value;
  const newSet = new Set(ids);

  $inserted.value = newSet;

  const removedIds = currentSet.difference(newSet);
  const newIds = newSet.difference(currentSet);
  const newTracks = Array.from(newIds).map((id) => tracksMap[id]);

  await Orama.removeMultiple(db, Array.from(removedIds));
  await Orama.insertMultiple(db, newTracks);

  $supplyFingerprint.value = xxh32(ids.sort().join("")).toString();
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, {
    search,
    supply,

    // State
    supplyFingerprint: $supplyFingerprint.get,
  });

  // Effects

  // Communicate state
  effect(() =>
    announce("supplyFingerprint", $supplyFingerprint.value, context)
  );
});

////////////////////////////////////////////
// ⛔️
////////////////////////////////////////////

/**
 * @param {SearchParams<Schema>} params
 * @param {Track[]} tracks
 */
async function _search(params, tracks) {
  const results = await Orama.search(db, {
    // @ts-ignore: No clue what the correct type is for this one
    sortBy,
    ...params,
    // mode: "hybrid",
    limit: 10000,
    offset: tracks.length,
  });

  const allTracks = tracks.concat(
    results.hits.map((
      hit,
    ) => /** @type {Track} */ (/** @type {unknown} */ (hit.document))),
  );

  if (allTracks.length < results.count) {
    return await _search(params, allTracks);
  } else {
    return allTracks;
  }
}

/**
 * @type {Orama.CustomSorterFunction<Orama.TypedDocument<Schema>>}
 */
function sortBy(a, b) {
  const artist = (a[2].tags?.artist ?? "").localeCompare(
    b[2].tags?.artist ?? "",
  );
  if (artist != 0) return artist;

  const album = (a[2].tags?.album ?? "").localeCompare(
    b[2].tags?.album ?? "",
  );
  if (album != 0) return album;

  const discNo = (a[2].tags?.disc?.no ?? 0) - (b[2].tags?.disc?.no ?? 0);
  if (discNo != 0) return discNo;

  const trackNo = (a[2].tags?.track?.no ?? 0) - (b[2].tags?.track?.no ?? 0);
  if (trackNo != 0) return trackNo;

  const title = (a[2].tags?.title ?? "").localeCompare(
    b[2].tags?.title ?? "",
  );
  if (title != 0) return title;
  return 0;
}
