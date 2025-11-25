import * as Orama from "@orama/orama";
import { xxh32 } from "xxh32";
// import { pluginQPS } from "@orama/plugin-qps";

import { SCHEMA } from "./constants.js";
import { ostiary, rpc } from "@common/worker.js";
import { signal } from "@common/signal.js";

/**
 * @import {Track} from "@definitions/types.d.ts"
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

export const cacheId = signal(/** @type {string} */ (""));
export const inserted = signal(/** @type {Set<string>} */ (new Set()));

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
export async function search(term) {
  term = term.trim();
  return await _search(term, []);
}

/**
 * @type {Actions['supply']}
 */
export async function supply(tracks) {
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

  const currentSet = inserted.value;
  const newSet = new Set(ids);

  const removedIds = currentSet.difference(newSet);
  const newIds = newSet.difference(currentSet);
  const newTracks = Array.from(newIds).map((id) => tracksMap[id]);

  await Orama.removeMultiple(await db, Array.from(removedIds));
  await Orama.insertMultiple(await db, newTracks);

  inserted.value = newSet;
  cacheId.value = xxh32(ids.sort().join("")).toString();
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, {
    search,
    supply,
  });
});

////////////////////////////////////////////
// ⛔️
////////////////////////////////////////////

/**
 * @param {string} term
 * @param {Track[]} tracks
 */
async function _search(term, tracks) {
  console.log("Search with offset:", tracks.length);

  const results = await Orama.search(await db, {
    // mode: "hybrid",
    term,
    limit: 10000,
    offset: tracks.length,
  });

  const allTracks = tracks.concat(
    results.hits.map((
      hit,
    ) => /** @type {Track} */ (/** @type {unknown} */ (hit.document))),
  );

  if (allTracks.length < results.count) {
    return await _search(term, allTracks);
  } else {
    return allTracks;
  }
}
