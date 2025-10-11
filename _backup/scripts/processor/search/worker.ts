import * as Orama from "@orama/orama";
import { getTransferables } from "@okikio/transferables";
import { xxh32 } from "xxh32";
// import { pluginQPS } from "@orama/plugin-qps";

import type { Track } from "@applets/core/types";
import type { State } from "./types";
import { postMessages, provide, transfer } from "@scripts/common";
import { SCHEMA } from "./constants";

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const actions = {
  search,
  supply,
};

const { ports, tasks } = provide({
  actions,
  tasks: { ...actions, data },
});

export type Actions = typeof actions;
export type Tasks = typeof tasks;

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

let state: State = {
  cacheId: "",
  inserted: new Set<string>(),
};

function data() {
  return state;
}

function notify() {
  const d = data();

  postMessages({
    data: {
      type: "data",
      data: d,
    },
    ports: ports.applets,
    transfer: getTransferables(d),
  });
}

// TODO: Generate embeddings plugin
//
// I tried this and getting some bundler/vite errors about a default import.
//
// const plugin = await pluginEmbeddings({
//   embeddings: {
//     defaultProperty: "embeddings",
//     onInsert: {
//       generate: true,
//       // Properties to use for generating embeddings at insert time.
//       // These properties will be concatenated and used to generate embeddings.
//       properties: ["album", "artist", "title", "year", "kind", "genre"],
//       // verbose: true,
//     },
//   },
// });
//
// TODO:
//
// Does not work either.
// `TypeError: a is undefined`
//
// pluginQPS()

const PLUGINS: Orama.OramaPlugin[] = [];

// Search through tracks
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

async function search(term: string): Promise<Track[]> {
  term = term.trim();
  const tracks: Track[] = await _search(term, []);
  return transfer(tracks);
}

async function _search(term: string, tracks: Track[]) {
  console.log("Search with offset:", tracks.length);

  const results = await Orama.search(db, {
    // mode: "hybrid",
    term,
    limit: 10000,
    offset: tracks.length,
  });

  const allTracks = tracks.concat(results.hits.map((hit) => hit.document as unknown as Track));

  if (allTracks.length < results.count) {
    return await _search(term, allTracks);
  } else {
    return allTracks;
  }
}

async function supply(tracks: Track[]) {
  // TODO: Generate a hash based on the track itself,
  //       so we can detect changes to tags or other data.

  const ids: string[] = [];
  const tracksMap: Record<string, Track> = {};

  tracks.forEach((track) => {
    ids.push(track.id);
    tracksMap[track.id] = track;
  });

  const currentSet = state.inserted;
  const newSet = new Set(ids);

  const removedIds = currentSet.difference(newSet);
  const newIds = newSet.difference(currentSet);
  const newTracks = Array.from(newIds).map((id) => tracksMap[id]);

  await Orama.removeMultiple(db, Array.from(removedIds));
  await Orama.insertMultiple(db, newTracks);

  state.inserted = newSet;
  state.cacheId = xxh32(ids.sort().join("")).toString();

  notify();
}
