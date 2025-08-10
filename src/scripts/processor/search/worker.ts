import * as Orama from "@orama/orama";
// import { pluginQPS } from "@orama/plugin-qps";

import type { Track } from "@applets/core/types";
import { expose, provide, transfer } from "@scripts/common";
import { SCHEMA } from "./constants";
import type { State } from "./types";

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

let state: State = {
  inserted: new Set<string>(),
};

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

// 🚀

const actions = {
  search,
  supply,
};

const { tasks } = provide({
  actions,
  tasks: actions,
});

export type Actions = typeof actions;
export type Tasks = typeof tasks;

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

async function search(term: string): Promise<Track[]> {
  const results = await Orama.search(db, {
    // mode: "hybrid",
    term,
  });

  const tracks = results.hits.map((hit) => hit.document as unknown as Track);
  return transfer(tracks);
}

async function supply(tracks: Track[]) {
  // TODO: Generate a hash based on the track itself,
  //       so we can detect changes to tags or other data.

  const ids = [];
  const tracksMap: Record<string, Track> = {};

  tracks.forEach((track) => {
    ids.push(track.id);
    tracksMap[track.id] = track;
  });

  const currentSet = state.inserted;
  const newSet = new Set(tracks.map((t) => t.id));

  const removedIds = currentSet.difference(newSet);
  const newIds = newSet.difference(currentSet);
  const newTracks = Array.from(newIds).map((id) => tracksMap[id]);

  await Orama.removeMultiple(db, Array.from(removedIds));
  await Orama.insertMultiple(db, newTracks);

  state.inserted = newSet;
}
