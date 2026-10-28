import * as Orama from "@orama/orama";
import { xxh32 } from "xxh32";

import { SCHEMA } from "./constants.js";
import { countWords, matchesWords, searchablePaths } from "~/common/search.js";
import { announce, ostiary, rpc } from "~/common/worker.js";
import { effect, signal } from "~/common/signal.js";

/**
 * @import {SearchParams} from "@orama/orama";
 *
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {Actions, Schema} from "@specs/components/orchestrator/scoped-tracks/types.d.ts"
 */

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

/** @type {Set<string>} */
export let inserted = new Set();
export let insertedFingerprint = "";

// Communicated state
export const $supplyFingerprint = signal(
  /** @type {string | undefined} */ (undefined),
);

////////////////////////////////////////////
// DATABASE
////////////////////////////////////////////

/**
 * @type {Orama.OramaPlugin[]}
 */
const PLUGINS = [];

const db = Orama.create({
  schema: SCHEMA,
  plugins: PLUGINS,
});

/**
 * The index tokenizer drops duplicate tokens, which makes `goo goo` behave
 * like `goo`. This one keeps them so repeated terms can each require their own
 * matching word.
 */
const tokenizer = Orama.components.tokenizer.createTokenizer({
  language: db.tokenizer.language,
  allowDuplicates: true,
});

/** String fields of the schema, i.e. what the index searches. */
const SEARCHABLE_PATHS = searchablePaths(SCHEMA);

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions['search']}
 */
export async function search(params) {
  const term = "term" in params && typeof params.term === "string"
    ? params.term.trim()
    : undefined;

  if (!term) return await _search(params, []);

  const keywords = tokenizer.tokenize(term);
  const distinct = [...new Set(keywords)];

  if (!distinct.length) return [];

  // AND: every distinct keyword must match somewhere in the document.
  //
  // Orama's `threshold: 0` is meant to express this, but it counts *matched
  // index words* per property rather than distinct keywords: with prefix
  // matching ("love" also matching "lovely") one keyword can satisfy the
  // count and leak documents that don't contain the others — i.e. OR
  // behaviour. Searching each keyword separately and intersecting the hits
  // enforces a real AND, and also lets keywords match in different
  // properties (artist + album, for example).
  const hits = await Promise.all(
    distinct.map((keyword) => _search({ ...params, term: keyword }, [])),
  );

  const [first = [], ...rest] = hits;
  const matched = rest.map((tracks) => new Set(tracks.map((t) => t.id)));

  const results = first.filter((track) =>
    matched.every((set) => set.has(track.id))
  );

  // Intersecting cannot tell one word satisfying a keyword twice from two
  // separate occurrences, so repeated terms are checked against the words
  // actually present.
  if (keywords.length === distinct.length) return results;

  return results.filter((track) =>
    matchesWords(
      countWords(tokenizer, SEARCHABLE_PATHS, track),
      keywords,
    )
  );
}

/**
 * @type {Actions['supply']}
 */
export async function supply({ tracks }) {
  // TODO: Generate a hash based on the track itself,
  //       so we can detect changes to tags or other data.

  /** @type {Map<string, Track>} */
  const tracksMap = new Map();

  for (const track of tracks) {
    if (!track.tags) continue;
    tracksMap.set(track.id, track);
  }

  const ids = Array.from(tracksMap.keys());
  const idsString = ids.sort().join("");
  const fingerprint = xxh32(idsString).toString();

  if (fingerprint === insertedFingerprint) return;

  const currentSet = inserted;
  const newSet = new Set(ids);

  inserted = newSet;
  insertedFingerprint = fingerprint;

  const removedIds = currentSet.difference(newSet);
  const newIds = newSet.difference(currentSet);

  /** @type {Track[]} */
  const newTracks = [];

  for (const id of newIds) {
    const track = tracksMap.get(id);
    if (track) newTracks.push(track);
  }

  await Orama.removeMultiple(db, Array.from(removedIds));
  await Orama.insertMultiple(db, newTracks);

  $supplyFingerprint.value = fingerprint;
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
  // -------

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
    threshold: 0, // One keyword per call; ANDing across keywords is done in `search`
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
