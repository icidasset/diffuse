import { Temporal } from "@js-temporal/polyfill";
import { ostiary, rpc } from "@common/worker.js";
import { filterFavourites } from "./common.js";

/**
 * @import {PlaylistItem, Track} from "@definitions/types.d.ts"
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// HELPERS
////////////////////////////////////////////

/**
 * Build a matching key from a track's tags.
 * @param {Track} track
 * @returns {string}
 */
function matchKey(track) {
  return `${track.tags?.artist ?? ""}.${track.tags?.title ?? ""}`;
}

/**
 * Extract the matching key from a playlist item's criteria.
 * @param {PlaylistItem} item
 * @returns {string}
 */
function itemMatchKey(item) {
  const artist = item.criteria.find((c) => c.field === "tags.artist")?.value ??
    "";
  const title = item.criteria.find((c) => c.field === "tags.title")?.value ??
    "";
  return `${artist}.${title}`;
}

/**
 * Create a favourites playlist item from a track.
 * @param {Track} track
 * @returns {PlaylistItem}
 */
function createFavouriteItem(track) {
  const transformations = ["toLowerCase"];
  const now = Temporal.Now.zonedDateTimeISO().toString();

  return /** @type {PlaylistItem} */ ({
    $type: "sh.diffuse.output.playlistItem",
    id: crypto.randomUUID(),
    playlist: "Favourites",
    criteria: [
      {
        field: "tags.artist",
        value: /** @type {unknown} */ (track.tags?.artist),
        transformations,
      },
      {
        field: "tags.title",
        value: /** @type {unknown} */ (track.tags?.title),
        transformations,
      },
    ],
    createdAt: now,
    updatedAt: now,
  });
}

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * Add one or more tracks to favourites.
 * @type {Actions["include"]}
 */
export async function include({ playlistItems, tracks }) {
  if (tracks.length === 0) return null;

  const favourites = filterFavourites(playlistItems);

  const existingKeys = new Set(
    favourites.map((item) => itemMatchKey(item)),
  );

  const newTracks = tracks.filter((track) =>
    !existingKeys.has(matchKey(track))
  );

  if (newTracks.length === 0) return null;

  const newItems = newTracks.map((track) => createFavouriteItem(track));

  return [...playlistItems, ...newItems];
}

/**
 * Remove one or more tracks from favourites.
 * @type {Actions["expel"]}
 */
export async function expel({ playlistItems, tracks }) {
  if (tracks.length === 0) return null;

  const keysToRemove = new Set(tracks.map((track) => matchKey(track)));

  const updatedItems = playlistItems.filter((item) =>
    item.playlist !== "Favourites" || !keysToRemove.has(itemMatchKey(item))
  );

  if (updatedItems.length === playlistItems.length) return null;

  return updatedItems;
}

/**
 * Toggle favourite status for one or more tracks.
 * @type {Actions["toggle"]}
 */
export async function toggle({ playlistItems, tracks }) {
  if (tracks.length === 0) return null;

  const favourites = filterFavourites(playlistItems);

  const existingKeys = new Set(
    favourites.map((item) => itemMatchKey(item)),
  );

  const toAdd = tracks.filter((track) => !existingKeys.has(matchKey(track)));
  const toRemove = tracks.filter((track) => existingKeys.has(matchKey(track)));

  let result = playlistItems;

  if (toAdd.length > 0) {
    const added = await include({ playlistItems: result, tracks: toAdd });
    if (added) result = added;
  }

  if (toRemove.length > 0) {
    const removed = await expel({ playlistItems: result, tracks: toRemove });
    if (removed) result = removed;
  }

  if (result === playlistItems) return null;
  return result;
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { include, expel, toggle });
});
