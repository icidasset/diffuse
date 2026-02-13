import { ostiary, rpc } from "@common/worker.js";
import { createEmptyFavouritesPlaylist } from "./common.js";

/**
 * @import {Playlist, Track} from "@definitions/types.d.ts"
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
 * @param {{ criteria: { field: string; value: unknown }[] }} item
 * @returns {string}
 */
function itemMatchKey(item) {
  const artist = item.criteria.find((c) => c.field === "tags.artist")?.value ?? "";
  const title = item.criteria.find((c) => c.field === "tags.title")?.value ?? "";
  return `${artist}.${title}`;
}

/**
 * Create criteria entries from a track's tags.
 * @param {Track} track
 */
function trackCriteria(track) {
  return [
    { field: "tags.artist", value: /** @type {unknown} */ (track.tags?.artist) },
    { field: "tags.title", value: /** @type {unknown} */ (track.tags?.title) },
  ];
}

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * Add one or more tracks to favourites.
 * @type {Actions["include"]}
 */
export async function include({ playlists, tracks }) {
  if (tracks.length === 0) return null;

  const favourites = playlists.find((p) => p.id === "favourites");

  // Get existing favourite keys (artist + title)
  const existingKeys = new Set(
    favourites?.items.map((item) => itemMatchKey(item)) ?? [],
  );

  // Filter out tracks that are already favourites
  const newTracks = tracks.filter((track) =>
    !existingKeys.has(matchKey(track))
  );

  if (newTracks.length === 0) return null;

  // Create or update favourites playlist
  const now = new Date().toISOString();
  const newItems = newTracks.map((track) => ({
    criteria: trackCriteria(track),
  }));

  /** @type {Playlist} */
  const updatedFavourites = favourites
    ? /** @type {Playlist} */ ({
      ...favourites,
      items: [...favourites.items, ...newItems],
      updatedAt: now,
    })
    : /** @type {Playlist} */ ({
      ...createEmptyFavouritesPlaylist(),
      items: newItems,
    });

  const otherPlaylists = playlists.filter((p) => p.id !== "favourites");
  return [...otherPlaylists, updatedFavourites];
}

/**
 * Remove one or more tracks from favourites.
 * @type {Actions["expel"]}
 */
export async function expel({ playlists, tracks }) {
  if (tracks.length === 0) return null;

  const favourites = playlists.find((p) => p.id === "favourites");
  if (!favourites) return null;

  // Create set of track keys to remove
  const keysToRemove = new Set(tracks.map((track) => matchKey(track)));

  // Filter out items matching the tracks to remove
  const updatedItems = favourites.items.filter((item) =>
    !keysToRemove.has(itemMatchKey(item))
  );

  // If nothing changed, don't save
  if (updatedItems.length === favourites.items.length) return null;

  const now = new Date().toISOString();

  /** @type {Playlist} */
  const updatedFavourites = {
    ...favourites,
    items: updatedItems,
    updatedAt: now,
  };

  const otherPlaylists = playlists.filter((p) => p.id !== "favourites");
  return [...otherPlaylists, updatedFavourites];
}

/**
 * Toggle favourite status for one or more tracks.
 * @type {Actions["toggle"]}
 */
export async function toggle({ playlists, tracks }) {
  if (tracks.length === 0) return null;

  const favourites = playlists.find((p) => p.id === "favourites");

  // Get existing favourite keys (artist + title)
  const existingKeys = new Set(
    favourites?.items.map((item) => itemMatchKey(item)) ?? [],
  );

  // Separate tracks into those to add and those to remove
  const toAdd = tracks.filter((track) => !existingKeys.has(matchKey(track)));
  const toRemove = tracks.filter((track) => existingKeys.has(matchKey(track)));

  // Apply add then remove in sequence
  let result = playlists;

  if (toAdd.length > 0) {
    const added = await include({ playlists: result, tracks: toAdd });
    if (added) result = added;
  }

  if (toRemove.length > 0) {
    const removed = await expel({ playlists: result, tracks: toRemove });
    if (removed) result = removed;
  }

  // If nothing changed, return null
  if (result === playlists) return null;
  return result;
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { include, expel, toggle });
});
