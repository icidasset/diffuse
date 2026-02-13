/**
 * @import {Playlist, PlaylistItem, Track} from "@definitions/types.d.ts"
 */

/**
 * Creates an empty favourites playlist structure.
 *
 * @returns {Playlist}
 */
export function createEmptyFavouritesPlaylist() {
  const now = new Date().toISOString();

  return /** @type {Playlist} */ ({
    $type: "sh.diffuse.output.playlist",
    id: "favourites",
    name: "Favourites",
    unordered: true,
    items: [],
    createdAt: now,
    updatedAt: now,
  });
}

/**
 * Check if a track is a favourite based on the `PlaylistItem`.
 *
 * @param {Track} track
 * @param {PlaylistItem} item
 */
export function matchItemWithTrack(track, item) {
  const artist = item.criteria.find((c) => c.field === "artist")?.value
    .toString()
    .toLowerCase();
  const title = item.criteria.find((c) => c.field === "title")?.value
    .toString()
    .toLowerCase();

  return track.tags?.artist?.toLowerCase() === artist &&
    track.tags?.title?.toLowerCase() === title;
}
