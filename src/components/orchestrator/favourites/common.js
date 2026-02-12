/**
 * @import {Playlist} from "@definitions/types.d.ts"
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
