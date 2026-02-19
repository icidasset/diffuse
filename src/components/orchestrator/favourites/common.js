/**
 * Filter playlist items that belong to the favourites playlist.
 *
 * @param {import("@definitions/types.d.ts").PlaylistItem[]} playlistItems
 * @returns {import("@definitions/types.d.ts").PlaylistItem[]}
 */
export function filterFavourites(playlistItems) {
  return playlistItems.filter((item) => item.playlist === "Favourites");
}
