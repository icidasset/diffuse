import * as TID from "@atcute/tid";

/**
 * @import {PlaylistItem, Track} from "~/definitions/types.d.ts"
 */

/**
 * Create a favourites playlist item from a track.
 * @param {Track} track
 * @returns {PlaylistItem}
 */
export function createFavouriteItem(track) {
  const transformations = ["toLowerCase"];
  const now = new Date().toISOString();

  return /** @type {PlaylistItem} */ ({
    $type: "sh.diffuse.output.playlistItem",
    id: TID.now(),
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

/**
 * Filter playlist items that belong to the favourites playlist.
 *
 * @param {import("~/definitions/types.d.ts").PlaylistItem[]} playlistItems
 * @returns {import("~/definitions/types.d.ts").PlaylistItem[]}
 */
export function filterFavourites(playlistItems) {
  return playlistItems.filter((item) => item.playlist === "Favourites");
}
