import { ostiary, rpc } from "@common/worker.js";
import { createEmptyFavouritesPlaylist } from "./common.js";

/**
 * @import {Playlist, Track} from "@definitions/types.d.ts"
 * @import {Actions} from "./types.d.ts"
 */

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

  // Get existing favourite track IDs
  const existingIds = new Set(
    favourites?.items.map((item) => /** @type {string} */ (
      /** @type {unknown} */ (item.criteria.find((c) => c.field === "id")
        ?.value)
    )) ?? [],
  );

  // Filter out tracks that are already favourites
  const newTracks = tracks.filter((track) =>
    !existingIds.has(
      /** @type {string} */ (/** @type {unknown} */ (track.id)),
    )
  );
  if (newTracks.length === 0) return null;

  // Create or update favourites playlist
  const now = new Date().toISOString();
  const newItems = newTracks.map((track) => ({
    criteria: [{ field: "id", value: /** @type {unknown} */ (track.id) }],
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

  // Create set of track IDs to remove
  const idsToRemove = new Set(
    tracks.map((track) => /** @type {unknown} */ (track.id)),
  );

  // Filter out items matching the tracks to remove
  const updatedItems = favourites.items.filter((item) => {
    const trackId = /** @type {string} */ (
      /** @type {unknown} */ (item.criteria.find((c) => c.field === "id")
        ?.value)
    );
    return !idsToRemove.has(trackId);
  });

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

  // Get existing favourite track IDs
  const existingIds = new Set(
    favourites?.items.map((item) => /** @type {string} */ (
      /** @type {unknown} */ (item.criteria.find((c) => c.field === "id")
        ?.value)
    )) ?? [],
  );

  // Separate tracks into those to add and those to remove
  const toAdd = tracks.filter((track) =>
    !existingIds.has(
      /** @type {string} */ (/** @type {unknown} */ (track.id)),
    )
  );
  const toRemove = tracks.filter((track) =>
    existingIds.has(/** @type {string} */ (/** @type {unknown} */ (track.id)))
  );

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
