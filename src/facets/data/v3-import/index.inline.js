import * as TID from "@atcute/tid";
import foundation from "~/common/foundation.js";

document.title = "V3.x Import | Diffuse";

const main = /** @type {HTMLElement} */ (document.querySelector("main"));
main.classList.add("has-loaded");

/**
 * @import {PlaylistItem, Track} from "~/definitions/types.d.ts"
 */

// Setup
const favourites = await foundation.orchestrator.favourites();
const output = await foundation.orchestrator.output();

// Elements
const fileInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#file"));
const importFavouritesBtn =
  /** @type {HTMLButtonElement} */ (document.querySelector(
    "#import-favourites",
  ));
const importPlaylistItemsBtn =
  /** @type {HTMLButtonElement} */ (document.querySelector(
    "#import-playlist-items",
  ));
const statusEl = /** @type {HTMLElement} */ (document.querySelector("#status"));

/** @type {Record<string, any> | null} */
let json = null;

/**
 * Show a status message.
 * @param {string} message
 * @param {"success" | "error"} type
 */
function showStatus(message, type) {
  statusEl.textContent = message;
  statusEl.className = `status status--${type}`;
  statusEl.hidden = false;
}

// Parse file on selection
fileInput.onchange = async () => {
  const file = fileInput.files?.[0];

  json = null;
  statusEl.hidden = true;
  importFavouritesBtn.disabled = true;
  importPlaylistItemsBtn.disabled = true;

  if (!file) return;

  try {
    json = JSON.parse(await file.text());
  } catch (err) {
    console.error("Failed to parse JSON:", err);
    showStatus(
      `Failed to parse JSON: ${/** @type {Error} */ (err).message}`,
      "error",
    );
    return;
  }

  if (json?.favourites?.data?.length > 0) {
    importFavouritesBtn.disabled = false;
  }

  if (json?.playlists?.data?.length > 0) {
    importPlaylistItemsBtn.disabled = false;
  }
};

// Import favourites on button click
importFavouritesBtn.onclick = async () => {
  /** @type {any[]} */
  const items = json?.favourites?.data;
  if (!items || items.length === 0) return;

  try {
    /** @type {Track[]} */
    const tracks = items.map((item) => ({
      $type: "sh.diffuse.output.track",
      id: "",
      uri: "",
      tags: {
        artist: item.artist ?? "",
        title: item.title ?? "",
      },
    }));

    await favourites.include(tracks);
    showStatus(`Imported ${tracks.length} favourite(s).`, "success");
  } catch (err) {
    console.error("Import failed:", err);
    showStatus(`Import failed: ${/** @type {Error} */ (err).message}`, "error");
  }
};

// Import playlist items on button click
importPlaylistItemsBtn.onclick = async () => {
  /** @type {any[]} */
  const items = json?.playlists?.data;
  if (!items || items.length === 0) return;

  try {
    const now = new Date().toISOString();

    const existingCol = output.playlistItems.collection();
    /** @type {any[]} */
    const existing = existingCol.state === "loaded" ? existingCol.data : [];
    const existingPlaylistNames = new Set(existing.map((p) => p.playlist));

    const newPlaylistItems = items
      .filter((item) => !existingPlaylistNames.has(item.name ?? "Untitled"))
      .flatMap((item) => {
        const playlistName = item.name ?? "Untitled";
        const isUnordered = !!item.collection;

        /** @type {PlaylistItem[]} */
        const playlistItems = [];

        /** @type {any[]} */ (item.tracks ?? []).forEach((track, index) => {
          playlistItems.push({
            $type: "sh.diffuse.output.playlistItem",
            id: TID.now(),
            playlist: playlistName,
            positionedAfter: isUnordered
              ? undefined
              : index > 0
              ? playlistItems[index - 1].id
              : undefined,
            criteria: [
              {
                field: "tags.album",
                value: track.album ?? "",
                transformations: ["toLowerCase"],
              },
              {
                field: "tags.artist",
                value: track.artist ?? "",
                transformations: ["toLowerCase"],
              },
              {
                field: "tags.title",
                value: track.title ?? "",
                transformations: ["toLowerCase"],
              },
            ],
            createdAt: now,
            updatedAt: now,
          });
        });

        return playlistItems;
      });

    await output.playlistItems.save([...existing, ...newPlaylistItems]);
    const playlistCount = new Set(newPlaylistItems.map((p) => p.playlist)).size;
    showStatus(`Imported ${playlistCount} playlist(s).`, "success");
  } catch (err) {
    console.error("Import failed:", err);
    showStatus(`Import failed: ${/** @type {Error} */ (err).message}`, "error");
  }
};
