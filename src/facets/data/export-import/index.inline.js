import * as Output from "~/common/output.js";
import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

// Setup
const output = await foundation.orchestrator.output();

// Elements
const exportBtn =
  /** @type {HTMLButtonElement} */ (document.querySelector("#export"));
const fileInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#file"));
const importTracksBtn =
  /** @type {HTMLButtonElement} */ (document.querySelector("#import-tracks"));
const importPlaylistItemsBtn =
  /** @type {HTMLButtonElement} */ (document.querySelector(
    "#import-playlist-items",
  ));
const importFacetsBtn =
  /** @type {HTMLButtonElement} */ (document.querySelector("#import-facets"));
const importThemesBtn =
  /** @type {HTMLButtonElement} */ (document.querySelector("#import-themes"));
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

// Enable export button once output is ready
effect(() => {
  exportBtn.disabled = !output.ready();
});

// Export all data as a JSON snapshot
exportBtn.onclick = async () => {
  const facets = await Output.data(output.facets);
  const playlistItems = await Output.data(output.playlistItems);
  const themes = await Output.data(output.themes);
  const tracks = await Output.data(output.tracks);

  const data = {
    exportedAt: new Date().toISOString(),
    facets,
    playlistItems,
    themes,
    tracks,
  };

  const blob = new Blob([JSON.stringify(data, null, 2)], {
    type: "application/json",
  });

  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = `diffuse-${new Date().toISOString().slice(0, 10)}.json`;
  document.body.append(a);
  a.click();
  a.remove();

  URL.revokeObjectURL(url);
};

// Parse file on selection
fileInput.onchange = async () => {
  const file = fileInput.files?.[0];

  json = null;
  statusEl.hidden = true;
  importTracksBtn.disabled = true;
  importPlaylistItemsBtn.disabled = true;
  importFacetsBtn.disabled = true;
  importThemesBtn.disabled = true;

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

  if (Array.isArray(json?.tracks) && json.tracks.length > 0) {
    importTracksBtn.disabled = false;
  }

  if (Array.isArray(json?.playlistItems) && json.playlistItems.length > 0) {
    importPlaylistItemsBtn.disabled = false;
  }

  if (Array.isArray(json?.facets) && json.facets.length > 0) {
    importFacetsBtn.disabled = false;
  }

  if (Array.isArray(json?.themes) && json.themes.length > 0) {
    importThemesBtn.disabled = false;
  }
};

// Import tracks
importTracksBtn.onclick = async () => {
  /** @type {any[]} */
  const tracks = json?.tracks;
  if (!Array.isArray(tracks) || tracks.length === 0) return;

  try {
    await output.tracks.save(tracks);
    showStatus(`Imported ${tracks.length} track(s).`, "success");
  } catch (err) {
    console.error("Import failed:", err);
    showStatus(`Import failed: ${/** @type {Error} */ (err).message}`, "error");
  }
};

// Import playlist items
importPlaylistItemsBtn.onclick = async () => {
  /** @type {any[]} */
  const playlistItems = json?.playlistItems;
  if (!Array.isArray(playlistItems) || playlistItems.length === 0) return;

  try {
    await output.playlistItems.save(playlistItems);
    const playlistCount = new Set(playlistItems.map((p) => p.playlist)).size;
    showStatus(`Imported ${playlistCount} playlist(s).`, "success");
  } catch (err) {
    console.error("Import failed:", err);
    showStatus(`Import failed: ${/** @type {Error} */ (err).message}`, "error");
  }
};

// Import facets
importFacetsBtn.onclick = async () => {
  /** @type {any[]} */
  const facets = json?.facets;
  if (!Array.isArray(facets) || facets.length === 0) return;

  try {
    await output.facets.save(facets);
    showStatus(`Imported ${facets.length} facet(s).`, "success");
  } catch (err) {
    console.error("Import failed:", err);
    showStatus(`Import failed: ${/** @type {Error} */ (err).message}`, "error");
  }
};

// Import themes
importThemesBtn.onclick = async () => {
  /** @type {any[]} */
  const themes = json?.themes;
  if (!Array.isArray(themes) || themes.length === 0) return;

  try {
    await output.themes.save(themes);
    showStatus(`Imported ${themes.length} theme(s).`, "success");
  } catch (err) {
    console.error("Import failed:", err);
    showStatus(`Import failed: ${/** @type {Error} */ (err).message}`, "error");
  }
};
