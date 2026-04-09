import { html, render as litRender } from "lit-html";

import * as Output from "~/common/output.js";
import * as Playlist from "~/common/playlist.js";
import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

/**
 * @import { PlaylistItem, Track } from "~/definitions/types.d.ts"
 */

foundation.setup({ title: "Playlists | Diffuse" });

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const outputOrchestrator = await foundation.orchestrator.output();

await customElements.whenDefined(outputOrchestrator.localName);

////////////////////////////////////////////
// UI
////////////////////////////////////////////

const list =
  /** @type {HTMLElement} */ (document.querySelector("#playlists-list"));
const empty =
  /** @type {HTMLElement} */ (document.querySelector("#playlists-empty"));
const dialog =
  /** @type {HTMLDialogElement} */ (document.querySelector("#playlists-dialog"));

effect(() => {
  const playlistItemsCol = outputOrchestrator.playlistItems.collection();
  const playlistItems =
    playlistItemsCol.state === "loaded" ? playlistItemsCol.data : [];

  const tracksCol = outputOrchestrator.tracks.collection();
  const tracks = tracksCol.state === "loaded" ? tracksCol.data : [];

  const playlists = [...Playlist.gather(playlistItems).values()]
    .sort((a, b) => a.name.localeCompare(b.name));
  const stats = computeStats(tracks, playlists);

  list.hidden = playlists.length === 0;
  empty.hidden = playlists.length > 0;

  litRender(
    html`
      ${playlists.map(({ name, items }, index) => {
        const { matchedCount, missingCount } = stats.get(name) ??
          { matchedCount: 0, missingCount: 0 };
        const menuId = `playlists-menu-${index}`;
        return html`
          <li class="playlists-item">
            <div class="playlists-item__info">
              <span class="playlists-item__name">${name}</span>
              <span class="playlists-item__detail">${matchedCount} found · ${missingCount} not found</span>
            </div>
            <button
              class="button--plain button--icon"
              aria-label="More"
              popovertarget="${menuId}"
            >
              <i class="ph-fill ph-dots-three-outline-vertical"></i>
            </button>
            <div id="${menuId}" class="dropdown" popover>
              <button
                @click="${(/** @type {MouseEvent} */ e) => {
                  /** @type {HTMLElement | null} */ (/** @type {HTMLElement} */ (e.currentTarget).closest("[popover]"))?.hidePopover();
                  showDetails(name, tracks, items);
                }}"
              >
                <i class="ph-fill ph-binoculars"></i>
                View tracks
              </button>
              <button
                @click="${(/** @type {MouseEvent} */ e) => {
                  /** @type {HTMLElement | null} */ (/** @type {HTMLElement} */ (e.currentTarget).closest("[popover]"))?.hidePopover();
                  removeDuplicates(name, items);
                }}"
              >
                <i class="ph-fill ph-copy"></i>
                Remove duplicates
              </button>
              <button
                @click="${(/** @type {MouseEvent} */ e) => {
                  /** @type {HTMLElement | null} */ (/** @type {HTMLElement} */ (e.currentTarget).closest("[popover]"))?.hidePopover();
                  removePlaylist(name);
                }}"
              >
                <i class="ph-fill ph-skull"></i>
                Delete
              </button>
            </div>
          </li>
        `;
      })}
    `,
    list,
  );
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/** @param {string} name */
async function removePlaylist(name) {
  const playlistItems = await Output.data(outputOrchestrator.playlistItems);
  await outputOrchestrator.playlistItems.save(
    playlistItems.filter((item) => item.playlist !== name),
  );
}

/**
 * @param {string} name
 * @param {Track[]} tracks
 * @param {PlaylistItem[]} items
 */
/**
 * @param {string} name
 * @param {PlaylistItem[]} items
 */
async function removeDuplicates(name, items) {
  const seen = new Set();
  const duplicateIds = new Set();

  for (const item of items) {
    const key = item.criteria
      .map((c) => `${c.field}\0${String(c.value)}`)
      .join("\0\0");
    if (seen.has(key)) {
      duplicateIds.add(item.id);
    } else {
      seen.add(key);
    }
  }

  if (duplicateIds.size === 0) return;

  const allItems = await Output.data(outputOrchestrator.playlistItems);
  await outputOrchestrator.playlistItems.save(
    allItems.filter((item) => !duplicateIds.has(item.id)),
  );
}

/**
 * @param {string} name
 * @param {Track[]} tracks
 * @param {PlaylistItem[]} items
 */
function showDetails(name, tracks, items) {
  const seenIds = new Set();
  const found = items
    .map((item) => tracks.find((t) => Playlist.match(t, item)))
    .filter((t) => t != null && !seenIds.has(t.id) && seenIds.add(t.id))
    .sort((a, b) => (a.tags?.title ?? "").localeCompare(b.tags?.title ?? ""));

  const notFound = items
    .filter((item) => !tracks.some((t) => Playlist.match(t, item)))
    .map((item) => ({
      title: String(item.criteria.find((c) => c.field === "tags.title")?.value ?? ""),
      artist: String(item.criteria.find((c) => c.field === "tags.artist")?.value ?? "") || null,
      album: String(item.criteria.find((c) => c.field === "tags.album")?.value ?? "") || null,
    }))
    .sort((a, b) => a.title.localeCompare(b.title));

  litRender(
    html`
      <div class="dialog-header">
        <strong>${name}</strong>
        <button
          class="button--plain button--icon"
          @click="${() => dialog.close()}"
        >
          <i class="ph-fill ph-x"></i>
        </button>
      </div>
      <div class="dialog-body">
        <div class="tracks-section">
          <span class="tracks-section__heading">${found.length} found</span>
          <ul class="tracks-list">
            ${found.map((t) => html`
              <li>
                <span class="tracks-list__name">${t.tags?.title ?? t.uri}</span>
                ${t.tags?.artist
                  ? html`<span class="tracks-list__artist">${t.tags.artist}</span>`
                  : null}
              </li>
            `)}
          </ul>
        </div>
        ${notFound.length > 0 ? html`
          <div class="tracks-section">
            <span class="tracks-section__heading">${notFound.length} not found</span>
            <ul class="tracks-list">
              ${notFound.map(({ title, artist, album }) => html`
                <li>
                  <span class="tracks-list__name">${title}</span>
                  ${artist ? html`<span class="tracks-list__artist">${artist}${album ? html` · ${album}` : null}</span>` : null}
                </li>
              `)}
            </ul>
          </div>
        ` : null}
      </div>
    `,
    dialog,
  );

  dialog.showModal();
}

////////////////////////////////////////////
// STATS
////////////////////////////////////////////

/**
 * Compute matched/missing counts for all playlists in a single pass over tracks.
 * O(tracks × playlists + items_total) instead of O(tracks × items × playlists).
 *
 * @param {Track[]} tracks
 * @param {Array<{ name: string, items: PlaylistItem[] }>} playlists
 * @returns {Map<string, { matchedCount: number, missingCount: number }>}
 */
function computeStats(tracks, playlists) {
  // Build a shape index per playlist.
  const indexes = playlists.map(({ name, items }) => {
    /** @type {Map<string, { fields: { parts: string[], transformations: string[] | undefined }[], trackKeys: Set<string>, itemKeys: Set<string> }>} */
    const shapeMap = new Map();

    for (const item of items) {
      const shapeKey = item.criteria
        .map((c) => `${c.field}\0${(c.transformations ?? []).join(",")}`)
        .join("\0\0");

      if (!shapeMap.has(shapeKey)) {
        shapeMap.set(shapeKey, {
          fields: item.criteria.map((c) => ({
            parts: c.field.split("."),
            transformations: /** @type {string[] | undefined} */ (c.transformations),
          })),
          trackKeys: new Set(),
          itemKeys: new Set(),
        });
      }

      const shape = shapeMap.get(shapeKey);
      const itemKey = item.criteria
        .map((c) => Playlist.transform(c.value, c.transformations))
        .join("\0");
      shape?.itemKeys.add(itemKey);
    }

    return { name, shapeMap, shapes: [...shapeMap.values()], items };
  });

  // Single pass over tracks — update all playlist indexes at once.
  const matchedCounts = new Map(playlists.map((p) => [p.name, 0]));
  for (const track of tracks) {
    for (const { name, shapes } of indexes) {
      let trackMatched = false;
      for (const shape of shapes) {
        const trackKey = shape.fields
          .map(({ parts, transformations }) =>
            Playlist.transform(
              parts.reduce((v, f) => /** @type {any} */ (v)?.[f], /** @type {any} */ (track)),
              transformations,
            )
          )
          .join("\0");
        if (shape.itemKeys.has(trackKey)) {
          shape.trackKeys.add(trackKey);
          trackMatched = true;
        }
      }
      if (trackMatched) matchedCounts.set(name, (matchedCounts.get(name) ?? 0) + 1);
    }
  }

  // Derive missing counts from the now-populated trackKeys sets.
  const result = new Map();
  for (const { name, shapeMap, items } of indexes) {
    let missingCount = 0;
    for (const item of items) {
      const shapeKey = item.criteria
        .map((c) => `${c.field}\0${(c.transformations ?? []).join(",")}`)
        .join("\0\0");
      const itemKey = item.criteria
        .map((c) => Playlist.transform(c.value, c.transformations))
        .join("\0");
      if (!shapeMap.get(shapeKey)?.trackKeys.has(itemKey)) missingCount++;
    }
    result.set(name, { matchedCount: matchedCounts.get(name) ?? 0, missingCount });
  }

  return result;
}


////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
