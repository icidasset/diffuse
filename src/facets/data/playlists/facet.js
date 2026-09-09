import { html, render as litRender } from "lit-html";

import * as Output from "~/common/output.js";
import * as Playlist from "~/common/playlist.js";
import * as TID from "@atcute/tid";
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
  /** @type {HTMLDialogElement} */ (document.querySelector(
    "#playlists-dialog",
  ));
const createDialog =
  /** @type {HTMLDialogElement} */ (document.querySelector(
    "#create-playlist-dialog",
  ));

document.querySelector("#create-playlist-btn")?.addEventListener(
  "click",
  () => {
    showCreatePlaylist();
  },
);

effect(() => {
  const playlistItemsCol = outputOrchestrator.playlistItems.collection();
  const playlistItems = playlistItemsCol.state === "loaded"
    ? playlistItemsCol.data
    : [];

  const tracksCol = outputOrchestrator.tracks.collection();
  const tracks = tracksCol.state === "loaded" ? tracksCol.data : [];

  const playlists = [...Playlist.gather(playlistItems).values()]
    .sort((a, b) => a.name.localeCompare(b.name));

  const orderedPlaylists = playlists.filter((p) => !p.unordered);
  const unorderedPlaylists = playlists.filter((p) => p.unordered);

  list.hidden = playlists.length === 0;
  empty.hidden = playlists.length > 0;

  /** @param {typeof playlists} group @param {number} offset */
  const renderGroup = (group, offset) =>
    group.map(({ name, items }, index) => {
      const menuId = `playlists-menu-${offset + index}`;
      return html`
        <li class="playlists-item">
          <div class="playlists-item__info">
            <span class="playlists-item__name">${name}</span>
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
                /** @type {HTMLElement | null} */ (/** @type {HTMLElement} */ (e
                  .currentTarget).closest("[popover]"))?.hidePopover();
                showDetails(name, tracks, items);
              }}"
            >
              <i class="ph-fill ph-binoculars"></i>
              View tracks
            </button>
            <button
              @click="${(/** @type {MouseEvent} */ e) => {
                /** @type {HTMLElement | null} */ (/** @type {HTMLElement} */ (e
                  .currentTarget).closest("[popover]"))?.hidePopover();
                removeDuplicates(name, items);
              }}"
            >
              <i class="ph-fill ph-copy"></i>
              Remove duplicates
            </button>
            <button
              @click="${(/** @type {MouseEvent} */ e) => {
                /** @type {HTMLElement | null} */ (/** @type {HTMLElement} */ (e
                  .currentTarget).closest("[popover]"))?.hidePopover();
                removePlaylist(name);
              }}"
            >
              <i class="ph-fill ph-skull"></i>
              Delete
            </button>
          </div>
        </li>
      `;
    });

  litRender(
    html`
      ${orderedPlaylists.length > 0
        ? html`
          <li class="playlists-category">Ordered</li>
          ${renderGroup(orderedPlaylists, 0)}
        `
        : null} ${unorderedPlaylists.length > 0
        ? html`
          <li class="playlists-category">Not ordered</li>
          ${renderGroup(unorderedPlaylists, orderedPlaylists.length)}
        `
        : null}
    `,
    list,
  );
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/** @param {string} name */
async function removePlaylist(name) {
  if (!confirm(`Are you sure you want to remove "${name}"?`)) return;

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
  // Build per-shape track indexes so each item resolves in O(1) instead of O(tracks).
  /** @type {Map<string, { parts: string[][], transformations: (string[] | undefined)[], trackMap: Map<string, Track> }>} */
  const shapes = new Map();

  for (const item of items) {
    const shapeKey = item.criteria
      .map((c) => `${c.field}\0${(c.transformations ?? []).join(",")}`)
      .join("\0\0");
    if (!shapes.has(shapeKey)) {
      shapes.set(shapeKey, {
        parts: item.criteria.map((c) => c.field.split(".")),
        transformations: item.criteria.map((c) => c.transformations),
        trackMap: new Map(),
      });
    }
  }

  for (const track of tracks) {
    for (const shape of shapes.values()) {
      const key = shape.parts
        .map((parts, i) => {
          let v = /** @type {any} */ (track);
          for (const p of parts) v = v?.[p];
          return Playlist.transform(v, shape.transformations[i]);
        })
        .join("\0");
      if (!shape.trackMap.has(key)) shape.trackMap.set(key, track);
    }
  }

  /** @type {Track[]} */
  const found = [];
  /** @type {PlaylistItem[]} */
  const notFoundItems = [];
  const seenIds = new Set();

  for (const item of items) {
    const shapeKey = item.criteria
      .map((c) => `${c.field}\0${(c.transformations ?? []).join(",")}`)
      .join("\0\0");
    const itemKey = item.criteria
      .map((c) => Playlist.transform(c.value, c.transformations))
      .join("\0");
    const track = shapes.get(shapeKey)?.trackMap.get(itemKey);

    if (track && !seenIds.has(track.id)) {
      seenIds.add(track.id);
      found.push(track);
    } else if (!track) {
      notFoundItems.push(item);
    }
  }

  found.sort((a, b) =>
    (a.tags?.title ?? "").localeCompare(b.tags?.title ?? "")
  );

  const notFound = notFoundItems
    .map((item) => ({
      title: String(
        item.criteria.find((c) => c.field === "tags.title")?.value ?? "",
      ),
      artist:
        String(
          item.criteria.find((c) => c.field === "tags.artist")?.value ?? "",
        ) || null,
      album:
        String(
          item.criteria.find((c) => c.field === "tags.album")?.value ?? "",
        ) || null,
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
            ${found.map((t) =>
              html`
                <li>
                  <span class="tracks-list__name">${t.tags?.title ??
                    t.uri}</span>
                  ${t.tags?.artist
                    ? html`
                      <span class="tracks-list__artist">${t.tags.artist}</span>
                    `
                    : null}
                </li>
              `
            )}
          </ul>
        </div>
        ${notFound.length > 0
          ? html`
            <div class="tracks-section">
              <span class="tracks-section__heading">${notFound
                .length} not found</span>
              <ul class="tracks-list">
                ${notFound.map(({ title, artist, album }) =>
                  html`
                    <li>
                      <span class="tracks-list__name">${title}</span>
                      ${artist
                        ? html`
                          <span class="tracks-list__artist">${artist}${album
                            ? html`
                              · ${album}
                            `
                            : null}</span>
                        `
                        : null}
                    </li>
                  `
                )}
              </ul>
            </div>
          `
          : null}
      </div>
    `,
    dialog,
  );

  dialog.showModal();
}

function showCreatePlaylist() {
  litRender(
    html`
      <div class="dialog-header">
        <strong>Create playlist</strong>
        <button
          class="button--plain button--icon"
          @click="${() => createDialog.close()}"
        >
          <i class="ph-fill ph-x"></i>
        </button>
      </div>
      <div class="dialog-body">
        <form
          class="create-form"
          @submit="${async (/** @type {SubmitEvent} */ e) => {
            e.preventDefault();
            const form = /** @type {HTMLFormElement} */ (e.currentTarget);
            const name =
              /** @type {HTMLInputElement} */ (form.elements.namedItem("name"))
                ?.value.trim();
            if (!name) return;
            await createPlaylist(name);
            createDialog.close();
          }}"
        >
          <label>
            <span class="create-form__label">Name</span>
            <input
              name="name"
              type="text"
              placeholder="My playlist"
              autocomplete="off"
              required
              autofocus
            />
          </label>
          <p class="button-row">
            <button class="button button--accent" type="submit">
              <i class="ph-bold ph-plus"></i>
              Create playlist
            </button>
          </p>
        </form>
      </div>
    `,
    createDialog,
  );

  createDialog.showModal();
}

/** @param {string} name */
async function createPlaylist(name) {
  const existing = await Output.data(outputOrchestrator.playlistItems);

  const now = new Date().toISOString();

  /** @type {import("~/definitions/types.d.ts").PlaylistItem} */
  const item = {
    $type: "sh.diffuse.output.playlistItem",
    id: TID.now(),
    playlist: name,
    criteria: [],
    createdAt: now,
    updatedAt: now,
  };

  await outputOrchestrator.playlistItems.save([...existing, item]);
}

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
