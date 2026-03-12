import foundation from "~/common/facets/foundation.js";
import { effect } from "~/common/signal.js";
import * as Playlist from "~/common/playlist.js";

const ACTIVE_CLASS = "button--active";

// Setup
await foundation.features.fillQueueAutomatically();
await foundation.features.processInputs();

const [repeatShuffle, scope, output] = await Promise.all([
  foundation.engine.repeatShuffle(),
  foundation.engine.scope(),
  foundation.orchestrator.output(),
]);

// Elements
const repeatBtn =
  /** @type {HTMLButtonElement} */ (document.querySelector("#repeat"));
const shuffleBtn =
  /** @type {HTMLButtonElement} */ (document.querySelector("#shuffle"));
const searchInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#search"));
const playlistSelect =
  /** @type {HTMLSelectElement} */ (document.querySelector("#playlist"));
const sortBySelect =
  /** @type {HTMLSelectElement} */ (document.querySelector("#sort-by"));
const sortDirectionBtn =
  /** @type {HTMLButtonElement} */ (document.querySelector("#sort-direction"));

// Repeat & Shuffle state
effect(() => {
  repeatBtn.classList.toggle(ACTIVE_CLASS, repeatShuffle.repeat());
});

effect(() => {
  shuffleBtn.classList.toggle(ACTIVE_CLASS, repeatShuffle.shuffle());
});

// Actions
repeatBtn.onclick = () => {
  repeatShuffle.setRepeat(!repeatShuffle.repeat());
};

shuffleBtn.onclick = () => {
  repeatShuffle.setShuffle(!repeatShuffle.shuffle());
};

// Search state
effect(() => {
  searchInput.value = scope.searchTerm() ?? "";
});

searchInput.oninput = () => {
  scope.setSearchTerm(searchInput.value.trim() || undefined);
};

// Playlist state
effect(() => {
  const col = output.playlistItems.collection();
  const items = col.state === "loaded" ? col.data : [];
  const currentPlaylist = scope.playlist();

  // Group items by playlist name
  const playlistMap = Playlist.gather(items);
  const all = [...playlistMap.values()].sort((a, b) =>
    a.name.localeCompare(b.name)
  );

  const ordered = all.filter((p) => !p.unordered);
  const unordered = all.filter((p) => p.unordered);

  playlistSelect.innerHTML = `<option value="">All tracks</option>`;

  for (
    const [label, group] of [
      ["Ordered", ordered],
      ["Unordered", unordered],
    ]
  ) {
    if (group.length === 0) continue;

    const optgroup = document.createElement("optgroup");
    optgroup.label = /** @type {string} */ (label);

    for (const playlist of group) {
      if (typeof playlist === "string") continue;
      const option = document.createElement("option");

      option.value = playlist.name;
      option.textContent = playlist.name;
      option.selected = playlist.name === currentPlaylist;

      optgroup.appendChild(option);
    }

    playlistSelect.appendChild(optgroup);
  }
});

playlistSelect.onchange = () => {
  scope.setPlaylist(
    playlistSelect.value.length ? playlistSelect.value : undefined,
  );
};

// Sort by state
effect(() => {
  const current = JSON.stringify(scope.sortBy());
  for (const option of sortBySelect.options) {
    if (JSON.stringify(JSON.parse(option.value)) === current) {
      sortBySelect.value = option.value;
      break;
    }
  }
});

sortBySelect.onchange = () => {
  scope.setSortBy(JSON.parse(sortBySelect.value));
};

// Sort direction state
effect(() => {
  const dir = scope.sortDirection() ?? "desc";
  sortDirectionBtn.textContent = dir.toUpperCase();
});

sortDirectionBtn.onclick = () => {
  const dir = scope.sortDirection() ?? "desc";
  scope.setSortDirection(dir === "asc" ? "desc" : "asc");
};
