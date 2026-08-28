import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";
import { CommandCore } from "~/vendor/kmenu-core/index.js";
import * as Playlist from "~/common/playlist.js";

foundation.setup({ title: "Command Menu | Diffuse" });

// ---------------------------------------------------------------------------
// Foundation setup
// ---------------------------------------------------------------------------

const [queue, repeatShuffle, output, scope, favourites] = await Promise.all([
  foundation.engine.queue(),
  foundation.engine.repeatShuffle(),
  foundation.orchestrator.output(),
  foundation.engine.scope(),
  foundation.orchestrator.favourites(),
]);

// ---------------------------------------------------------------------------
// DOM elements
// ---------------------------------------------------------------------------

const searchInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#search"));
const optionsList =
  /** @type {HTMLUListElement} */ (document.querySelector("#kmenu-listbox"));
const breadcrumbsEl =
  /** @type {HTMLElement} */ (document.querySelector("#breadcrumbs"));

// ---------------------------------------------------------------------------
// Command menu instance
// ---------------------------------------------------------------------------

const menu = new CommandCore({
  // At root level, the package searches allOptions (full flattened tree) which
  // includes submenu children. Restrict to root-only items by filtering out
  // anything with a parent (i.e. options that belong to a submenu).
  filter: (options, query) => {
    if (!query) return options;
    const rootOnly = options.filter((/** @type {any} */ o) => !o.parent);
    const q = query.toLowerCase();
    return rootOnly.filter(
      (/** @type {any} */ o) =>
        !o.disabled &&
        (o.label.toLowerCase().includes(q) ||
          o.keywords?.some((/** @type {string} */ k) =>
            k.toLowerCase().includes(q)
          )),
    );
  },
});

// Register DOM elements with CommandCore (for scroll management and focus)
menu.getListboxProps().ref(optionsList);
menu.getInputProps().ref(searchInput);

// Wire up input events
searchInput.oninput = (/** @type {Event} */ e) =>
  menu.setInput(/** @type {HTMLInputElement} */ (e.target).value);
searchInput.onkeydown = (/** @type {KeyboardEvent} */ e) =>
  menu.getInputProps().onKeyDown(e);

// The package's Escape handler always calls close(), never goBack(). Intercept
// at the window level so it works regardless of which element has focus.
window.addEventListener("keydown", (/** @type {KeyboardEvent} */ e) => {
  if (e.key === "Escape" && menu.getState().breadcrumbs.length > 0) {
    e.preventDefault();
    e.stopImmediatePropagation();
    menu.goBack();
  }
});

// ---------------------------------------------------------------------------
// Render
// ---------------------------------------------------------------------------

function render() {
  const state = menu.getState();

  // Sync input value without disturbing cursor position when focused
  if (document.activeElement !== searchInput) {
    searchInput.value = state.input;
  }

  // Breadcrumbs
  breadcrumbsEl.innerHTML = "";

  if (state.breadcrumbs.length > 0) {
    const home = document.createElement("button");
    home.className = "crumb-home";
    home.textContent = "Home";
    home.onclick = () => {
      while (menu.getState().breadcrumbs.length > 0) menu.goBack();
    };
    breadcrumbsEl.appendChild(home);

    for (const crumb of state.breadcrumbs) {
      const span = document.createElement("span");
      span.textContent = crumb.label;
      breadcrumbsEl.appendChild(span);
    }
  }

  // Options list
  optionsList.innerHTML = "";

  if (state.filtered.length === 0) {
    const li = document.createElement("li");
    li.className = "empty-message";
    li.textContent = "No commands found";
    optionsList.appendChild(li);
    return;
  }

  let currentGroup = null;

  for (const option of state.filtered) {
    // Group header
    if (option.group && option.group !== currentGroup) {
      currentGroup = option.group;
      const groupLi = document.createElement("li");
      groupLi.className = "group-label";
      groupLi.setAttribute("role", "presentation");
      groupLi.textContent = option.group;
      optionsList.appendChild(groupLi);
    }

    const li = document.createElement("li");
    li.className = "option";

    const props = menu.getOptionProps(/** @type {string} */ (option.id));
    props.ref(li); // Register with CommandCore for scroll management

    li.setAttribute("id", `kmenu-option-${option.id}`);
    li.setAttribute("role", "option");
    li.setAttribute(
      "aria-selected",
      state.activeId === option.id ? "true" : "false",
    );
    if (option.disabled) li.setAttribute("aria-disabled", "true");

    const label = document.createElement("span");
    label.className = "option-label";
    label.textContent = option.label;
    li.appendChild(label);

    const hasChildren = option.children && option.children.length > 0;
    if (hasChildren) {
      const chevron = document.createElement("i");
      chevron.className = "ph-bold ph-caret-right option-chevron";
      chevron.setAttribute("aria-hidden", "true");
      li.appendChild(chevron);
    }

    li.onclick = props.onClick;
    li.onmouseenter = props.onMouseEnter;

    optionsList.appendChild(li);
  }
}

// Full re-render only for structural changes (list content changes)
for (const event of ["open", "change", "submenu", "back"]) {
  menu.on(/** @type {any} */ (event), render);
}

// Navigation: update only the active highlight without rebuilding the DOM
menu.on("navigate", (/** @type {any} */ e) => {
  for (const li of optionsList.querySelectorAll("[role=option]")) {
    li.setAttribute(
      "aria-selected",
      li.id === `kmenu-option-${e.activeId}` ? "true" : "false",
    );
  }
  if (e.activeId) {
    searchInput.setAttribute(
      "aria-activedescendant",
      `kmenu-option-${e.activeId}`,
    );
  } else {
    searchInput.removeAttribute("aria-activedescendant");
  }
});

// Re-open the menu immediately on close to keep it always visible
menu.on("close", () => menu.open());

// ---------------------------------------------------------------------------
// Commands — registered reactively via effect()
// ---------------------------------------------------------------------------

effect(() => {
  const nowItem = queue.now();
  const isRepeat = repeatShuffle.repeat();
  const isShuffle = repeatShuffle.shuffle();
  const currentPlaylist = scope.playlist();

  const tracksCol = output.tracks.collection();
  const now = nowItem && tracksCol.state === "loaded"
    ? tracksCol.data.find((t) => t.id === nowItem.id) ?? null
    : null;

  const col = output.playlistItems.collection();
  const items = col.state === "loaded" ? col.data : [];

  const playlistMap = Playlist.gather(items);
  const playlists = [...playlistMap.values()].sort((a, b) =>
    a.name.localeCompare(b.name)
  );

  const isFav = now && favourites ? favourites.isFavourite(now) : false;
  const nowLabel = now
    ? (now.tags?.artist
      ? `${now.tags.artist} - ${now.tags.title}`
      : "current track")
    : null;

  menu.registerOptions([
    // ------------------------------------------------------------------
    // Playback
    // ------------------------------------------------------------------
    {
      id: "favourite-toggle",
      label: nowLabel
        ? isFav
          ? `Remove "${nowLabel}" from favourites`
          : `Add "${nowLabel}" to favourites`
        : "Add now playing to favourites",
      keywords: ["favourite", "favorite", "like", "heart", "star"],
      group: "Playback",
      disabled: !now,
      action: () => {
        const item = queue.now();
        if (!item) return;
        const tc = output.tracks.collection();
        const track = tc.state === "loaded"
          ? tc.data.find((t) => t.id === item.id)
          : undefined;
        if (track) favourites.toggle(track);
      },
    },
    {
      id: "toggle-repeat",
      label: isRepeat ? "Disable repeat" : "Enable repeat",
      keywords: ["repeat", "loop"],
      group: "Playback",
      action: () => {
        repeatShuffle.setRepeat(!repeatShuffle.repeat());
      },
    },
    {
      id: "toggle-shuffle",
      label: isShuffle ? "Disable shuffle" : "Enable shuffle",
      keywords: ["shuffle", "random"],
      group: "Playback",
      action: () => {
        repeatShuffle.setShuffle(!repeatShuffle.shuffle());
      },
    },

    // ------------------------------------------------------------------
    // Playlists
    // ------------------------------------------------------------------
    {
      id: "select-playlist",
      label: currentPlaylist
        ? `Playlist: ${currentPlaylist}`
        : "Select playlist",
      keywords: ["playlist", "filter", "browse", "queue"],
      group: "Playlists",
      children: [
        {
          id: "playlist-all",
          label: "All tracks",
          keywords: ["all", "everything", "reset"],
          action: () => {
            scope.setPlaylist(undefined);
          },
        },
        ...playlists.map((p) => ({
          id: `playlist-select-${p.name}`,
          label: p.name,
          action: () => {
            scope.setPlaylist(p.name);
          },
        })),
      ],
    },
    {
      id: "create-playlist",
      label: nowLabel
        ? `Create playlist with "${nowLabel}"`
        : "Create playlist",
      keywords: ["new", "add", "create", "playlist"],
      group: "Playlists",
      disabled: !now,
      action: () => {
        const item = queue.now();
        if (!item) return;
        const tc = output.tracks.collection();
        const track = tc.state === "loaded"
          ? tc.data.find((t) => t.id === item.id)
          : undefined;
        if (!track) return;

        const name = prompt("New playlist name:");
        if (!name?.trim()) return;

        const col = output.playlistItems.collection();
        const existing = col.state === "loaded" ? col.data : [];
        const ts = new Date().toISOString();

        output.playlistItems.save([
          ...existing,
          {
            $type: "sh.diffuse.output.playlistItem",
            id: crypto.randomUUID(),
            playlist: name.trim(),
            criteria: [
              {
                field: "tags.artist",
                value: /** @type {Record<string, unknown>} */ (/** @type {unknown} */ (track.tags?.artist ?? "")),
                transformations: ["toLowerCase"],
              },
              {
                field: "tags.title",
                value: /** @type {Record<string, unknown>} */ (/** @type {unknown} */ (track.tags?.title ?? "")),
                transformations: ["toLowerCase"],
              },
            ],
            createdAt: ts,
            updatedAt: ts,
          },
        ]);
      },
    },
    {
      id: "remove-playlist",
      label: "Remove playlist",
      keywords: ["delete", "remove", "playlist"],
      group: "Playlists",
      disabled: playlists.length === 0,
      children: playlists.map((p) => ({
        id: `playlist-remove-${p.name}`,
        label: p.name,
        children: [
          {
            id: `playlist-remove-${p.name}-confirm`,
            label: `Confirm: remove "${p.name}"`,
            keywords: ["yes", "confirm", "delete"],
            action: () => {
              const col = output.playlistItems.collection();
              const existing = col.state === "loaded" ? col.data : [];
              output.playlistItems.save(
                existing.filter((item) => item.playlist !== p.name),
              );
            },
          },
        ],
      })),
    },
  ]);

  // Re-render after options change so labels (repeat/shuffle/now-playing)
  // stay in sync without relying on the navigate event.
  render();
});

// ---------------------------------------------------------------------------
// Open and signal ready
// ---------------------------------------------------------------------------

menu.open();

foundation.ready();
