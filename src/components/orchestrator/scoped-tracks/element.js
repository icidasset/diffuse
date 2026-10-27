import {
  BroadcastableDiffuseElement,
  defineElement,
  query,
  queryOptional,
} from "~/common/element.js";
import { batch, computed, signal } from "~/common/signal.js";
import {
  filterByPlaylist,
  gather,
  orderByPlaylist,
  transform,
} from "~/common/playlist.js";
import { safeDecodeURIComponent } from "~/common/utils.js";
import { listen } from "~/common/worker.js";

/**
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {InputElement} from "@specs/components/input/types.d.ts"
 * @import {OutputElement} from "@specs/components/output/types.d.ts"
 * @import {Actions, State, TrackPools} from "@specs/components/orchestrator/scoped-tracks/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class ScopedTracksOrchestrator extends BroadcastableDiffuseElement {
  static NAME = "diffuse/orchestrator/scoped-tracks";
  static WORKER_URL = "components/orchestrator/scoped-tracks/worker.js";

  /** @type {ProxiedActions<Actions & State>} */
  #proxy;

  constructor() {
    super();
    this.#proxy = this.workerProxy();
  }

  // SIGNALS

  #input = signal(/** @type {InputElement | null} */ (null));
  #output = signal(/** @type {OutputElement | null} */ (null));

  #scope = signal(
    /** @type {import("~/components/engine/scope/element.js").CLASS | null} */ (null),
  );

  #supplyFingerprint = signal(/** @type {string | undefined} */ (undefined));

  #selectedPlaylistItems = computed(() => {
    const playlist = this.#scope.value?.playlist();
    if (!playlist) return undefined;

    const col = this.#output.value?.playlistItems.collection();
    if (!col || col.state !== "loaded") return undefined;
    return col.data.filter((p) => p.playlist === playlist);
  });

  #disabledSources = computed(() => {
    const col = this.#output.value?.settings.collection();
    if (!col || col.state !== "loaded") return [];

    const setting = col.data.find((s) =>
      s.key === "sh.diffuse.input.disabled.uris"
    );

    if (!setting) return [];

    try {
      const parsed = JSON.parse(setting.value);
      return Array.isArray(parsed) ? /** @type {string[]} */ (parsed) : [];
    } catch {
      return [];
    }
  });

  #tracksAvailable = signal(/** @type {Track[]} */ ([]));

  /** `undefined` when no search is active; `[]` means an active search with no hits. */
  #tracksSearch = signal(/** @type {Track[] | undefined} */ (undefined));
  #tracksFinal = signal(/** @type {Track[]} */ ([]));

  /**
   * The full available pool minus disabled sources — the base both the
   * catalog and the scoped pipeline start from. Because it derives from
   * signals that are already replicated (`#tracksAvailable`) and shared
   * (`#disabledSources`), it doesn't need its own broadcast channel.
   */
  #tracksBase = computed(() => {
    const available = this.#tracksAvailable.value;
    const disabledSources = this.#disabledSources();

    return disabledSources.length
      ? available.filter((t) =>
        !disabledSources.some((source) => t.uri.startsWith(source))
      )
      : available;
  });

  #tracksGrouped = computed(() => {
    const tracks = this.#tracksFinal.value;
    const groupBy = this.#scope.value?.groupBy();
    if (!groupBy) return undefined;
    return buildGroups(tracks, groupBy);
  });

  // STATE

  supplyFingerprint = this.#supplyFingerprint.get;
  groups = this.#tracksGrouped;

  /** @returns {TrackPools} */
  tracks = computed(() => ({
    available: this.#tracksBase(),
    scoped: this.#tracksFinal.value,
  }));

  /**
   * Whether the currently selected playlist should be rendered in its own
   * order (i.e. its playlist items form a `positionedAfter` chain). When
   * this is true, themes should prefer it over `sortBy` for UI indicators
   * (e.g. don't highlight the "artist" column as the active sort). It is
   * independent of `groupBy`/`sortBy` so grouping may still be applied —
   * in that case tracks are grouped by the `groupBy` key while playlist
   * order is preserved within each group.
   */
  playlistIsOrdered = computed(() => {
    const playlistItems = this.#selectedPlaylistItems();
    const playlistName = this.#scope.value?.playlist();

    return playlistItems?.length && playlistName
      ? gather(playlistItems).get(playlistName)?.unordered === false
      : false;
  });

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(this.identifier, {
        getTracksAvailable: {
          strategy: "leaderOnly",
          fn: this.#tracksAvailable.get,
        },
        getTracksSearch: {
          strategy: "leaderOnly",
          fn: this.#tracksSearch.get,
        },
        getTracksFinal: {
          strategy: "leaderOnly",
          fn: this.#tracksFinal.get,
        },
        setTracksAvailable: {
          strategy: "replicate",
          fn: this.#tracksAvailable.set,
        },
        setTracksSearch: {
          strategy: "replicate",
          fn: this.#tracksSearch.set,
        },
        setTracksFinal: {
          strategy: "replicate",
          fn: this.#tracksFinal.set,
        },
      });

      if (!actions) return;

      this.#tracksAvailable.set = actions.setTracksAvailable;
      this.#tracksSearch.set = actions.setTracksSearch;
      this.#tracksFinal.set = actions.setTracksFinal;

      // Sync signal state with leader
      Promise.all([
        actions.getTracksAvailable(),
        actions.getTracksSearch(),
        actions.getTracksFinal(),
      ]).then(([available, search, final]) =>
        batch(() => {
          this.#tracksAvailable.value = available;
          this.#tracksSearch.value = search;
          this.#tracksFinal.value = final;
        })
      );
    }

    // Super
    super.connectedCallback();

    /** @type {InputElement} */
    const input = query(this, "input-selector");

    /** @type {OutputElement} */
    const output = query(this, "output-selector");

    /** @type {import("~/components/engine/scope/element.js").CLASS | null} */
    const scope = queryOptional(this, "scope-engine-selector");

    // Assign to self
    this.#input.value = input;
    this.#output.value = output;
    if (scope) this.#scope.value = scope;

    // Sync supply fingerprint with worker
    const link = this.workerLink();
    listen("supplyFingerprint", this.#supplyFingerprint.set, link);
    this.#proxy.supplyFingerprint().then(this.#supplyFingerprint.set);

    // When defined
    await customElements.whenDefined(input.localName);
    await customElements.whenDefined(output.localName);
    if (scope) await customElements.whenDefined(scope.localName);

    // Watch tracks collection
    this.effect(async () => {
      const tracksCol = output.tracks.collection();

      if ((await this.isLeader()) === false) return;
      if (tracksCol.state !== "loaded") return;

      /** @type {string[]} */
      const uris = [];
      const tracks = tracksCol.data.filter((t) => {
        uris.push(t.uri);
        return t.kind !== "placeholder";
      });

      // Consult inputs
      const groups = tracksCol.data.length
        ? await input.groupConsult(uris)
        : {};

      /** @type {Set<string>} */
      const availableUris = new Set();

      Object.values(groups).forEach((value) => {
        // Only include tracks whose source confirmed availability ("yes").
        // Both "no" (server explicitly rejected) and "unsure" (transient
        // consult failure — e.g. a laptop waking up before the network
        // is fully back) hide the source's tracks until a real consult
        // succeeds. `cachedConsult` normalises "unsure" to "no" for callers
        // (without caching it), so a brief blip doesn't pin availability
        // to "unavailable" for the full TTL — the next consult retries
        // immediately and repopulates the browser once the server responds.
        if (value.available !== "yes") return;
        for (const uri of value.uris) {
          availableUris.add(uri);
        }
      });

      const availableTracks = tracks.filter((t) => {
        return availableUris.has(t.uri) && !!t.tags;
      });

      // Set pool
      this.#proxy.supply({ tracks: availableTracks });
      this.#tracksAvailable.set(availableTracks);
    });

    // Watch the search term + supplied index
    let searchX = 0;

    this.effect(async () => {
      const _trigger = this.#supplyFingerprint.value;
      const searchTerm = this.#scope.value?.searchTerm();

      const y = ++searchX;

      if ((await this.isLeader()) === false) return;
      if (y !== searchX) return;

      if (searchTerm?.length) {
        const searchResults = await this.#proxy.search({
          term: searchTerm,
        });
        if (y !== searchX) return;
        this.#tracksSearch.set(searchResults);
      } else {
        this.#tracksSearch.set(undefined);
      }
    });

    // Watch `#tracksAvailable` + search + Playlist + Sort + Selection.
    //
    // Pipeline order (deliberate): the catalog is always the full pool;
    // the scoped list is sort → playlist → selection → search.
    let x = 0;

    this.effect(async () => {
      // Read every dependency before the await so reactivity is registered.
      // Disabled sources are a library-level preference, so `#tracksBase`
      // narrows both the catalog and the final list.
      const base = this.#tracksBase();
      const searchResults = this.#tracksSearch.value;
      const playlistItems = this.#selectedPlaylistItems();
      const sortBy = this.#scope.value?.sortBy();
      const sortDirection = this.#scope.value?.sortDirection();
      const groupBy = this.#scope.value?.groupBy();
      const selection = this.#scope.value?.selection();

      const y = ++x;

      if ((await this.isLeader()) === false) return;
      if (y !== x) return;

      // 1. Sort. When groupBy is active, sort by group key first using
      // the group's canonical direction (from GROUP_BY_SORT_OVERRIDES, or
      // the user's direction for firstLetter), then by the user's sortBy.
      //
      // Schwartzian transform: precompute all keys once (O(N)) so the
      // comparator never re-parses URLs or re-splits dot-paths (O(N log N)).
      let final = base;

      const groupOverride = groupBy
        ? GROUP_BY_SORT_OVERRIDES[groupBy]
        : undefined;
      const groupDir =
        (groupOverride?.sortDirection ?? sortDirection) === "desc" ? -1 : 1;
      const userFields = sortBy ?? [];
      const userDir = sortDirection === "desc" ? -1 : 1;
      const splitPaths = userFields.map((f) => f.split("."));

      if (groupBy || userFields.length) {
        const decorated = final.map((track) => ({
          track,
          groupKey: groupBy ? groupKeyLabel(track, groupBy).key : "",
          fieldVals: splitPaths.map((parts) => {
            let v = /** @type {any} */ (track);
            for (const p of parts) v = v?.[p];
            return v;
          }),
        }));

        decorated.sort((a, b) => {
          if (groupBy && a.groupKey !== b.groupKey) {
            if (!a.groupKey) return 1;
            if (!b.groupKey) return -1;
            return collator.compare(a.groupKey, b.groupKey) * groupDir;
          }
          for (let i = 0; i < a.fieldVals.length; i++) {
            const av = a.fieldVals[i];
            const bv = b.fieldVals[i];
            // Null/undefined always sorts last regardless of direction
            if (av == null && bv == null) continue;
            if (av == null) return 1;
            if (bv == null) return -1;
            const cmp = compareValues(av, bv);
            if (cmp !== 0) return cmp * userDir;
          }
          return 0;
        });

        final = decorated.map((d) => d.track);
      }

      // 2. Playlist. Filtering preserves the sorted order; an ordered
      // playlist then re-applies its own `positionedAfter` chain.
      if (playlistItems?.length) {
        final = filterByPlaylist(final, playlistItems);

        if (this.playlistIsOrdered()) {
          final = orderByPlaylist(
            final,
            /** @type {import("~/definitions/types.d.ts").PlaylistItem[]} */ (playlistItems),
          );
        }
      }

      // 3. Selection
      if (selection?.length) {
        final = final.filter((track) => inSelection(track, selection));
      }

      // 4. Search
      if (searchResults) {
        const ids = new Set(searchResults.map((t) => t.id));
        final = final.filter((t) => ids.has(t.id));
      }

      this.#tracksFinal.set(final);
    });
  }
}

export default ScopedTracksOrchestrator;

////////////////////////////////////////////
// HELPERS
////////////////////////////////////////////

const collator = new Intl.Collator();

const MONTHS = [
  "January",
  "February",
  "March",
  "April",
  "May",
  "June",
  "July",
  "August",
  "September",
  "October",
  "November",
  "December",
];

/** @type {Record<string, { sortDirection: "asc" | "desc" }>} */
const GROUP_BY_SORT_OVERRIDES = {
  createdAt: { sortDirection: "desc" },
  directory: { sortDirection: "asc" },
  firstLetter: { sortDirection: "asc" },
  "tags.year": { sortDirection: "desc" },
};

/**
 * Whether a track passes every criterion of the scope's selection filter.
 * Values are compared as strings for `eq`, so tag fields that are stored as
 * numbers (years) still match string criteria. Missing values compare as the
 * empty string, mirroring how album/artist groupings bucket untagged tracks.
 *
 * @param {Track} track
 * @param {import("~/components/engine/scope/element.js").SelectionCriterion[]} selection
 * @returns {boolean}
 */
function inSelection(track, selection) {
  for (const criterion of selection) {
    let value = /** @type {any} */ (track);
    for (const part of criterion.field.split(".")) value = value?.[part];

    const actual = transform(value, criterion.transformations);
    const expected = transform(criterion.value, criterion.transformations);

    switch (criterion.op ?? "eq") {
      case "prefix":
        if (!String(actual ?? "").startsWith(String(expected ?? ""))) {
          return false;
        }
        break;
      case "gte":
        if (!(Number(actual) >= Number(expected))) return false;
        break;
      case "lt":
        if (!(Number(actual) < Number(expected))) return false;
        break;
      default:
        if (String(actual ?? "") !== String(expected ?? "")) return false;
    }
  }

  return true;
}

/**
 * @param {Track[]} tracks
 * @param {string} groupBy  dot-path field, e.g. "createdAt" or "tags.artist"
 * @returns {{ label: string; tracks: Track[] }[]}
 */
function buildGroups(tracks, groupBy) {
  // Bucket by key rather than relying on consecutive runs: the scope list
  // is usually sorted by group, but an ordered playlist can reorder it
  // after the sort, and grouping should still produce one bucket per key.
  /** @type {Map<string, { label: string; tracks: Track[] }>} */
  const groups = new Map();

  for (const track of tracks) {
    const { key, label } = groupKeyLabel(track, groupBy);

    let group = groups.get(key);
    if (!group) {
      group = { label, tracks: [] };
      groups.set(key, group);
    }

    group.tracks.push(track);
  }

  return [...groups.values()];
}

/**
 * @param {Track} track
 * @param {string} fieldPath
 * @returns {{ key: string; label: string }}
 */
function groupKeyLabel(track, fieldPath) {
  if (fieldPath === "createdAt") {
    const iso = track.createdAt;
    if (!iso) return { key: "", label: "Unknown" };
    const year = iso.slice(0, 4);
    const month = iso.slice(5, 7);
    return {
      key: `${year}-${month}`,
      label: `${MONTHS[parseInt(month, 10) - 1]} ${year}`,
    };
  }

  if (fieldPath === "directory") {
    const uri = track.uri ?? "";
    let path = uri;
    try {
      path = new URL(uri).pathname;
    } catch {
      // not a valid URL, use as-is
    }
    const slash = path.lastIndexOf("/");
    const dir = slash > 0 ? path.slice(0, slash) : path;
    const key = uri.slice(0, uri.lastIndexOf("/"));
    return { key, label: safeDecodeURIComponent(dir) || "Unknown" };
  }

  if (fieldPath.startsWith("firstLetter:")) {
    const dotPath = fieldPath.slice("firstLetter:".length);
    let val = /** @type {any} */ (track);
    for (const key of dotPath.split(".")) val = val?.[key];
    const str = val != null ? String(val) : "";
    const letter = str.charAt(0).toUpperCase();
    const key = /[A-Z]/.test(letter) ? letter : "#";
    return { key, label: key };
  }

  // Generic dot-path extraction
  let val = /** @type {any} */ (track);
  for (const key of fieldPath.split(".")) val = val?.[key];
  const str = val != null ? String(val) : "";
  return { key: str, label: str || "Unknown" };
}

/**
 * @param {any} aVal
 * @param {any} bVal
 * @returns {number}
 */
function compareValues(aVal, bVal) {
  if (aVal == null && bVal == null) return 0;
  if (aVal == null) return 1;
  if (bVal == null) return -1;
  return typeof aVal === "string" && typeof bVal === "string"
    ? aVal.localeCompare(bVal)
    : aVal < bVal
    ? -1
    : aVal > bVal
    ? 1
    : 0;
}

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ScopedTracksOrchestrator;
export const NAME = "do-scoped-tracks";

defineElement(NAME, CLASS);
