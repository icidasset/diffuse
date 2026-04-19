import {
  BroadcastableDiffuseElement,
  defineElement,
  query,
  queryOptional,
} from "~/common/element.js";
import { batch, computed, signal } from "~/common/signal.js";
import { filterByPlaylist } from "~/common/playlist.js";
import { safeDecodeURIComponent } from "~/common/utils.js";
import { listen } from "~/common/worker.js";

/**
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {InputElement} from "~/components/input/types.d.ts"
 * @import {OutputElement} from "~/components/output/types.d.ts"
 * @import {Actions, State} from "./types.d.ts"
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
  #tracksSearch = signal(/** @type {Track[]} */ ([]));
  #tracksFinal = signal(/** @type {Track[]} */ ([]));

  #tracksGrouped = computed(() => {
    const tracks = this.#tracksFinal.value;
    const groupBy = this.#scope.value?.groupBy();
    if (!groupBy) return undefined;
    return buildGroups(tracks, groupBy);
  });

  // STATE

  supplyFingerprint = this.#supplyFingerprint.get;
  tracks = this.#tracksFinal.get;
  groups = this.#tracksGrouped;

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
        if (value.available === false) return;
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

    // Watch search supply
    this.effect(async () => {
      const _trigger = this.#supplyFingerprint.value;
      const availableTracks = this.#tracksAvailable.value;
      const searchTerm = this.#scope.value?.searchTerm();

      if ((await this.isLeader()) === false) return;

      if (searchTerm?.length) {
        const searchResults = await this.#proxy.search({
          term: searchTerm,
        });
        this.#tracksSearch.set(searchResults);
      } else {
        this.#tracksSearch.set(availableTracks);
      }
    });

    // Watch `#tracksSearch` + Playlist + Sort
    this.effect(async () => {
      const tracks = this.#tracksSearch.value;
      const playlistItems = this.#selectedPlaylistItems();
      const disabledSources = this.#disabledSources();
      const sortBy = this.#scope.value?.sortBy();
      const sortDirection = this.#scope.value?.sortDirection();
      const groupBy = this.#scope.value?.groupBy();

      if ((await this.isLeader()) === false) return;

      let final = playlistItems?.length
        ? filterByPlaylist(tracks, playlistItems)
        : tracks;

      if (disabledSources.length) {
        final = final.filter((t) =>
          !disabledSources.some((source) => t.uri.startsWith(source))
        );
      }

      // When groupBy is active, sort by group key first using the group's
      // canonical direction (from GROUP_BY_SORT_OVERRIDES, or user's direction
      // for firstLetter). Within each group, sort by the user's sortBy and
      // sortDirection as normal.
      //
      // Schwartzian transform: precompute all keys once (O(N)) so the
      // comparator never re-parses URLs or re-splits dot-paths (O(N log N)).
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
            return a.groupKey.localeCompare(b.groupKey) * groupDir;
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

      this.#tracksFinal.set(final);
    });
  }
}

export default ScopedTracksOrchestrator;

////////////////////////////////////////////
// HELPERS
////////////////////////////////////////////

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
 * @param {Track[]} tracks
 * @param {string} groupBy  dot-path field, e.g. "createdAt" or "tags.artist"
 * @returns {{ label: string; tracks: Track[] }[]}
 */
function buildGroups(tracks, groupBy) {
  /** @type {{ label: string; tracks: Track[] }[]} */
  const groups = [];
  let lastKey = /** @type {string | undefined} */ (undefined);
  let current =
    /** @type {{ label: string; tracks: Track[] } | undefined} */ (undefined);

  for (const track of tracks) {
    const { key, label } = groupKeyLabel(track, groupBy);

    if (key !== lastKey) {
      current = { label, tracks: [] };
      groups.push(current);
      lastKey = key;
    }

    current?.tracks.push(track);
  }

  return groups;
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
