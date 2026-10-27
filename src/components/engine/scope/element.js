import {
  BroadcastableDiffuseElement,
  defineElement,
} from "~/common/element.js";
import { batch, signal } from "~/common/signal.js";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * A criterion that narrows the track pool on top of the other scope
 * filters (playlist, search). This is what lets a browser reduce the
 * scope to the item currently selected in its own UI (album, artist,
 * year, ...) without having to invent a playlist for it.
 *
 * `eq` (the default) compares stringified values, `prefix` matches a
 * string prefix, and `gte` / `lt` compare numerically — the two range
 * operators together express things like a decade.
 *
 * @typedef {{
 *   field: string;
 *   value: string | number;
 *   op?: "eq" | "prefix" | "gte" | "lt";
 *   transformations?: string[];
 * }} SelectionCriterion
 */

class ScopeEngine extends BroadcastableDiffuseElement {
  static NAME = "diffuse/engine/scope";

  // SIGNALS

  #groupBy = signal(/** @type {string | undefined} */ (undefined));
  #playlist = signal(/** @type {string | undefined} */ (undefined));
  #searchTerm = signal(/** @type {string | undefined} */ (undefined));
  #selection = signal(
    /** @type {SelectionCriterion[] | undefined} */ (undefined),
  );
  #sortBy = signal(
    /** @type {string[]} */ ([
      "tags.artist",
      "tags.album",
      "tags.disc.no",
      "tags.track.no",
    ]),
  );
  #sortDirection = signal(/** @type {"asc" | "desc" | undefined} */ ("asc"));

  groupBy = this.#groupBy.get;
  playlist = this.#playlist.get;
  searchTerm = this.#searchTerm.get;
  selection = this.#selection.get;
  sortBy = this.#sortBy.get;
  sortDirection = this.#sortDirection.get;

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(this.identifier, {
        revertToDefaultSort: {
          strategy: "replicate",
          fn: this.revertToDefaultSort,
        },
        setGroupBy: { strategy: "replicate", fn: this.setGroupBy },
        setPlaylist: { strategy: "replicate", fn: this.setPlaylist },
        setSearchTerm: { strategy: "replicate", fn: this.setSearchTerm },
        setSelection: { strategy: "replicate", fn: this.setSelection },
        setSortBy: { strategy: "replicate", fn: this.setSortBy },
        setSortDirection: { strategy: "replicate", fn: this.setSortDirection },
      });

      if (actions) {
        this.revertToDefaultSort = actions.revertToDefaultSort;
        this.setGroupBy = actions.setGroupBy;
        this.setPlaylist = actions.setPlaylist;
        this.setSearchTerm = actions.setSearchTerm;
        this.setSelection = actions.setSelection;
        this.setSortBy = actions.setSortBy;
        this.setSortDirection = actions.setSortDirection;
      }
    }

    // Super
    super.connectedCallback();

    // Signals
    const storagePrefix =
      `${this.constructor.prototype.constructor.NAME}/${this.group}`;

    batch(() => {
      this.#groupBy.value = localStorage.getItem(`${storagePrefix}/groupBy`) ??
        undefined;
      this.#playlist.value =
        localStorage.getItem(`${storagePrefix}/playlistId`) ?? undefined;
      this.#searchTerm.value =
        localStorage.getItem(`${storagePrefix}/searchTerm`) ?? undefined;
      try {
        const rawSelection = localStorage.getItem(
          `${storagePrefix}/selection`,
        );
        this.#selection.value = rawSelection
          ? JSON.parse(rawSelection)
          : undefined;
      } catch {
        this.#selection.value = undefined;
      }
      this.#sortBy.value = JSON.parse(
        localStorage.getItem(`${storagePrefix}/sortBy`) ??
          `["tags.artist", "tags.album", "tags.disc.no", "tags.track.no"]`,
      );
      this.#sortDirection.value =
        /** @type {"desc" | "asc"} */ (localStorage.getItem(
          `${storagePrefix}/sortDirection`,
        ) ?? "asc");
    });

    // Effects
    this.effect(() => {
      const key = `${storagePrefix}/groupBy`;
      const val = this.#groupBy.value;

      if (val) localStorage.setItem(key, val);
      else localStorage.removeItem(key);
    });

    this.effect(() => {
      const key = `${storagePrefix}/playlistId`;
      const val = this.#playlist.value;

      if (val) localStorage.setItem(key, val);
      else localStorage.removeItem(key);
    });

    this.effect(() => {
      const key = `${storagePrefix}/searchTerm`;
      const val = this.#searchTerm.value;

      if (val) localStorage.setItem(key, val);
      else localStorage.removeItem(key);
    });

    this.effect(() => {
      const key = `${storagePrefix}/selection`;
      const val = this.#selection.value;

      if (val?.length) localStorage.setItem(key, JSON.stringify(val));
      else localStorage.removeItem(key);
    });

    this.effect(() => {
      const key = `${storagePrefix}/sortBy`;
      const val = this.#sortBy.value;

      if (val.length) localStorage.setItem(key, JSON.stringify(val));
      else localStorage.removeItem(key);
    });

    this.effect(() => {
      const key = `${storagePrefix}/sortDirection`;
      const val = this.#sortDirection.value;

      if (val) localStorage.setItem(key, val);
      else localStorage.removeItem(key);
    });
  }

  // ACTIONS

  revertToDefaultSort = async () => {
    this.#sortBy.value = [
      "tags.artist",
      "tags.album",
      "tags.disc.no",
      "tags.track.no",
    ];
    this.#sortDirection.value = "asc";
  };

  /** @param {string | undefined} val */
  setGroupBy = async (val) => this.#groupBy.value = val;

  /** @param {string | undefined} val */
  setPlaylist = async (val) => this.#playlist.value = val;

  /** @param {string | undefined} val */
  setSearchTerm = async (val) => this.#searchTerm.value = val;

  /** @param {SelectionCriterion[] | undefined} val */
  setSelection = async (val) => this.#selection.value = val;

  /** @param {string[]} val */
  setSortBy = async (val) => this.#sortBy.value = val;

  /** @param {"asc" | "desc" | undefined} val */
  setSortDirection = async (val) => this.#sortDirection.value = val;
}

export default ScopeEngine;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ScopeEngine;
export const NAME = "de-scope";

defineElement(NAME, CLASS);
