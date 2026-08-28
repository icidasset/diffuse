import {
  BroadcastableDiffuseElement,
  defineElement,
} from "~/common/element.js";
import { batch, signal } from "~/common/signal.js";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class ScopeEngine extends BroadcastableDiffuseElement {
  static NAME = "diffuse/engine/scope";

  // SIGNALS

  #groupBy = signal(/** @type {string | undefined} */ (undefined));
  #playlist = signal(/** @type {string | undefined} */ (undefined));
  #searchTerm = signal(/** @type {string | undefined} */ (undefined));
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
        setSortBy: { strategy: "replicate", fn: this.setSortBy },
        setSortDirection: { strategy: "replicate", fn: this.setSortDirection },
      });

      if (actions) {
        this.revertToDefaultSort = actions.revertToDefaultSort;
        this.setGroupBy = actions.setGroupBy;
        this.setPlaylist = actions.setPlaylist;
        this.setSearchTerm = actions.setSearchTerm;
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
