import { BroadcastableDiffuseElement } from "~/common/element.js";
import { signal } from "~/common/signal.js";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class ScopeEngine extends BroadcastableDiffuseElement {
  static NAME = "diffuse/engine/scope";

  // SIGNALS

  #playlist = signal(/** @type {string | undefined} */ (undefined));
  #searchTerm = signal(/** @type {string | undefined} */ (undefined));
  #sortBy = signal(/** @type {string[]} */ ([]));

  playlist = this.#playlist.get;
  searchTerm = this.#searchTerm.get;
  sortBy = this.#sortBy.get;

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(this.identifier, {
        setPlaylist: { strategy: "replicate", fn: this.setPlaylist },
        setSearchTerm: { strategy: "replicate", fn: this.setSearchTerm },
        setSortBy: { strategy: "replicate", fn: this.setSortBy },
      });

      if (actions) {
        this.setPlaylist = actions.setPlaylist;
        this.setSearchTerm = actions.setSearchTerm;
        this.setSortBy = actions.setSortBy;
      }
    }

    // Super
    super.connectedCallback();

    // Signals
    const storagePrefix =
      `${this.constructor.prototype.constructor.NAME}/${this.group}/`;

    this.#playlist.value =
      localStorage.getItem(`${storagePrefix}/playlistId`) ?? undefined;
    this.#searchTerm.value =
      localStorage.getItem(`${storagePrefix}/searchTerm`) ?? undefined;
    this.#sortBy.value =
      JSON.parse(localStorage.getItem(`${storagePrefix}/sortBy`) ?? "[]");

    // Effects
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
  }

  // ACTIONS

  /** @param {string | undefined} val */
  setPlaylist = async (val) => this.#playlist.value = val;

  /** @param {string | undefined} val */
  setSearchTerm = async (val) => this.#searchTerm.value = val;

  /** @param {string[]} val */
  setSortBy = async (val) => this.#sortBy.value = val;
}

export default ScopeEngine;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ScopeEngine;
export const NAME = "de-scope";

customElements.define(NAME, CLASS);
