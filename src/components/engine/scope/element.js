import { BroadcastableDiffuseElement } from "@common/element.js";
import { signal } from "@common/signal.js";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class ScopeEngine extends BroadcastableDiffuseElement {
  static NAME = "diffuse/engine/scope";

  // SIGNALS

  #playlistId = signal(/** @type {string | undefined} */ (undefined));
  #searchTerm = signal(/** @type {string | undefined} */ (undefined));

  playlistId = this.#playlistId.get;
  searchTerm = this.#searchTerm.get;

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(this.nameWithGroup, {
        setPlaylistId: { strategy: "replicate", fn: this.setPlaylistId },
        setSearchTerm: { strategy: "replicate", fn: this.setSearchTerm },
      });

      if (actions) {
        this.setPlaylistId = actions.setPlaylistId;
        this.setSearchTerm = actions.setSearchTerm;
      }
    }

    // Super
    super.connectedCallback();

    // Signals
    const storagePrefix =
      `${this.constructor.prototype.constructor.NAME}/${this.group}/`;

    this.#playlistId.value =
      localStorage.getItem(`${storagePrefix}/playlistId`) ?? undefined;
    this.#searchTerm.value =
      localStorage.getItem(`${storagePrefix}/searchTerm`) ?? undefined;

    // Effects
    this.effect(() => {
      const key = `${storagePrefix}/playlistId`;
      const val = this.#playlistId.value;

      if (val) localStorage.setItem(key, val);
      else localStorage.removeItem(key);
    });

    this.effect(() => {
      const key = `${storagePrefix}/searchTerm`;
      const val = this.#searchTerm.value;

      if (val) localStorage.setItem(key, val);
      else localStorage.removeItem(key);
    });
  }

  // ACTIONS

  /** @param {string | undefined} val */
  setPlaylistId = async (val) => this.#playlistId.value = val;

  /** @param {string | undefined} val */
  setSearchTerm = async (val) => this.#searchTerm.value = val;
}

export default ScopeEngine;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ScopeEngine;
export const NAME = "de-scope";

customElements.define(NAME, CLASS);
