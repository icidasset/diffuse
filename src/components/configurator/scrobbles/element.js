import { DiffuseElement } from "~/common/element.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {ScrobbleActions, ScrobbleElement} from "~/components/supplement/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ScrobbleActions}
 */
class ScrobbleConfigurator extends DiffuseElement {
  static NAME = "diffuse/configurator/scrobbles";

  // SCROBBLE ACTIONS

  /**
   * @param {Track} track
   */
  async nowPlaying(track) {
    return await Promise.all(
      this.#activeScrobblers().map((s) => s.nowPlaying(track)),
    );
  }

  /**
   * @param {Track} track
   * @param {number} startedAt Unix timestamp in milliseconds
   */
  async scrobble(track, startedAt) {
    return await Promise.all(
      this.#activeScrobblers().map((s) => s.scrobble(track, startedAt)),
    );
  }

  // MISC

  /**
   * All child scrobble elements, regardless of authentication state.
   *
   * @returns {ScrobbleElement[]}
   */
  scrobblers() {
    return Array.from(this.root().children).flatMap((el) => {
      if (!("isAuthenticated" in el && "nowPlaying" in el)) return [];
      return [/** @type {ScrobbleElement} */ (/** @type {unknown} */ (el))];
    });
  }

  /**
   * Child scrobble elements that are currently authenticated.
   *
   * @returns {ScrobbleElement[]}
   */
  #activeScrobblers() {
    return this.scrobblers().filter((s) => s.isAuthenticated());
  }
}

export default ScrobbleConfigurator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ScrobbleConfigurator;
export const NAME = "dc-scrobbles";

customElements.define(NAME, ScrobbleConfigurator);
