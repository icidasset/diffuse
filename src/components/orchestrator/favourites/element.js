import { BroadcastableDiffuseElement, query } from "~/common/element.js";
import { match as matchPlaylistItem } from "~/common/playlist.js";
import { computed, signal } from "~/common/signal.js";
import { filterFavourites } from "./common.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 * @import {OutputElement} from "~/components/output/types.d.ts"
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class FavouritesOrchestrator extends BroadcastableDiffuseElement {
  static NAME = "diffuse/orchestrator/favourites";
  static WORKER_URL = "components/orchestrator/favourites/worker.js";

  /** @type {ProxiedActions<Actions>} */
  #proxy;

  constructor() {
    super();

    this.#proxy = this.workerProxy();

    // Bind methods for broadcasting
    this.include = this.include.bind(this);
    this.expel = this.expel.bind(this);
    this.toggle = this.toggle.bind(this);
  }

  // SIGNALS

  #output = signal(/** @type {OutputElement | null} */ (null));

  // STATE

  /**
   * Returns the favourites playlist items.
   */
  playlistItems = computed(() => {
    const output = this.#output.value;
    if (!output) return [];

    const col = output.playlistItems.collection();
    if (col.state !== "loaded") return [];
    return filterFavourites(col.data);
  });

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(this.identifier, {
        include: { strategy: "leaderOnly", fn: this.include },
        expel: { strategy: "leaderOnly", fn: this.expel },
        toggle: { strategy: "leaderOnly", fn: this.toggle },
      });

      if (actions) {
        this.include = actions.include;
        this.expel = actions.expel;
        this.toggle = actions.toggle;
      }
    }

    // Super
    super.connectedCallback();

    /** @type {OutputElement} */
    const output = query(this, "output-selector");

    // Wait until defined
    await customElements.whenDefined(output.localName);

    this.#output.value = output;
  }

  // ACTIONS

  /**
   * Add one or more tracks to favourites.
   * @param {Track | Track[]} tracks
   */
  async include(tracks) {
    const tracksArray = Array.isArray(tracks) ? tracks : [tracks];
    if (tracksArray.length === 0) return;

    const output = this.#output.value;
    if (!output) {
      console.warn("Favourites orchestrator: output element not ready");
      return;
    }

    const col = output.playlistItems.collection();
    const playlistItems = col.state === "loaded" ? col.data : [];
    const result = await this.#proxy.include({
      playlistItems,
      tracks: tracksArray,
    });

    if (result) await output.playlistItems.save(result);
  }

  /**
   * Remove one or more tracks from favourites.
   * @param {Track | Track[]} tracks
   */
  async expel(tracks) {
    const tracksArray = Array.isArray(tracks) ? tracks : [tracks];
    if (tracksArray.length === 0) return;

    const output = this.#output.value;
    if (!output) {
      console.warn("Favourites orchestrator: output element not ready");
      return;
    }

    const col = output.playlistItems.collection();
    const playlistItems = col.state === "loaded" ? col.data : [];
    const result = await this.#proxy.expel({
      playlistItems,
      tracks: tracksArray,
    });

    if (result) await output.playlistItems.save(result);
  }

  /**
   * Toggle favourite status for one or more tracks.
   * Adds tracks if not in favourites, removes if already favourited.
   * @param {Track | Track[]} tracks
   */
  async toggle(tracks) {
    const tracksArray = Array.isArray(tracks) ? tracks : [tracks];
    if (tracksArray.length === 0) return;

    const output = this.#output.value;
    if (!output) {
      console.warn("Favourites orchestrator: output element not ready");
      return;
    }

    const col = output.playlistItems.collection();
    const playlistItems = col.state === "loaded" ? col.data : [];
    const result = await this.#proxy.toggle({
      playlistItems,
      tracks: tracksArray,
    });

    if (result) await output.playlistItems.save(result);
  }

  // 🛠️

  /**
   * Check if a track is a favourite.
   *
   * @param {Track} track
   */
  isFavourite(track) {
    return this.playlistItems().some((item) => {
      return matchPlaylistItem(track, item);
    });
  }
}

export default FavouritesOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = FavouritesOrchestrator;
export const NAME = "do-favourites";

customElements.define(NAME, CLASS);
