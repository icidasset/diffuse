import {
  BroadcastableDiffuseElement,
  query,
  queryOptional,
} from "@common/element.js";
import { computed, signal } from "@common/signal.js";
import { filterByPlaylist } from "@common/playlist.js";

/**
 * @import {Track} from "@definitions/types.d.ts"
 * @import {ProxiedActions} from "@common/worker.d.ts"
 * @import {InputElement} from "@components/input/types.d.ts"
 * @import {OutputElement} from "@components/output/types.d.ts"
 *
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class ScopedTracksOrchestrator extends BroadcastableDiffuseElement {
  static NAME = "diffuse/orchestrator/scoped-tracks";
  static WORKER_URL = "components/orchestrator/scoped-tracks/worker.js";

  /** @type {ProxiedActions<Actions>} */
  #proxy;

  constructor() {
    super();
    this.#proxy = this.workerProxy({
      forceNew: {
        dependencies: {
          input: true,
        },
      },
    });
  }

  // SIGNALS

  #input = signal(/** @type {InputElement | null} */ (null));
  #output = signal(/** @type {OutputElement | null} */ (null));

  #scope = signal(
    /** @type {import("@components/engine/scope/element.js").CLASS | null} */ (null),
  );

  #search = signal(
    /** @type {import("@components/processor/search/element.js").CLASS | null} */ (null),
  );

  #selectedPlaylistItems = computed(() => {
    const playlist = this.#scope.value?.playlist();
    if (!playlist) return undefined;

    return this.#output.value?.playlistItems.collection().filter((p) =>
      p.playlist === playlist
    );
  });

  #tracksAvailable = signal(/** @type {Track[]} */ ([]));
  #tracksSearch = signal(/** @type {Track[]} */ ([]));
  #tracksFinal = signal(/** @type {Track[]} */ ([]));

  // STATE

  tracks = this.#tracksFinal.get;

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(this.nameWithGroup, {
        setTracksSearch: {
          strategy: "replicate",
          fn: this.#tracksSearch.set,
        },
        setTracksFinal: {
          strategy: "replicate",
          fn: this.#tracksFinal.set,
        },
      });

      if (actions) {
        this.#tracksSearch.set = actions.setTracksSearch;
        this.#tracksFinal.set = actions.setTracksFinal;
      }
    }

    // Super
    super.connectedCallback();

    /** @type {InputElement} */
    const input = query(this, "input-selector");

    /** @type {OutputElement} */
    const output = query(this, "output-selector");

    /** @type {import("@components/processor/search/element.js").CLASS} */
    const search = query(this, "search-processor-selector");

    /** @type {import("@components/engine/scope/element.js").CLASS | null} */
    const scope = queryOptional(this, "scope-engine-selector");

    // Assign to self
    this.#input.value = input;
    this.#output.value = output;
    this.#search.value = search;
    if (scope) this.#scope.value = scope;

    // When defined
    await customElements.whenDefined(output.localName);
    if (scope) await customElements.whenDefined(scope.localName);

    const startTime = performance.now();

    // Watch tracks collection
    this.effect(async () => {
      const collection = output.tracks.collection();
      console.log("🫠", collection.length);
      if ((await this.isLeader()) === false) return;
      const { availableTracks } = await this.#proxy.supply(collection);
      this.#tracksAvailable.value = availableTracks;
    });

    // Watch search supply
    this.effect(async () => {
      const _trigger = search.supplyFingerprint();
      const availableTracks = this.#tracksAvailable.value;
      const searchTerm = this.#scope.value?.searchTerm();

      if ((await this.isLeader()) === false) return;

      if (searchTerm?.length) {
        const searchResults = await search.search({
          term: searchTerm,
        });
        this.#tracksSearch.set(searchResults);
      } else {
        this.#tracksSearch.set(availableTracks);
      }
    });

    // Watch `#tracksSearch` + Playlist
    this.effect(async () => {
      const tracks = this.#tracksSearch.value;
      const playlistItems = this.#selectedPlaylistItems();

      if ((await this.isLeader()) === false) return;

      const final = playlistItems?.length
        ? filterByPlaylist(tracks, playlistItems)
        : tracks;

      const endTime = performance.now();
      console.log("🚀", endTime - startTime);

      this.#tracksFinal.set(final);
    });
  }

  // WORKERS

  /**
   * @override
   */
  dependencies() {
    if (!this.#input.value) throw new Error("Input element not defined yet");
    if (!this.#search.value) throw new Error("Search element not defined yet");

    return {
      input: this.#input.value,
      search: this.#search.value,
    };
  }
}

export default ScopedTracksOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ScopedTracksOrchestrator;
export const NAME = "do-scoped-tracks";

customElements.define(NAME, CLASS);
