import {
  BroadcastableDiffuseElement,
  query,
  queryOptional,
} from "@common/element.js";
import { batch, computed, signal } from "@common/signal.js";
import { filterByPlaylist } from "@common/playlist.js";

/**
 * @import {Track} from "@definitions/types.d.ts"
 * @import {InputElement} from "@components/input/types.d.ts"
 * @import {OutputElement} from "@components/output/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class ScopedTracksOrchestrator extends BroadcastableDiffuseElement {
  static NAME = "diffuse/orchestrator/scoped-tracks";

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
    await customElements.whenDefined(input.localName);
    await customElements.whenDefined(output.localName);
    if (scope) await customElements.whenDefined(scope.localName);

    // Watch tracks collection
    this.effect(async () => {
      const collection = output.tracks.collection();
      if ((await this.isLeader()) === false) return;

      /** @type {string[]} */
      const uris = [];
      const tracks = collection.filter((t) => {
        uris.push(t.uri);
        return t.kind !== "placeholder";
      });

      // Consult inputs
      const groups = collection.length ? await input.groupConsult(uris) : {};

      /** @type {Set<string>} */
      const availableUris = new Set();

      Object.values(groups).forEach((value) => {
        if (value.available === false) return;
        for (const uri of value.uris) {
          availableUris.add(uri);
        }
      });

      const availableTracks = tracks.filter((t) => {
        return availableUris.has(t.uri);
      });

      // Set pool
      search.supply({ tracks: availableTracks });

      this.#tracksAvailable.set(availableTracks);
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

      this.#tracksFinal.set(final);
    });
  }
}

export default ScopedTracksOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ScopedTracksOrchestrator;
export const NAME = "do-scoped-tracks";

customElements.define(NAME, CLASS);
