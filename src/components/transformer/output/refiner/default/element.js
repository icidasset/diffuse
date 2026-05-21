import * as IDB from "idb-keyval";

import { computed, signal } from "~/common/signal.js";
import { OutputTransformer } from "../../base.js";
import { defineElement } from "~/common/element.js";

const IDB_KEY_PLAYLISTS =
  "diffuse/transformer/output/refiner/default/playlistItems/ephemeral";
const IDB_KEY_TRACKS =
  "diffuse/transformer/output/refiner/default/tracks/ephemeral";

/**
 * @import { OutputManagerDeputy } from "@specs/components/output/types.d.ts"
 * @import { PlaylistItem, Track } from "~/definitions/types.d.ts"
 */

/**
 * @extends {OutputTransformer}
 */
class DefaultOutputRefinerTransformer extends OutputTransformer {
  constructor() {
    super();

    const base = this.base();

    // Restore stored ephemeral items
    IDB.get(IDB_KEY_PLAYLISTS).then((items) => {
      if (items) this.#ephemeralPlaylistItems.set(items);
    });

    IDB.get(IDB_KEY_TRACKS).then((items) => {
      if (items) this.#ephemeralTracks.set(items);
    });

    /** @type {OutputManagerDeputy} */
    const manager = {
      facets: {
        ...base.facets,
        collection: computed(() => {
          const col = base.facets.collection();
          if (col.state !== "loaded") return col;
          return { state: "loaded", data: col.data };
        }),
      },
      playlistItems: {
        ...base.playlistItems,
        collection: computed(() => {
          const col = base.playlistItems.collection();
          if (col.state !== "loaded") return col;
          return {
            state: "loaded",
            data: [...col.data, ...this.#ephemeralPlaylistItems.get()],
          };
        }),
        save: async (newPlaylists) => {
          /** @type {PlaylistItem[]} */
          const ephemeral = [];

          const filtered = newPlaylists.filter((p) => {
            if (p.ephemeral) {
              ephemeral.push(p);
              return false;
            } else {
              return true;
            }
          });

          await IDB.set(IDB_KEY_PLAYLISTS, ephemeral);
          this.#ephemeralPlaylistItems.set(ephemeral);

          await base.playlistItems.save(filtered);
        },
      },
      tracks: {
        ...base.tracks,
        collection: computed(() => {
          const col = base.tracks.collection();
          if (col.state !== "loaded") return col;
          return {
            state: "loaded",
            data: [...col.data, ...this.#ephemeralTracks.get()],
          };
        }),
        save: async (newTracks) => {
          /** @type {Track[]} */
          const ephemeral = [];

          const filtered = newTracks.filter((t) => {
            if (t.ephemeral) {
              ephemeral.push(t);
              return false;
            } else {
              return true;
            }
          });

          await IDB.set(IDB_KEY_TRACKS, ephemeral);
          this.#ephemeralTracks.set(ephemeral);

          await base.tracks.save(filtered);
        },
      },

      settings: base.settings,

      // Other
      ready: base.ready,
    };

    // Assign manager properties to class
    this.facets = manager.facets;
    this.playlistItems = manager.playlistItems;
    this.settings = manager.settings;
    this.tracks = manager.tracks;
    this.ready = manager.ready;
  }

  // SIGNALS

  #ephemeralPlaylistItems = signal(/** @type {PlaylistItem[]} */ ([]));
  #ephemeralTracks = signal(/** @type {Track[]} */ ([]));

  // LIFECYCLE

  /** @override */
  connectedCallback() {
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(IDB_KEY_TRACKS, {
        getEphemeralPlaylistItems: {
          strategy: "leaderOnly",
          fn: this.#ephemeralPlaylistItems.get,
        },
        setEphemeralPlaylistItems: {
          strategy: "replicate",
          fn: this.#ephemeralPlaylistItems.set,
        },
        getEphemeralTracks: {
          strategy: "leaderOnly",
          fn: this.#ephemeralTracks.get,
        },
        setEphemeralTracks: {
          strategy: "replicate",
          fn: this.#ephemeralTracks.set,
        },
      });

      if (actions) {
        this.#ephemeralPlaylistItems.set = actions.setEphemeralPlaylistItems;
        this.#ephemeralTracks.set = actions.setEphemeralTracks;

        actions.getEphemeralPlaylistItems().then((items) => {
          this.#ephemeralPlaylistItems.value = items;
        });

        actions.getEphemeralTracks().then((tracks) => {
          this.#ephemeralTracks.value = tracks;
        });
      }
    }

    super.connectedCallback();
  }
}

export default DefaultOutputRefinerTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = DefaultOutputRefinerTransformer;
export const NAME = "dtor-default";

defineElement(NAME, CLASS);
