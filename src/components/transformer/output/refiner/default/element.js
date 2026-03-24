import * as IDB from "idb-keyval";

import { computed, signal } from "~/common/signal.js";
import { OutputTransformer } from "../../base.js";

const IDB_KEY_PLAYLISTS =
  "diffuse/transformer/output/refiner/default/playlistItems/ephemeral";
const IDB_KEY_TRACKS =
  "diffuse/transformer/output/refiner/default/tracks/ephemeral";

/**
 * @import { OutputManagerDeputy } from "~/components/output/types.d.ts"
 * @import { PlaylistItem, Track } from "~/definitions/types.d.ts"
 */

/**
 * @extends {OutputTransformer}
 */
class DefaultOutputRefinerTransformer extends OutputTransformer {
  constructor() {
    super();

    const base = this.base();

    // Ephemeral signals
    const ephemeralPlaylistItems = signal(/** @type {any[]} */ ([]));
    const ephemeralTracks = signal(/** @type {any[]} */ ([]));

    // Restore stored ephemeral items
    IDB.get(IDB_KEY_PLAYLISTS).then((items) => {
      if (items) ephemeralPlaylistItems.set(items);
    });

    IDB.get(IDB_KEY_TRACKS).then((items) => {
      if (items) ephemeralTracks.set(items);
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
            data: [...col.data, ...ephemeralPlaylistItems.get()],
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
          ephemeralPlaylistItems.set(ephemeral);

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
            data: [...col.data, ...ephemeralTracks.get()],
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
          ephemeralTracks.set(ephemeral);

          await base.tracks.save(filtered);
        },
      },

      // Other
      ready: base.ready,
    };

    // Assign manager properties to class
    this.facets = manager.facets;
    this.playlistItems = manager.playlistItems;
    this.tracks = manager.tracks;
    this.ready = manager.ready;
  }
}

export default DefaultOutputRefinerTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = DefaultOutputRefinerTransformer;
export const NAME = "dtor-default";

if (!customElements.get(NAME)) customElements.define(NAME, CLASS);
