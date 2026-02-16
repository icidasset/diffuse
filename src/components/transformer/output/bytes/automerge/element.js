import * as Automerge from "@automerge/automerge";
import { isUint8Array } from "iso-base/utils";

import { computed } from "@common/signal.js";
import { recursivelyCloneRecords } from "@common/utils.js";
import { OutputTransformer } from "../../base.js";
import {
  INITIAL_FACETS_DOCUMENT,
  INITIAL_PLAYLISTS_DOCUMENT,
  INITIAL_THEMES_DOCUMENT,
  INITIAL_TRACKS_DOCUMENT,
} from "./constants.js";

/**
 * @import { SignalReader } from "@common/signal.d.ts";
 * @import { OutputManagerDeputy } from "@components/output/types.d.ts"
 * @import { FacetsDocument, PlaylistsDocument, ThemesDocument, TracksDocument } from "./types.d.ts"
 */

/**
 * @extends {OutputTransformer<Uint8Array>}
 */
class AutomergeBytesOutputTransformer extends OutputTransformer {
  constructor() {
    super();

    const base = this.base();

    /** @type {SignalReader<Automerge.Doc<FacetsDocument>>} */
    const facetsDocument = computed(() => {
      const value = base.facets.collection();

      if (isUint8Array(value)) {
        return Automerge.load(value);
      } else if (value == undefined) {
        return INITIAL_FACETS_DOCUMENT;
      } else {
        // TODO: Better error
        throw new Error("Invalid data type");
      }
    });

    /** @type {SignalReader<Automerge.Doc<PlaylistsDocument>>} */
    const playlistsDocument = computed(() => {
      const value = base.playlists.collection();

      if (isUint8Array(value)) {
        return Automerge.load(value);
      } else if (value == undefined) {
        return INITIAL_PLAYLISTS_DOCUMENT;
      } else {
        // TODO: Better error
        throw new Error("Invalid data type");
      }
    });

    /** @type {SignalReader<Automerge.Doc<ThemesDocument>>} */
    const themesDocument = computed(() => {
      const value = base.themes.collection();

      if (isUint8Array(value)) {
        return Automerge.load(value);
      } else if (value == undefined) {
        return INITIAL_THEMES_DOCUMENT;
      } else {
        // TODO: Better error
        throw new Error("Invalid data type");
      }
    });

    /** @type {SignalReader<Automerge.Doc<TracksDocument>>} */
    const tracksDocument = computed(() => {
      const value = base.tracks.collection();

      if (isUint8Array(value)) {
        return Automerge.load(value);
      } else if (value == undefined) {
        return INITIAL_TRACKS_DOCUMENT;
      } else {
        // TODO: Better error
        throw new Error("Invalid data type");
      }
    });

    /** @type {OutputManagerDeputy} */
    const manager = {
      facets: {
        ...base.facets,
        collection: computed(() => facetsDocument().collection),
        save: async (newFacets) => {
          const doc = Automerge.change(facetsDocument(), (d) => {
            const clonedCollection = newFacets.map((facet) => {
              return recursivelyCloneRecords(facet);
            });

            d.collection = clonedCollection;
          });

          const bytes = Automerge.save(doc);
          await base.facets.save(bytes);
        },
      },
      playlists: {
        ...base.playlists,
        collection: computed(() => playlistsDocument().collection),
        save: async (newPlaylists) => {
          const doc = Automerge.change(playlistsDocument(), (d) => {
            const clonedCollection = newPlaylists.map((facet) => {
              return recursivelyCloneRecords(facet);
            });

            d.collection = clonedCollection;
          });

          const bytes = Automerge.save(doc);
          await base.playlists.save(bytes);
        },
      },
      themes: {
        ...base.themes,
        collection: computed(() => themesDocument().collection),
        save: async (newThemes) => {
          const doc = Automerge.change(themesDocument(), (d) => {
            const clonedCollection = newThemes.map((theme) => {
              return recursivelyCloneRecords(theme);
            });

            d.collection = clonedCollection;
          });

          const bytes = Automerge.save(doc);
          await base.themes.save(bytes);
        },
      },
      tracks: {
        ...base.tracks,
        collection: computed(() => tracksDocument().collection),
        save: async (newTracks) => {
          const doc = Automerge.change(tracksDocument(), (d) => {
            const clonedCollection = newTracks.map((track) => {
              return recursivelyCloneRecords(track);
            });

            d.collection = clonedCollection;
          });

          const bytes = Automerge.save(doc);
          await base.tracks.save(bytes);
        },
      },
    };

    // Assign manager properties to class
    this.facets = manager.facets;
    this.playlists = manager.playlists;
    this.themes = manager.themes;
    this.tracks = manager.tracks;
    this.ready = base.ready;
  }
}

export default AutomergeBytesOutputTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = AutomergeBytesOutputTransformer;
export const NAME = "dtob-automerge";

customElements.define(NAME, CLASS);
