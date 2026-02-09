import * as Automerge from "@automerge/automerge";
import { isUint8Array } from "iso-base/utils";

import { computed } from "@common/signal.js";
import { recursivelyCloneRecords } from "@common/utils.js";
import { OutputTransformer } from "../../base.js";
import {
  INITIAL_CONSTITUENTS_DOCUMENT,
  INITIAL_TRACKS_DOCUMENT,
} from "./constants.js";

/**
 * @import { SignalReader } from "@common/signal.d.ts";
 * @import { OutputManagerDeputy } from "@components/output/types.d.ts"
 * @import { ConstituentsDocument, PlaylistsDocument, ThemesDocument, TracksDocument } from "./types.d.ts"
 */

/**
 * @extends {OutputTransformer<Uint8Array>}
 */
class AutomergeBytesOutputTransformer extends OutputTransformer {
  constructor() {
    super();

    const base = this.base();

    /** @type {SignalReader<Automerge.Doc<ConstituentsDocument>>} */
    const constituentsDocument = computed(() => {
      const value = base.constituents.collection();

      if (isUint8Array(value)) {
        return Automerge.load(value);
      } else if (value == undefined) {
        return INITIAL_CONSTITUENTS_DOCUMENT;
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
      constituents: {
        ...base.constituents,
        collection: computed(() => constituentsDocument().collection),
        save: async (newConstituents) => {
          const doc = Automerge.change(constituentsDocument(), (d) => {
            const clonedCollection = newConstituents.map((constituent) => {
              return recursivelyCloneRecords(constituent);
            });

            d.collection = clonedCollection;
          });

          const bytes = Automerge.save(doc);
          await base.constituents.save(bytes);
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
    this.tracks = manager.tracks;
  }
}

export default AutomergeBytesOutputTransformer;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = AutomergeBytesOutputTransformer;
export const NAME = "dtob-automerge";

customElements.define(NAME, CLASS);
