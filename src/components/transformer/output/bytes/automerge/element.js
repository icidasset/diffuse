import * as Automerge from "@automerge/automerge";
import { isUint8Array } from "iso-base/utils";

import { computed } from "@common/signal.js";
import { OutputTransformer } from "../../base.js";
import { INITIAL_TRACKS_DOCUMENT } from "./constants.js";
import { recursivelyCloneRecords } from "@toko/diffuse/common/utils.js";

/**
 * @import { SignalReader } from "@common/signal.d.ts";
 * @import { OutputManagerDeputy } from "@components/output/types.d.ts"
 * @import { Track } from "@definitions/types.d.ts"
 * @import { TracksDocument } from "./types.d.ts"
 */

/**
 * @extends {OutputTransformer<Uint8Array>}
 */
class AutomergeBytesOutputTransformer extends OutputTransformer {
  constructor() {
    super();

    const base = this.base();

    /** @type {SignalReader<Automerge.Doc<TracksDocument>>} */
    const document = computed(() => {
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
      tracks: {
        ...base.tracks,
        collection: computed(() => document().collection),
        save: async (newTracks) => {
          const doc = Automerge.change(document(), (d) => {
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
