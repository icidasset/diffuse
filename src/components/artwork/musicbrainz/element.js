import { defineElement, DiffuseElement } from "~/common/element.js";

/**
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 * @import {Actions} from "~/components/artwork/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<Actions>}
 */
class MusicBrainzArtwork extends DiffuseElement {
  static NAME = "diffuse/artwork/musicbrainz";
  static WORKER_URL = "components/artwork/musicbrainz/worker.js";

  constructor() {
    super();

    /** @type {ProxiedActions<Actions>} */
    const p = this.workerProxy();

    this.get = p.get;
  }
}

export default MusicBrainzArtwork;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = MusicBrainzArtwork;
export const NAME = "da-musicbrainz";

defineElement(NAME, MusicBrainzArtwork);
