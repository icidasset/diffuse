import { defineElement, DiffuseElement } from "~/common/element.js";

/**
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 * @import {Actions} from "@specs/components/artwork/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<Actions>}
 */
class LastFmArtwork extends DiffuseElement {
  static NAME = "diffuse/artwork/last.fm";
  static WORKER_URL = "components/artwork/last.fm/worker.js";

  constructor() {
    super();

    /** @type {ProxiedActions<Actions>} */
    const p = this.workerProxy();

    this.get = p.get;
  }
}

export default LastFmArtwork;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = LastFmArtwork;
export const NAME = "da-lastfm";

defineElement(NAME, LastFmArtwork);
