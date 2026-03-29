import { DiffuseElement, query } from "~/common/element.js";

/**
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 * @import {InputElement} from "~/components/input/types.d.ts"
 * @import {Actions} from "~/components/artwork/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<Actions>}
 */
class AudioMetadataArtwork extends DiffuseElement {
  static NAME = "diffuse/artwork/audio-metadata";
  static WORKER_URL = "components/artwork/audio-metadata/worker.js";

  constructor() {
    super();

    /** @type {ProxiedActions<Actions>} */
    const p = this.workerProxy();

    this.get = p.get;
  }

  // LIFECYCLE

  /** @override */
  async connectedCallback() {
    super.connectedCallback();

    /** @type {InputElement} */
    this.input = query(this, "input-selector");

    await customElements.whenDefined(this.input.localName);
  }

  // WORKERS

  /**
   * @override
   */
  dependencies() {
    if (!this.input) throw new Error("Input element not defined yet");
    return { input: this.input };
  }
}

export default AudioMetadataArtwork;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = AudioMetadataArtwork;
export const NAME = "da-audio-metadata";

customElements.define(NAME, AudioMetadataArtwork);
