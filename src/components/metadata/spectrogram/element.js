import { defineElement, DiffuseElement, query } from "~/common/element.js";

/**
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 * @import {InputElement} from "@specs/components/input/types.d.ts"
 * @import {Actions} from "@specs/components/metadata/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Computes a spectrogram for a track and stores the derived spectral
 * descriptors on the track's `stats`.
 *
 * Like the audio-file metadata element, it exposes a `patch(track)` action and
 * is designed to be a child of the `<dc-metadata>` configurator, where it runs
 * alongside `<dm-audio-file>` during track processing.
 *
 * @implements {ProxiedActions<Actions>}
 */
class SpectrogramMetadata extends DiffuseElement {
  static NAME = "diffuse/metadata/spectrogram";
  static WORKER_URL = "components/metadata/spectrogram/worker.js";

  constructor() {
    super();

    /** @type {ProxiedActions<Actions>} */
    const p = this.workerProxy();

    this.patch = p.patch;
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

export default SpectrogramMetadata;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = SpectrogramMetadata;
export const NAME = "dm-spectrogram";

defineElement(NAME, SpectrogramMetadata);
