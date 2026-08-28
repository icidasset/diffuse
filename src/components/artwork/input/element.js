import { defineElement, DiffuseElement, query } from "~/common/element.js";

/**
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 * @import {InputElement} from "@specs/components/input/types.d.ts"
 * @import {Actions} from "@specs/components/artwork/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<Actions>}
 */
class InputArtwork extends DiffuseElement {
  static NAME = "diffuse/artwork/input";
  static WORKER_URL = "components/artwork/input/worker.js";

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

export default InputArtwork;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = InputArtwork;
export const NAME = "da-input";

defineElement(NAME, InputArtwork);
