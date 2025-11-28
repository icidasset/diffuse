import { DiffuseElement, workerProxy } from "@common/element.js";

/**
 * @import {InputActions} from "@components/input/types.d.ts"
 * @import {ProxiedActions} from "@common/worker.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<InputActions>}
 */
class OpensubsonicInput extends DiffuseElement {
  static NAME = "diffuse/input/opensubsonic";
  static WORKER_URL = "components/input/opensubsonic/worker.js";

  constructor() {
    super();

    /** @type {ProxiedActions<InputActions>} */
    const p = workerProxy(this.workerLink);

    this.consult = p.consult;
    this.contextualize = p.contextualize;
    this.groupConsult = p.groupConsult;
    this.list = p.list;
    this.resolve = p.resolve;
  }
}

export default OpensubsonicInput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = OpensubsonicInput;
export const NAME = "di-opensubsonic";

customElements.define(NAME, OpensubsonicInput);
