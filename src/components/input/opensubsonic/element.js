import { DiffuseElement } from "@common/element.js";
import { use } from "@common/worker.js";

/**
 * @import {InputActions} from "@common/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {InputActions}
 */
class OpensubsonicInput extends DiffuseElement {
  constructor() {
    super();

    // Setup worker
    const name = `diffuse/input/opensubsonic/${this.group}`;
    const url = "/components/input/opensubsonic/worker.js";
    const worker = new Worker(url, { name, type: "module" });

    // Worker proxy
    this.consult = use("consult", worker);
    this.contextualize = use("contextualize", worker);
    this.groupConsult = use("groupConsult", worker);
    this.list = use("list", worker);
    this.resolve = use("resolve", worker);
  }
}

export default OpensubsonicInput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = OpensubsonicInput;
export const NAME = "di-opensubsonic";

customElements.define(NAME, OpensubsonicInput);
