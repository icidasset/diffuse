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
class S3Input extends DiffuseElement {
  static NAME = "diffuse/input/s3";
  static WORKER_URL = "components/input/s3/worker.js";

  constructor() {
    super();

    /** @type {ProxiedActions<InputActions & { demo: () => Promise<void> }>} */
    const p = workerProxy(this.workerLink);

    this.consult = p.consult;
    this.contextualize = p.contextualize;
    this.groupConsult = p.groupConsult;
    this.list = p.list;
    this.resolve = p.resolve;

    this.demo = p.demo;
  }
}

export default S3Input;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = S3Input;
export const NAME = "di-s3";

customElements.define(NAME, CLASS);
