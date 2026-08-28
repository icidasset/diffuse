import { defineElement, DiffuseElement } from "~/common/element.js";

/**
 * @import {ProxiedActions, Tunnel} from "~/common/worker.d.ts"
 * @import {UploadElement} from "@specs/components/upload/types.d.ts"
 * @import {Actions} from "@specs/components/configurator/upload/types.d.ts"
 */

/**
 * @typedef {{ element: UploadElement, tunnel: Tunnel, worker: Worker | SharedWorker }} Upload
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<Actions>}
 */
class UploadConfigurator extends DiffuseElement {
  static NAME = "diffuse/configurator/upload";
  static WORKER_URL = "components/configurator/upload/worker.js";

  constructor() {
    super();

    /** @type {ProxiedActions<Actions>} */
    const proxy = this.workerProxy();

    this.consult = proxy.consult;
    this.upload = proxy.upload;
    this.delete = proxy.delete;
    this.createSource = proxy.createSource;
  }

  // WORKERS

  /**
   * @override
   */
  dependencies() {
    return this.uploaders();
  }

  uploaders() {
    return Object.fromEntries(
      Array.from(this.children).map((element) => {
        const upload = /** @type {UploadElement} */ (element);
        return [upload.SCHEME, upload];
      }),
    );
  }
}

export default UploadConfigurator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = UploadConfigurator;
export const NAME = "dc-upload";

defineElement(NAME, CLASS);
