import { defineElement, DiffuseElement } from "~/common/element.js";
import { SCHEME } from "~/common/firkin/files.js";

/**
 * @import {UploadActions, UploadSchemeProvider} from "@specs/components/upload/types.d.ts"
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Uploader that stores audio on Firkin and records file names in the account's
 * file registry.
 *
 * The worker reads the shared Firkin session from IndexedDB, so this element
 * does not depend on the WNFS output or a broadcast channel.
 *
 * @implements {ProxiedActions<UploadActions>}
 * @implements {UploadSchemeProvider}
 */
class FirkinUpload extends DiffuseElement {
  static NAME = "diffuse/upload/firkin";
  static WORKER_URL = "components/upload/firkin/worker.js";

  SCHEME = SCHEME;

  /** Where the Firkin account is connected. */
  CONNECT_PATH = "facets/connect/wnfs-firkin/index.tile";

  constructor() {
    super();

    /** @type {ProxiedActions<UploadActions>} */
    const proxy = this.workerProxy();

    this.consult = proxy.consult;
    this.upload = proxy.upload;
    this.delete = proxy.delete;
    this.createSource = proxy.createSource;
  }
}

export default FirkinUpload;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = FirkinUpload;
export const NAME = "du-firkin";

defineElement(NAME, CLASS);
