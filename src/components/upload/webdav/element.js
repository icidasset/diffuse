import { defineElement, DiffuseElement } from "~/common/element.js";
import { SCHEME } from "~/components/input/webdav/constants.js";

/**
 * @import {UploadActions, UploadSchemeProvider} from "@specs/components/upload/types.d.ts"
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<UploadActions>}
 * @implements {UploadSchemeProvider}
 */
class WebdavUpload extends DiffuseElement {
  static NAME = "diffuse/upload/webdav";
  static WORKER_URL = "components/upload/webdav/worker.js";

  SCHEME = SCHEME;

  /** Where a server for this scheme is connected. */
  CONNECT_PATH = "facets/connect/webdav/index.tile";

  constructor() {
    super();

    /** @type {ProxiedActions<UploadActions>} */
    this.proxy = this.workerProxy();

    this.consult = this.proxy.consult;
    this.upload = this.proxy.upload;
    this.delete = this.proxy.delete;
    this.createSource = this.proxy.createSource;
  }
}

export default WebdavUpload;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = WebdavUpload;
export const NAME = "du-webdav";

defineElement(NAME, CLASS);
