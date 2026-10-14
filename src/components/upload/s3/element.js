import { defineElement, DiffuseElement } from "~/common/element.js";
import { SCHEME } from "~/components/input/s3/constants.js";

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
class S3Upload extends DiffuseElement {
  static NAME = "diffuse/upload/s3";
  static WORKER_URL = "components/upload/s3/worker.js";

  SCHEME = SCHEME;

  /** Where a bucket for this scheme is connected. */
  CONNECT_PATH = "facets/connect/s3/index.tile";

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

export default S3Upload;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = S3Upload;
export const NAME = "du-s3";

defineElement(NAME, CLASS);
