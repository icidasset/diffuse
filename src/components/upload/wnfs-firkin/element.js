import { defineElement, DiffuseElement } from "~/common/element.js";
import { SCHEME } from "~/components/output/bytes/wnfs/files.js";

/**
 * @import {UploadActions, UploadSchemeProvider} from "@specs/components/upload/types.d.ts"
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Uploader that stores files in the Firkin + WNFS WNFS.
 *
 * @implements {ProxiedActions<UploadActions>}
 * @implements {UploadSchemeProvider}
 */
class WnfsFirkinUpload extends DiffuseElement {
  static NAME = "diffuse/upload/wnfs-firkin";
  static WORKER_URL = "components/upload/wnfs-firkin/worker.js";

  SCHEME = SCHEME;

  /** Where the Firkin + WNFS output is connected. */
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

  /**
   * Pass the output element's group/namespace to the worker so it can find the
   * matching broadcast channel.
   */
  workerQuery() {
    return { group: this.group, namespace: this.namespace ?? "" };
  }
}

export default WnfsFirkinUpload;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = WnfsFirkinUpload;
export const NAME = "du-wnfs-firkin";

defineElement(NAME, CLASS);
