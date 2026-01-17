import { DiffuseElement } from "@common/element.js";
import { SCHEME } from "./constants.js";
import { bucketsFromTracks, buildURI } from "./common.js";

/**
 * @import {InputActions, InputSchemeProvider} from "@components/input/types.d.ts"
 * @import {ProxiedActions} from "@common/worker.d.ts"
 * @import {Track} from "@definitions/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<InputActions>}
 * @implements {InputSchemeProvider}
 */
class S3Input extends DiffuseElement {
  static NAME = "diffuse/input/s3";
  static WORKER_URL = "components/input/s3/worker.js";

  SCHEME = SCHEME;

  constructor() {
    super();

    /** @type {ProxiedActions<InputActions & { demo: () => Promise<void> }>} */
    this.proxy = this.workerProxy();

    this.consult = this.proxy.consult;
    this.detach = this.proxy.detach;
    this.groupConsult = this.proxy.groupConsult;
    this.list = this.proxy.list;
    this.resolve = this.proxy.resolve;

    this.demo = this.proxy.demo;
  }

  // 🛠️

  /** @param {Track[]} tracks */
  sources(tracks) {
    return Object.values(bucketsFromTracks(tracks)).map((server) => {
      return {
        label: `${server.bucketName} (${server.host})`,
        uri: buildURI(server),
      };
    });
  }
}

export default S3Input;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = S3Input;
export const NAME = "di-s3";

customElements.define(NAME, CLASS);
