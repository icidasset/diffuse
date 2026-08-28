import { defineElement, DiffuseElement } from "~/common/element.js";
import { SCHEME } from "./constants.js";
import { hostsFromTracks } from "./common.js";

/**
 * @import {InputActions, InputSchemeProvider} from "@specs/components/input/types.d.ts"
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 * @import {Track} from "~/definitions/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<InputActions>}
 * @implements {InputSchemeProvider}
 */
class HttpsInput extends DiffuseElement {
  static NAME = "diffuse/input/https";
  static WORKER_URL = "components/input/https/worker.js";

  SCHEME = SCHEME;

  constructor() {
    super();

    /** @type {ProxiedActions<InputActions>} */
    this.proxy = this.workerProxy();

    this.artwork = this.proxy.artwork;
    this.consult = this.proxy.consult;
    this.detach = this.proxy.detach;
    this.groupConsult = this.proxy.groupConsult;
    this.list = this.proxy.list;
    this.resolve = this.proxy.resolve;
  }

  // 🛠️

  /** @param {Track[]} tracks */
  sources(tracks) {
    const hosts = Object.values(hostsFromTracks(tracks));

    return hosts.map((host) => ({
      label: host,
      uri: `https://${host}`,
    }));
  }
}

export default HttpsInput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = HttpsInput;
export const NAME = "di-https";

defineElement(NAME, CLASS);
