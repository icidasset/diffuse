import { defineElement, DiffuseElement } from "~/common/element.js";
import { hostsFromTracks } from "./common.js";
import { SCHEME } from "./constants.js";

/**
 * @import {InputActions, InputSchemeProvider} from "~/components/input/types.d.ts"
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
class IcecastInput extends DiffuseElement {
  static NAME = "diffuse/input/icecast";
  static WORKER_URL = "components/input/icecast/worker.js";

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
      uri: `${SCHEME}://${host}`,
    }));
  }
}

export default IcecastInput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = IcecastInput;
export const NAME = "di-icecast";

defineElement(NAME, CLASS);
