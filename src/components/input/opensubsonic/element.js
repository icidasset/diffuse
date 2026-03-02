import { DiffuseElement } from "~/common/element.js";
import { SCHEME } from "./constants.js";
import { buildURI, serversFromTracks } from "./common.js";

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
class OpensubsonicInput extends DiffuseElement {
  static NAME = "diffuse/input/opensubsonic";
  static WORKER_URL = "components/input/opensubsonic/worker.js";

  SCHEME = SCHEME;

  constructor() {
    super();

    /** @type {ProxiedActions<InputActions>} */
    this.proxy = this.workerProxy();

    this.consult = this.proxy.consult;
    this.detach = this.proxy.detach;
    this.groupConsult = this.proxy.groupConsult;
    this.list = this.proxy.list;
    this.resolve = this.proxy.resolve;
  }

  // 🛠️

  /** @param {Track[]} tracks */
  sources(tracks) {
    return Object.values(serversFromTracks(tracks)).map((server) => {
      return {
        label: `${server.host} (${server.username ?? server.apiKey})`,
        uri: buildURI(server),
      };
    });
  }
}

export default OpensubsonicInput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = OpensubsonicInput;
export const NAME = "di-opensubsonic";

customElements.define(NAME, CLASS);
