import { defineElement, DiffuseElement } from "~/common/element.js";
import { SCHEME } from "./constants.js";
import { buildURI, serversFromTracks } from "./common.js";

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
class HttpsJsonInput extends DiffuseElement {
  static NAME = "diffuse/input/https-json";
  static WORKER_URL = "components/input/https-json/worker.js";

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
    return Object.values(serversFromTracks(tracks)).map((server) => ({
      label: `${server.host}${server.dir}`,
      uri: buildURI(server),
    }));
  }
}

export default HttpsJsonInput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = HttpsJsonInput;
export const NAME = "di-https-json";

defineElement(NAME, CLASS);
