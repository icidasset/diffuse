import { defineElement, DiffuseElement } from "~/common/element.js";
import { SCHEME } from "./constants.js";
import { accountsFromTracks, buildURI } from "./common.js";

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
class DropboxInput extends DiffuseElement {
  static NAME = "diffuse/input/dropbox";
  static WORKER_URL = "components/input/dropbox/worker.js";

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
    return Object.values(accountsFromTracks(tracks)).map((account) => ({
      label: `Dropbox (${account.directoryPath})`,
      uri: buildURI(account),
    }));
  }
}

export default DropboxInput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = DropboxInput;
export const NAME = "di-dropbox";

defineElement(NAME, CLASS);
