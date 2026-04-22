import { defineElement, DiffuseElement } from "~/common/element.js";
import { DEFAULT_APP_KEY, SCHEME } from "./constants.js";
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

  /** @type {string} */
  appKey = DEFAULT_APP_KEY;

  static observedAttributes = ["app-key"];

  /**
   * @override
   * @param {string} name
   * @param {string} old
   * @param {string} next
   */
  attributeChangedCallback(name, old, next) {
    super.attributeChangedCallback(name, old, next);
    if (name === "app-key" && next !== null) this.appKey = next;
  }

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

  authorize() {
    sessionStorage.setItem("oauth/callback/redirect_path", location.pathname + location.search);

    const params = new URLSearchParams({
      response_type: "token",
      client_id: this.appKey,
      redirect_uri: location.origin + "/oauth/callback/",
    });

    location.assign(`https://www.dropbox.com/oauth2/authorize?${params}`);
  }

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
