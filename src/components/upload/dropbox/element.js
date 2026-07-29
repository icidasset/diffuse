import { defineElement, DiffuseElement } from "~/common/element.js";
import { DEFAULT_APP_KEY, PKCE_VERIFIER_KEY, SCHEME } from "~/components/input/dropbox/constants.js";
import { generatePKCEPair } from "~/components/input/dropbox/common.js";

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
class DropboxUpload extends DiffuseElement {
  static NAME = "diffuse/upload/dropbox";
  static WORKER_URL = "components/upload/dropbox/worker.js";

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

    /** @type {ProxiedActions<UploadActions>} */
    this.proxy = this.workerProxy();

    this.consult = this.proxy.consult;
    this.upload = this.proxy.upload;
    this.delete = this.proxy.delete;
    this.createSource = this.proxy.createSource;
  }

  // 🛠️

  async authorize() {
    localStorage.setItem("oauth/callback/redirect_path", location.pathname + location.search);

    const { verifier, challenge } = await generatePKCEPair();
    localStorage.setItem(PKCE_VERIFIER_KEY, verifier);

    const params = new URLSearchParams({
      response_type: "code",
      client_id: this.appKey,
      redirect_uri: location.origin + "/oauth/callback/",
      token_access_type: "offline",
      code_challenge: challenge,
      code_challenge_method: "S256",
    });

    location.assign(`https://www.dropbox.com/oauth2/authorize?${params}`);
  }
}

export default DropboxUpload;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = DropboxUpload;
export const NAME = "du-dropbox";

defineElement(NAME, CLASS);
