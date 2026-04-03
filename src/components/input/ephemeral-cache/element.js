import { defineElement, DiffuseElement } from "~/common/element.js";
import { SCHEME } from "./constants.js";

/**
 * @import { InputActions, InputSchemeProvider } from "~/components/input/types.d.ts"
 * @import { ProxiedActions } from "~/common/worker.d.ts"
 * @import { Track } from "~/definitions/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<InputActions>}
 * @implements {InputSchemeProvider}
 */
class EphemeralCacheInput extends DiffuseElement {
  static NAME = "diffuse/input/ephemeral-cache";
  static WORKER_URL = "components/input/ephemeral-cache/worker.js";

  SCHEME = SCHEME;

  constructor() {
    super();

    /** @type {ProxiedActions<InputActions>} */
    const proxy = this.workerProxy();

    this.artwork = proxy.artwork;
    this.consult = proxy.consult;
    this.detach = proxy.detach;
    this.groupConsult = proxy.groupConsult;
    this.list = proxy.list;
    this.resolve = proxy.resolve;
  }

  // 🛠️

  /** @param {Track[]} tracks */
  sources(tracks) {
    return tracks.map((t) => ({
      label: t.uri,
      uri: t.uri,
    }));
  }
}

export default EphemeralCacheInput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = EphemeralCacheInput;
export const NAME = "di-ephemeral-cache";

defineElement(NAME, CLASS);
