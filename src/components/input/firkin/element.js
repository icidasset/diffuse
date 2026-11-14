import { defineElement, DiffuseElement } from "~/common/element.js";
import { SCHEME, SOURCE_URI } from "~/common/firkin/files.js";

/**
 * @import {InputActions, InputSchemeProvider} from "@specs/components/input/types.d.ts"
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 * @import {Track} from "~/definitions/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Audio source backed by Firkin blobs listed in the account's file registry.
 *
 * The worker reads the shared Firkin session from IndexedDB, so this element
 * does not depend on the WNFS output or a broadcast channel.
 *
 * @implements {ProxiedActions<InputActions>}
 * @implements {InputSchemeProvider}
 */
class FirkinInput extends DiffuseElement {
  static NAME = "diffuse/input/firkin";
  static WORKER_URL = "components/input/firkin/worker.js";

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

  /** @param {Track[]} _tracks */
  sources(_tracks) {
    return [{ label: "Firkin", uri: SOURCE_URI }];
  }
}

export default FirkinInput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = FirkinInput;
export const NAME = "di-firkin";

defineElement(NAME, CLASS);
