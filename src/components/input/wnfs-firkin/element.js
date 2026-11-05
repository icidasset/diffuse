import { defineElement, DiffuseElement } from "~/common/element.js";
import { SCHEME, SOURCE_URI } from "~/components/output/bytes/wnfs/files.js";

/**
 * @import {InputActions, InputSchemeProvider} from "@specs/components/input/types.d.ts"
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 * @import {Track} from "~/definitions/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Audio source backed by the files in the Firkin + WNFS WNFS.
 *
 * The actual WNFS work happens in the storage output element; this element's
 * worker reaches it over a `BroadcastChannel`.
 *
 * @implements {ProxiedActions<InputActions>}
 * @implements {InputSchemeProvider}
 */
class WnfsFirkinInput extends DiffuseElement {
  static NAME = "diffuse/input/wnfs-firkin";
  static WORKER_URL = "components/input/wnfs-firkin/worker.js";

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

  /**
   * Pass the output element's group/namespace to the worker so it can find the
   * matching broadcast channel.
   */
  workerQuery() {
    return { group: this.group, namespace: this.namespace ?? "" };
  }

  // 🛠️

  /** @param {Track[]} _tracks */
  sources(_tracks) {
    return [{ label: "Firkin + WNFS", uri: SOURCE_URI }];
  }
}

export default WnfsFirkinInput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = WnfsFirkinInput;
export const NAME = "di-wnfs-firkin";

defineElement(NAME, CLASS);
