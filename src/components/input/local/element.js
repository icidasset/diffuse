import * as TID from "@atcute/tid";
import { DiffuseElement } from "~/common/element.js";
import { SCHEME } from "./constants.js";
import {
  buildURI,
  loadHandles,
  saveHandles,
  tidsFromTracks,
} from "./common.js";

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
class LocalInput extends DiffuseElement {
  static NAME = "diffuse/input/local";
  static WORKER_URL = "components/input/local/worker.js";

  SCHEME = SCHEME;

  /** @type {Map<string, string>} tid → handle name */
  #names = new Map();

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

  // LIFECYCLE

  /** @override */
  async connectedCallback() {
    super.connectedCallback();
    const handles = await loadHandles();
    for (const [tid, handle] of Object.entries(handles)) {
      this.#names.set(tid, handle.name);
    }
  }

  // 🛠️

  /**
   * Prompts the user to pick a directory.
   * Stores handle in IDB.
   * Returns the URI for the track placeholder.
   */
  async addDirectory() {
    const dirHandle = await /** @type {any} */ (globalThis).showDirectoryPicker(
      { mode: "read" },
    );

    const tid = TID.now();
    const handles = await loadHandles();

    handles[tid] = dirHandle;
    await saveHandles(handles);
    this.#names.set(tid, dirHandle.name);

    return buildURI(tid);
  }

  /**
   * Prompts the user to pick one or more files.
   * Stores handles in IDB.
   * Returns the URIs for the track placeholders.
   */
  async addFiles() {
    const fileHandles = await /** @type {any} */ (globalThis)
      .showOpenFilePicker({ multiple: true });
    const handles = await loadHandles();
    const uris = [];

    for (const fileHandle of fileHandles) {
      const tid = TID.now();
      handles[tid] = fileHandle;
      this.#names.set(tid, fileHandle.name);
      uris.push(buildURI(tid));
    }

    await saveHandles(handles);
    return uris;
  }

  /** @param {Track[]} tracks */
  sources(tracks) {
    return Object.values(tidsFromTracks(tracks)).map((tid) => ({
      label: this.#names.get(tid) ?? tid,
      uri: buildURI(tid),
    }));
  }
}

export default LocalInput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = LocalInput;
export const NAME = "di-local";

customElements.define(NAME, CLASS);
