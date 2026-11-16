import { defineElement, DiffuseElement } from "~/common/element.js";
import {
  parseBlobURI,
  parseURI,
  SCHEME,
  SOURCE_URI,
} from "~/common/firkin/files.js";

/**
 * @import {InputActions, InputSchemeProvider} from "@specs/components/input/types.d.ts"
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 * @import {Track} from "~/definitions/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Audio source for Firkin account files and generic CID-addressed blobs.
 *
 * Account files are listed from the account's file registry and use the
 * shared Firkin session. Generic `firkin://<host>/blob/<cid>` tracks resolve
 * anonymously via RASL by default; a `?token=` query parameter opts into an
 * authenticated read.
 *
 * The worker reads the shared Firkin session from IndexedDB, so account mode
 * only needs the Connect / Firkin page to have been used once.
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

  /** @param {Track[]} tracks */
  sources(tracks) {
    /** @type {Map<string, { label: string; uri: string }>} */
    const generic = new Map();
    let hasAccount = false;

    for (const track of tracks) {
      if (track.uri === SOURCE_URI || parseURI(track.uri)) {
        hasAccount = true;
        continue;
      }

      const blob = parseBlobURI(track.uri);
      if (!blob) continue;

      const uri = `firkin://${blob.host}`;
      if (!generic.has(uri)) {
        generic.set(uri, { label: `Firkin (${blob.host})`, uri });
      }
    }

    /** @type {{ label: string; uri: string }[]} */
    const sources = [...generic.values()];

    // The account source is only meaningful when this is the account library
    // (or there is nothing else to show). Generic CID sources stand alone.
    if (hasAccount || sources.length === 0) {
      sources.unshift({ label: "Firkin", uri: SOURCE_URI });
    }

    return sources;
  }
}

export default FirkinInput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = FirkinInput;
export const NAME = "di-firkin";

defineElement(NAME, CLASS);
