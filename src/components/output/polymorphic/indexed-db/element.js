import * as IDB from "idb-keyval";

import { IDB_PREFIX } from "./constants.js";
import { BroadcastedOutputElement, outputManager } from "../../common.js";

/**
 * @import {OutputElement, OutputManager, OutputWorkerActions} from "../../types.d.ts"
 * @import {SupportedDataTypes} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {OutputElement<SupportedDataTypes>}
 */
class IndexedDBOutput extends BroadcastedOutputElement {
  static NAME = "diffuse/output/polymorphic/indexed-db";
  static WORKER_URL = "components/output/polymorphic/indexed-db/worker.js";

  #manager;

  constructor() {
    super();

    /** @type {OutputManager<SupportedDataTypes>} */
    this.#manager = outputManager({
      facets: {
        empty: () => undefined,
        get: () => this.#get("facets"),
        put: (data) => this.#put("facets", data),
      },
      init: () => this.whenConnected(),
      playlistItems: {
        empty: () => undefined,
        get: () => this.#get("playlistItems"),
        put: (data) => this.#put("playlistItems", data),
      },
      themes: {
        empty: () => undefined,
        get: () => this.#get("themes"),
        put: (data) => this.#put("themes", data),
      },
      tracks: {
        empty: () => undefined,
        get: () => this.#get("tracks"),
        put: (data) => this.#put("tracks", data),
      },
    });

    this.facets = this.#manager.facets;
    this.playlistItems = this.#manager.playlistItems;
    this.themes = this.#manager.themes;
    this.tracks = this.#manager.tracks;

    this.ready = () => true;
  }

  // LIFECYCLE

  /** @override */
  connectedCallback() {
    this.replicateSavedData(this.#manager);
    super.connectedCallback();
  }

  // GET & PUT

  /** @param {string} name */
  #get = (name) => IDB.get(`${IDB_PREFIX}/${this.#cat(name)}`);

  /** @param {string} name; @param {any} data */
  #put = (name, data) => IDB.set(`${IDB_PREFIX}/${this.#cat(name)}`, data);

  // 🛠️

  /** @param {string} name */
  #cat(name) {
    return `${this.namespace?.length ? this.namespace + "/" : ""}${name}`;
  }
}

export default IndexedDBOutput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = IndexedDBOutput;
export const NAME = "dop-indexed-db";

customElements.define(NAME, IndexedDBOutput);
