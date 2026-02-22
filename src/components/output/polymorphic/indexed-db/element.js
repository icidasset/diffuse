import { DiffuseElement } from "@common/element.js";
import { outputManager } from "../../common.js";

/**
 * @import {ProxiedActions} from "@common/worker.d.ts"
 * @import {OutputElement, OutputManager, OutputWorkerActions} from "../../types.d.ts"
 * @import {SupportedDataTypes} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {OutputElement<SupportedDataTypes>}
 */
class IndexedDBOutput extends DiffuseElement {
  static NAME = "diffuse/output/polymorphic/indexed-db";
  static WORKER_URL = "components/output/polymorphic/indexed-db/worker.js";

  #manager;

  constructor() {
    super();

    /** @type {ProxiedActions<OutputWorkerActions<SupportedDataTypes>>} */
    this.proxy = this.workerProxy();

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

  // GET & PUT

  /** @param {string} name */
  #get = (name) => this.proxy.get({ name: this.#cat(name) });

  /** @param {string} name; @param {any} data */
  #put = (name, data) => this.proxy.put({ name: this.#cat(name), data });

  // 🛠️

  get namespace() {
    return this.hasAttribute("namespace")
      ? this.getAttribute("namespace") + "/"
      : "";
  }

  /** @param {string} name */
  #cat(name) {
    return `${this.namespace}${name}`;
  }
}

export default IndexedDBOutput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = IndexedDBOutput;
export const NAME = "dop-indexed-db";

customElements.define(NAME, IndexedDBOutput);
