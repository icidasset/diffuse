import * as IDB from "idb-keyval";

import { IDB_PREFIX } from "./constants.js";
import { BroadcastedOutputElement, outputManager } from "../../common.js";
import { isSelfDescribing } from "~/common/self-describing.js";
import { decodeCollection, encodeCollection } from "~/common/lens.js";
import { defineElement } from "~/common/element.js";

/**
 * @import {OutputElement, OutputManager, OutputWorkerActions} from "@specs/components/output/types.d.ts"
 * @import {SupportedDataTypes} from "@specs/components/output/polymorphic/indexed-db/types.d.ts"
 * @import {CollectionName} from "~/common/self-describing.js"
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
      settings: {
        empty: () => undefined,
        get: () => this.#get("settings"),
        put: (data) => this.#put("settings", data),
      },
      tracks: {
        empty: () => undefined,
        get: () => this.#get("tracks"),
        put: (data) => this.#put("tracks", data),
      },
    });

    this.facets = this.#manager.facets;
    this.playlistItems = this.#manager.playlistItems;
    this.settings = this.#manager.settings;
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
  #get = async (name) => {
    const stored = await IDB.get(`${IDB_PREFIX}/${this.#cat(name)}`);
    if (stored === undefined) return undefined;
    return decodeCollection(
      stored,
      /** @type {CollectionName} */ (name),
    ) ?? undefined;
  };

  /** @param {string} name; @param {any} data */
  #put = async (name, data) => {
    // Bytes/strings produced by a transformer above (bytes/json or string/json
    // envelope bytes/JSON, or automerge/dasl binary) are already self-describing
    // at that layer, so they pass through as-is. Plain collections (arrays) are
    // wrapped so a standalone IndexedDB output is itself self-describing.
    const stored = data instanceof Uint8Array || typeof data === "string" ||
        isSelfDescribing(data)
      ? data
      : encodeCollection(data, /** @type {CollectionName} */ (name));
    await IDB.set(`${IDB_PREFIX}/${this.#cat(name)}`, stored);
  };

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

defineElement(NAME, IndexedDBOutput);
