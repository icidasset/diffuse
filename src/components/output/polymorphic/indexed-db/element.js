import { BroadcastableDiffuseElement } from "@common/element.js";
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
class IndexedDBOutput extends BroadcastableDiffuseElement {
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

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(
        `${this.nameWithGroup}${
          this.namespace.length ? "/" + this.namespace.replace(/\/$/, "") : ""
        }`,
        {
          put: { strategy: "replicate", fn: this.#putIncoming },
        },
      );

      if (actions) {
        this.#put = this.#putOutgoing(actions.put);
      }
    }

    // Super
    super.connectedCallback();
  }

  // GET & PUT

  /** @param {string} name */
  #getProxy = (name) => this.proxy.get({ name: this.#cat(name) });
  #get = this.#getProxy;

  /** @param {string} name; @param {any} data */
  #putProxy = (name, data) => this.proxy.put({ name: this.#cat(name), data });
  #put = this.#putProxy;

  /**
   * @param {(uuidSender: ReturnType<typeof crypto.randomUUID>, name: string, data: any) => Promise<void>} action
   * @returns {(name: string, data: any) => Promise<void>}
   */
  #putOutgoing = (action) => async (name, data) => {
    return await action(this.uuid, name, data);
  };

  /**
   * @param {ReturnType<typeof crypto.randomUUID>} uuidSender
   * @param {string} name
   * @param {any} data
   */
  #putIncoming(uuidSender, name, data) {
    if (uuidSender === this.uuid) {
      // Initiator
      this.#putProxy(name, data);
    } else {
      // Listener
      if (name === "facets") this.#manager.signals.facets.value = data;
      if (name === "playlistItems") {
        this.#manager.signals.playlistItems.value = data;
      }
      if (name === "themes") this.#manager.signals.themes.value = data;
      if (name === "tracks") this.#manager.signals.tracks.value = data;
    }
  }

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
