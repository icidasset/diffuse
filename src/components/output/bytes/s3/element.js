import * as IDB from "idb-keyval";

import { BroadcastableDiffuseElement } from "@common/element.js";
import { computed, signal } from "@common/signal.js";
import { outputManager } from "../../common.js";

const STORAGE_PREFIX = "diffuse/output/bytes/s3";

/**
 * @import {ProxiedActions} from "@common/worker.d.ts"
 * @import {OutputElement, OutputManager} from "../../types.d.ts"
 * @import {Bucket} from "@components/input/s3/types.d.ts"
 * @import {S3OutputElement, S3OutputWorkerActions} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {OutputElement<Uint8Array | undefined>}
 * @implements {S3OutputElement}
 */
class S3Output extends BroadcastableDiffuseElement {
  static NAME = "diffuse/output/bytes/s3";
  static WORKER_URL = "components/output/bytes/s3/worker.js";

  #manager;

  constructor() {
    super();

    /** @type {ProxiedActions<S3OutputWorkerActions>} */
    this.proxy = this.workerProxy();

    /** @type {OutputManager<Uint8Array | undefined>} */
    this.#manager = outputManager({
      facets: {
        empty: () => undefined,
        get: () => this.#get("facets"),
        put: (data) => this.#put("facets", data),
      },
      init: () => this.whenConnected(),
      playlists: {
        empty: () => undefined,
        get: () => this.#get("playlists"),
        put: (data) => this.#put("playlists", data),
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
    this.playlists = this.#manager.playlists;
    this.themes = this.#manager.themes;
    this.tracks = this.#manager.tracks;
  }

  // STATE

  ready = computed(() => {
    return this.#bucket.value !== undefined;
  });

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      // TODO: Get via leader?
      const actions = this.broadcast(this.nameWithGroup, {
        put: { strategy: "replicate", fn: this.#putIncoming },
      });

      if (actions) {
        this.#put = this.#putOutgoing(actions.put);
      }
    }

    // Super
    super.connectedCallback();

    /** @type {Bucket | undefined} */
    const stored = await IDB.get(`${STORAGE_PREFIX}/bucket`);
    if (stored) this.#bucket.value = stored;
  }

  // BUCKET

  #bucket = signal(/** @type {Bucket | undefined} */ (undefined));

  /** @returns {Promise<Bucket | undefined>} */
  async bucket() {
    if (!this.#bucket.value) {
      /** @type {Bucket | undefined} */
      const stored = await IDB.get(`${STORAGE_PREFIX}/bucket`);
      if (stored) this.#bucket.value = stored;
      return stored;
    }

    return this.#bucket.value;
  }

  /**
   * @param {Bucket} bucket
   */
  async setBucket(bucket) {
    this.#bucket.value = bucket;
    await IDB.set(`${STORAGE_PREFIX}/bucket`, bucket);
  }

  // GET & PUT

  /** @param {string} name */
  #getProxy = async (name) => {
    const bucket = await this.bucket();
    if (!bucket) return undefined;
    return this.proxy.get({ bucket, name: this.#cat(name) });
  };

  #get = this.#getProxy;

  /** @param {string} name; @param {any} data */
  #putProxy = async (name, data) => {
    const bucket = await this.bucket();
    if (!bucket) return undefined;
    return this.proxy.put({ bucket, data, name: this.#cat(name) });
  };

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
      if (name === "playlists") this.#manager.signals.playlists.value = data;
      if (name === "themes") this.#manager.signals.themes.value = data;
      if (name === "tracks") this.#manager.signals.tracks.value = data;
    }
  }

  // 🛠️

  /** @param {string} name */
  #cat(name) {
    const namespace = this.hasAttribute("namespace")
      ? this.getAttribute("namespace") + "/"
      : "";
    return `${namespace}${name}`;
  }
}

export default S3Output;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = S3Output;
export const NAME = "dob-s3";

customElements.define(NAME, S3Output);
