import * as IDB from "idb-keyval";

import { DiffuseElement } from "@common/element.js";
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
class S3Output extends DiffuseElement {
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
  }

  // SIGNALS

  #isOnline = signal(navigator.onLine);

  // STATE

  ready = computed(() => {
    return this.#bucket.value !== undefined && this.#isOnline.value;
  });

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();

    /** @type {Bucket | undefined} */
    const stored = await IDB.get(`${STORAGE_PREFIX}/bucket`);
    if (stored) this.#bucket.value = stored;

    globalThis.addEventListener("online", this.#online);
    globalThis.addEventListener("offline", this.#offline);
  }

  /** @override */
  disconnectedCallback() {
    globalThis.removeEventListener("online", this.#online);
    globalThis.removeEventListener("offline", this.#offline);
  }

  #offline = () => this.#isOnline.set(false);
  #online = () => this.#isOnline.set(true);

  // BUCKET

  #bucket = signal(/** @type {Bucket | undefined} */ (undefined));

  bucket = this.#bucket.get;

  /** @returns {Promise<Bucket | undefined>} */
  async getBucket() {
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

  async unsetBucket() {
    this.#bucket.value = undefined;
    await IDB.del(`${STORAGE_PREFIX}/bucket`);
  }

  // GET & PUT

  /** @param {string} name */
  #get = async (name) => {
    const bucket = await this.getBucket();
    if (!bucket) return undefined;
    return this.proxy.get({ bucket, name: this.#cat(name) });
  };

  /** @param {string} name; @param {any} data */
  #put = async (name, data) => {
    const bucket = await this.getBucket();
    if (!bucket) return undefined;
    return this.proxy.put({ bucket, data, name: this.#cat(name) });
  };

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
