import { DiffuseElement } from "@common/element.js";
import { SCHEME } from "./constants.js";
import { computed, signal } from "@common/signal.js";
import { listen } from "@common/worker.js";

/**
 * @import {InputActions, InputSchemeProvider} from "@components/input/types.d.ts"
 * @import {ProxiedActions} from "@common/worker.d.ts"
 *
 * @import {Bucket, State} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<InputActions>}
 * @implements {InputSchemeProvider}
 */
class S3Input extends DiffuseElement {
  static NAME = "diffuse/input/s3";
  static WORKER_URL = "components/input/s3/worker.js";

  SCHEME = SCHEME;

  constructor() {
    super();

    /** @type {ProxiedActions<InputActions & State & { demo: () => Promise<void> }>} */
    this.proxy = this.workerProxy();

    this.consult = this.proxy.consult;
    this.contextualize = this.proxy.contextualize;
    this.groupConsult = this.proxy.groupConsult;
    this.list = this.proxy.list;
    this.resolve = this.proxy.resolve;

    this.demo = this.proxy.demo;
  }

  // SIGNALS

  #buckets = signal(/** @type {Record<string, Bucket>} */ ({}));

  // STATE

  buckets = this.#buckets.get;

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    // Sync data with worker
    const link = this.workerLink();

    // Listen for remote data changes
    listen("buckets", this.#buckets.set, link);

    // Fetch current data state
    this.proxy.buckets().then(this.#buckets.set);
  }

  // 🛠️

  bucketList = computed(() => {
    const buckets = this.#buckets.value;

    return Object.values(buckets).map((bucket) => {
      return {
        label: `${bucket.bucketName} (${bucket.accessKey}, ${bucket.host})`,
        bucket,
      };
    });
  });
}

export default S3Input;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = S3Input;
export const NAME = "di-s3";

customElements.define(NAME, CLASS);
