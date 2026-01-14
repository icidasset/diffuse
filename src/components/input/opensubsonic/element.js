import { DiffuseElement } from "@common/element.js";
import { computed, signal } from "@common/signal.js";
import { listen } from "@common/worker.js";
import { SCHEME } from "./constants.js";

/**
 * @import {InputActions, InputSchemeProvider} from "@components/input/types.d.ts"
 * @import {ProxiedActions} from "@common/worker.d.ts"
 *
 * @import {Server, State} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ProxiedActions<InputActions>}
 * @implements {InputSchemeProvider}
 */
class OpensubsonicInput extends DiffuseElement {
  static NAME = "diffuse/input/opensubsonic";
  static WORKER_URL = "components/input/opensubsonic/worker.js";

  SCHEME = SCHEME;

  constructor() {
    super();

    /** @type {ProxiedActions<InputActions & State>} */
    this.proxy = this.workerProxy();

    this.consult = this.proxy.consult;
    this.contextualize = this.proxy.contextualize;
    this.groupConsult = this.proxy.groupConsult;
    this.list = this.proxy.list;
    this.resolve = this.proxy.resolve;
  }

  // SIGNALS

  #servers = signal(/** @type {Record<string, Server>} */ ({}));

  // STATE

  servers = this.#servers.get;

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    // Sync data with worker
    const link = this.workerLink();

    // Listen for remote data changes
    listen("servers", this.#servers.set, link);

    // Fetch current data state
    this.proxy.servers().then(this.#servers.set);
  }

  // 🛠️

  serverList = computed(() => {
    const servers = this.#servers.value;

    return Object.values(servers).map((server) => {
      return {
        label: `${server.host} (${server.username ?? server.apiKey})`,
        server,
      };
    });
  });
}

export default OpensubsonicInput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = OpensubsonicInput;
export const NAME = "di-opensubsonic";

customElements.define(NAME, OpensubsonicInput);
