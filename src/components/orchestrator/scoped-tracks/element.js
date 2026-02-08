import {
  BroadcastableDiffuseElement,
  query,
  queryOptional,
} from "@common/element.js";
import { signal, untracked } from "@common/signal.js";

/**
 * @import {Track} from "@definitions/types.d.ts"
 * @import {ProxiedActions} from "@common/worker.d.ts"
 * @import {InputElement} from "@components/input/types.d.ts"
 * @import {OutputElement} from "@components/output/types.d.ts"
 *
 * @import {Actions} from "./types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class ScopedTracksOrchestrator extends BroadcastableDiffuseElement {
  static NAME = "diffuse/orchestrator/scoped-tracks";
  static WORKER_URL = "components/orchestrator/scoped-tracks/worker.js";

  /** @type {ProxiedActions<Actions>} */
  #proxy;

  constructor() {
    super();
    this.#proxy = this.workerProxy({
      forceNew: {
        dependencies: {
          input: true,
        },
      },
    });
  }

  // SIGNALS

  #input = signal(/** @type {InputElement | null} */ (null));
  #output = signal(/** @type {OutputElement | null} */ (null));

  #search = signal(
    /** @type {import("@components/processor/search/element.js").CLASS | null} */ (null),
  );

  #scope = signal(
    /** @type {import("@components/engine/scope/element.js").CLASS | null} */ (null),
  );

  #tracks = signal(/** @type {Track[]} */ ([]));

  // STATE

  tracks = this.#tracks.get;

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      this.broadcast(this.nameWithGroup, {});
    }

    // Super
    super.connectedCallback();

    /** @type {InputElement} */
    const input = query(this, "input-selector");

    /** @type {OutputElement} */
    const output = query(this, "output-selector");

    /** @type {import("@components/processor/search/element.js").CLASS} */
    const search = query(this, "search-processor-selector");

    /** @type {import("@components/engine/scope/element.js").CLASS | null} */
    const scope = queryOptional(this, "scope-engine-selector");

    // Assign to self
    this.#input.value = input;
    this.#output.value = output;
    this.#search.value = search;
    if (scope) this.#scope.value = scope;

    // When defined
    await customElements.whenDefined(output.localName);
    if (scope) await customElements.whenDefined(scope.localName);

    // Watch tracks collection
    this.effect(async () => {
      const collection = output.tracks.collection();
      if ((await this.isLeader()) === false) return;
      this.#proxy.supplyAvailable(collection);
    });

    // Watch search supply
    this.effect(async () => {
      const _trigger = search.supplyFingerprint();
      const searchTerm = this.#scope.value?.searchTerm();

      if ((await this.isLeader()) === false) return;

      const searchResults = searchTerm
        ? await this.#search.value?.search({ term: searchTerm })
        : untracked(() => output.tracks.collection());

      // TODO: Playlist selection
      this.#tracks.value = searchResults ?? [];
    });
  }

  // WORKERS

  /**
   * @override
   */
  dependencies() {
    if (!this.#input.value) throw new Error("Input element not defined yet");
    if (!this.#search.value) throw new Error("Search element not defined yet");

    return {
      input: this.#input.value,
      search: this.#search.value,
    };
  }
}

export default ScopedTracksOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ScopedTracksOrchestrator;
export const NAME = "do-scoped-tracks";

customElements.define(NAME, CLASS);
