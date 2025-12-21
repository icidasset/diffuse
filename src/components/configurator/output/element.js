import { DiffuseElement } from "@common/element.js";
import { computed, signal } from "@common/signal.js";

/**
 * @import {Track} from "@definitions/types.d.ts"
 * @import {OutputManager, OutputElement} from "@components/output/types.d.ts"
 */

/**
 * @typedef {OutputElement<Track[]>} Output
 */

const STORAGE_PREFIX = "diffuse/configurator/output";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {OutputManager<Track[]>}
 */
class OutputConfigurator extends DiffuseElement {
  static NAME = "diffuse/configurator/output";

  constructor() {
    super();

    /** @type {OutputManager<Track[]>} */
    const manager = {
      tracks: {
        collection: computed(() => {
          const out = this.#selectedOutput.value;
          if (out) return out.tracks.collection();
          return this.#memory.tracks.value;
        }),
        reload: () => {
          const out = this.#selectedOutput.value;
          if (out) return out.tracks.reload();
          return Promise.resolve();
        },
        save: async (newTracks) => {
          const out = this.#selectedOutput.value;
          if (out) return await out.tracks.save(newTracks);
          this.#memory.tracks.value = newTracks;
        },
        state: computed(() => {
          const out = this.#selectedOutput.value;
          if (out) return out.tracks.state();
          return out === undefined ? "loading" : "loaded";
        }),
      },
    };

    // Assign manager properties to class
    this.tracks = manager.tracks;
  }

  // SIGNALS

  #memory = {
    tracks: signal(/** @type {Track[]} */ ([])),
  };

  #selectedOutput = signal(
    /** @type {Output | null | undefined} */ (undefined),
  );

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();
    this.#selectedOutput.value = await this.#findSelectedOutput();

    this.effect(() => {
      console.log("selectedOutput changed", this.#selectedOutput.value);
    });

    this.effect(() => {
      console.log("collection changed", this.tracks.collection());
    });

    this.effect(() => {
      console.log("state changed", this.tracks.state());
    });
  }

  // MISC

  async #findSelectedOutput() {
    const id = localStorage.getItem(`${STORAGE_PREFIX}/selected/id`);
    const el = id ? this.root().querySelector(`#${id}`) : null;

    if (!el) return null;

    await customElements.whenDefined(el.localName);

    if (
      "nameWithGroup" in el === false ||
      "tracks" in el === false
    ) {
      return null;
    }

    return /** @type {Output} */ (/** @type {unknown} */ (el));
  }

  /**
   * @override
   */
  dependencies() {
    return Object.fromEntries(
      Array.from(this.children).flatMap((element) => {
        if (element.hasAttribute("id") === false) {
          console.warn(
            "Missing `id` for output configurator child element with `localName` '" +
              element.localName + "'",
          );
          return [];
        }

        const d = /** @type {DiffuseElement} */ (element);
        return [[d.id, d]];
      }),
    );
  }

  // ADDITIONAL ACTIONS

  /**
   * @param {string} id
   */
  selectOutput(id) {
    localStorage.setItem(`${STORAGE_PREFIX}/selected/id`, id);
  }
}

export default OutputConfigurator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = OutputConfigurator;
export const NAME = "dc-output";

customElements.define(NAME, CLASS);
