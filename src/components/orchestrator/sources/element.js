import { BroadcastableDiffuseElement, query } from "~/common/element.js";
import { groupTracksPerScheme } from "~/common/utils.js";
import { signal } from "~/common/signal.js";

/**
 * @import {InputElement, Source} from "~/components/input/types.d.ts"
 * @import {OutputElement} from "~/components/output/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class Sources extends BroadcastableDiffuseElement {
  static NAME = "diffuse/orchestrator/sources";

  // SIGNALS

  #sources = signal(/** @type {{ [scheme: string]: Source[] }} */ ({}));

  // STATE

  sources = this.#sources.get;

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();

    /** @type {InputElement} */
    const input = query(this, "input-selector");

    /** @type {OutputElement} */
    const output = query(this, "output-selector");

    // Wait until defined
    await customElements.whenDefined(input.localName);
    await customElements.whenDefined(output.localName);

    const singleInputMode = !!input.SCHEME;
    const deps =
      /** @type {{ [k: string]: InputElement }} */ (singleInputMode
        ? {}
        : input.dependencies());

    // Effects
    this.effect(() => {
      const tracks = output.tracks.collection();
      const groups = groupTracksPerScheme(tracks);

      /** @type {{ [scheme: string]: Source[] }} */
      const record = {};

      Object.entries(groups).map(([scheme, tracks]) => {
        /** @type {Source[]} */
        let sources;

        if (singleInputMode) {
          if (input.SCHEME === scheme) {
            sources = input.sources(tracks);
          } else {
            sources = [];
          }
        } else {
          const dep = deps[scheme];
          if (!dep) sources = [];
          else sources = dep.sources(tracks);
        }

        record[scheme] = sources;
      });

      this.#sources.value = record;
    });
  }
}

export default Sources;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = Sources;
export const NAME = "do-sources";

customElements.define(NAME, CLASS);
