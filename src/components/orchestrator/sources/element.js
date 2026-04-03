import deepDiff from "@fry69/deep-diff";

import * as Output from "~/common/output.js";
import { BroadcastableDiffuseElement, defineElement, query } from "~/common/element.js";
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
  static DISABLED_KEY = "sh.diffuse.input.disabled.uris";

  // SIGNALS

  #sources = signal(/** @type {{ [scheme: string]: Source[] }} */ ({}));

  // STATE

  sources = this.#sources.get;

  #output = signal(/** @type {OutputElement | null} */ (null));

  // METHODS

  /**
   * @param {string} uri
   */
  async toggle(uri) {
    const q = uri.indexOf("?");
    const key = q === -1 ? uri : uri.slice(0, q);

    const output = this.#output.value;
    if (!output) {
      console.warn("Output element is not available yet.");
      return;
    }

    const settings = await Output.data(output.settings);
    const existing = settings.find((s) => s.key === Sources.DISABLED_KEY);

    /** @type {string[]} */
    let disabled = [];
    if (existing) {
      try {
        const parsed = JSON.parse(existing.value);
        disabled = Array.isArray(parsed) ? parsed : [];
      } catch {
        disabled = [];
      }
    }

    if (disabled.includes(key)) {
      disabled = disabled.filter((u) => u !== key);
    } else {
      disabled = [...disabled, key];
    }

    const value = JSON.stringify(disabled);
    const updated = existing
      ? settings.map((s) =>
        s.key === Sources.DISABLED_KEY ? { ...s, value } : s
      )
      : [
        ...settings,
        {
          $type: /** @type {"sh.diffuse.output.setting"} */ (
            "sh.diffuse.output.setting"
          ),
          id: crypto.randomUUID(),
          key: Sources.DISABLED_KEY,
          value,
        },
      ];

    await output.settings.save(updated);
  }

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

    // Signals
    this.#output.value = output;

    // Single input mode + dependencies
    const singleInputMode = !!input.SCHEME;
    const deps =
      /** @type {{ [k: string]: InputElement }} */ (singleInputMode
        ? {}
        : input.dependencies());

    // Effects
    this.effect(() => {
      const col = output.tracks.collection();
      const tracks = col.state === "loaded" ? col.data : [];
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
          if (!dep) sources = tracks.map((t) => ({ label: t.uri, uri: t.uri }));
          else sources = dep.sources(tracks);
        }

        record[scheme] = sources;
      });

      if (deepDiff(this.#sources.value, record)) {
        this.#sources.value = record;
      }
    });
  }
}

export default Sources;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = Sources;
export const NAME = "do-sources";

defineElement(NAME, CLASS);
