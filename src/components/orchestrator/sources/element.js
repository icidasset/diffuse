import deepDiff from "@fry69/deep-diff";

import * as Output from "~/common/output.js";
import { BroadcastableDiffuseElement, defineElement, query } from "~/common/element.js";
import { groupTracksPerScheme } from "~/common/utils.js";
import { signal } from "~/common/signal.js";

import { DISABLED_KEY } from "./constants.js";

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
  #disabled = signal(/** @type {string[]} */ ([]));

  // STATE

  sources = this.#sources.get;
  disabled = this.#disabled.get;

  #output = signal(/** @type {OutputElement | null} */ (null));

  // METHODS

  /**
   * Returns whether the given source URI is disabled.
   * Strips query params before comparing, matching how {@link toggle} stores keys.
   *
   * @param {string} uri
   * @returns {boolean}
   */
  isDisabled(uri) {
    const q = uri.indexOf("?");
    const key = q === -1 ? uri : uri.slice(0, q);
    return this.#disabled.get().includes(key);
  }

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
    const existing = settings.find((s) => s.key === DISABLED_KEY);

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
        s.key === DISABLED_KEY ? { ...s, value } : s
      )
      : [
        ...settings,
        {
          $type: /** @type {"sh.diffuse.output.setting"} */ (
            "sh.diffuse.output.setting"
          ),
          id: crypto.randomUUID(),
          key: DISABLED_KEY,
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

    // Effects
    this.effect(() => {
      const col = output.settings.collection();
      if (col.state !== "loaded") { this.#disabled.value = []; return; }
      const setting = col.data.find((s) => s.key === DISABLED_KEY);
      if (!setting) { this.#disabled.value = []; return; }
      try {
        const parsed = JSON.parse(setting.value);
        this.#disabled.value = Array.isArray(parsed) ? parsed : [];
      } catch {
        this.#disabled.value = [];
      }
    });

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
