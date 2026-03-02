import * as Automerge from "@automerge/automerge";
import { ifDefined } from "lit-html/directives/if-defined.js";
import { isUint8Array } from "iso-base/utils";

import "~/components/output/polymorphic/indexed-db/element.js";

import { computed, signal } from "~/common/signal.js";
import {
  recursivelyCloneRecords,
  removeUndefinedValuesFromRecord,
} from "~/common/utils.js";
import { OutputTransformer } from "../../base.js";
import {
  INITIAL_FACETS_DOCUMENT,
  INITIAL_PLAYLIST_ITEMS_DOCUMENT,
  INITIAL_THEMES_DOCUMENT,
  INITIAL_TRACKS_DOCUMENT,
} from "./constants.js";

/**
 * @import { RenderArg } from "~/common/element.d.ts"
 * @import { SignalReader } from "~/common/signal.d.ts";
 * @import { OutputElement } from "~/components/output/types.d.ts";
 */

/**
 * @extends {OutputTransformer<Uint8Array>}
 */
class AutomergeBytesOutputTransformer extends OutputTransformer {
  constructor() {
    super();

    const remote = this.base();
    const local = this.#localOutput.get;

    /**
     * @template T
     * @param {SignalReader<Uint8Array | undefined>} localCollection
     * @param {SignalReader<Uint8Array | undefined>} remoteCollection
     * @param {Automerge.Doc<T>} initial
     * @returns {SignalReader<{ doc: Automerge.Doc<T>; diverged: boolean; local: boolean; remote: boolean; }>}
     */
    const state = (localCollection, remoteCollection, initial) =>
      computed(() => {
        const l = loadDocument(localCollection);
        const r = remote.ready() ? loadDocument(remoteCollection) : undefined;

        if (!r) {
          return l
            ? { doc: l, diverged: true, local: false, remote: true }
            : { doc: initial, diverged: false, local: false, remote: false };
        } else if (!l) {
          return { doc: r, diverged: true, local: true, remote: false };
        }

        const lh = Automerge.getHeads(l)[0];
        const rh = Automerge.getHeads(r)[0];
        const diverged = lh !== rh;

        return {
          doc: diverged
            ? Automerge.merge(Automerge.clone(l), Automerge.clone(r))
            : r,
          diverged,
          local: Automerge.hasHeads(r, [lh]),
          remote: Automerge.hasHeads(l, [rh]),
        };
      });

    const facets = state(
      computed(() => local()?.facets?.collection()),
      remote.facets.collection,
      INITIAL_FACETS_DOCUMENT,
    );

    const playlistItems = state(
      computed(() => local()?.playlistItems?.collection()),
      remote.playlistItems.collection,
      INITIAL_PLAYLIST_ITEMS_DOCUMENT,
    );

    const themes = state(
      computed(() => local()?.themes?.collection()),
      remote.themes.collection,
      INITIAL_THEMES_DOCUMENT,
    );

    const tracks = state(
      computed(() => local()?.tracks?.collection()),
      remote.tracks.collection,
      INITIAL_TRACKS_DOCUMENT,
    );

    this.facets = automergeEntry(
      computed(() => local()?.facets),
      remote.facets,
      computed(() => facets().doc),
      {
        stripUndefined: true,
      },
    );

    this.playlistItems = automergeEntry(
      computed(() => local()?.playlistItems),
      remote.playlistItems,
      computed(() => playlistItems().doc),
    );

    this.themes = automergeEntry(
      computed(() => local()?.themes),
      remote.themes,
      computed(() => themes().doc),
      {
        stripUndefined: true,
      },
    );

    this.tracks = automergeEntry(
      computed(() => local()?.tracks),
      remote.tracks,
      computed(() => tracks().doc),
    );

    this.ready = () => true;

    // Effects
    this.effect(() => {
      const l = local();
      if (!l) return;

      this.effect(() => {
        if (remote.facets.state() !== "loaded") return;
        const s = facets();
        if (s.diverged) {
          const bytes = Automerge.save(s.doc);
          if (l && s.local) l.facets.save(bytes);
          if (s.remote) remote.facets.save(bytes);
        }
      });

      this.effect(() => {
        if (remote.playlistItems.state() !== "loaded") return;
        const s = playlistItems();
        if (s.diverged) {
          const bytes = Automerge.save(s.doc);
          if (l && s.local) l.playlistItems.save(bytes);
          if (s.remote) remote.playlistItems.save(bytes);
        }
      });

      this.effect(() => {
        if (remote.themes.state() !== "loaded") return;
        const s = themes();
        if (s.diverged) {
          const bytes = Automerge.save(s.doc);
          if (l && s.local) l.themes.save(bytes);
          if (s.remote) remote.themes.save(bytes);
        }
      });

      this.effect(() => {
        if (remote.tracks.state() !== "loaded") return;
        const s = tracks();
        if (s.diverged) {
          const bytes = Automerge.save(s.doc);
          if (l && s.local) l.tracks.save(bytes);
          if (s.remote) remote.tracks.save(bytes);
        }
      });
    });
  }

  // SIGNALS

  #localOutput = signal(
    /** @type {OutputElement<Uint8Array | undefined> | undefined} */ (undefined),
  );

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    /** @type {OutputElement<Uint8Array | undefined> | null} */
    const local = this.root().querySelector("dop-indexed-db");
    if (!local) throw new Error("Can't find local output");

    // When defined
    customElements.whenDefined(local.localName).then(() => {
      this.#localOutput.value = local;
    });
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    return html`
      <dop-indexed-db
        namespace="${ifDefined(this.getAttribute(`namespace`))}"
      ></dop-indexed-db>
    `;
  }
}

export default AutomergeBytesOutputTransformer;

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

/**
 * @template T
 * @param {SignalReader<Uint8Array | undefined>} source
 * @returns {Automerge.Doc<T> | undefined}
 */
export function loadDocument(source) {
  const value = source();

  if (isUint8Array(value)) {
    return Automerge.load(value);
  } else if (value == undefined) {
    return undefined;
  } else {
    throw new Error("Invalid data type");
  }
}

/**
 * @template {Record<string, any>} T
 * @param {SignalReader<{ collection: SignalReader<Uint8Array | undefined>, reload: () => Promise<void>, save: (bytes: Uint8Array) => Promise<void>, state: SignalReader<"loading" | "loaded" | "sleeping"> } | undefined>} local
 * @param {{ collection: SignalReader<Uint8Array | undefined>, reload: () => Promise<void>, save: (bytes: Uint8Array) => Promise<void>, state: SignalReader<"loading" | "loaded" | "sleeping"> }} remote
 * @param {SignalReader<Automerge.Doc<{ collection: T[] }>>} document
 * @param {{ stripUndefined?: boolean }} [opts]
 * @returns {{ collection: SignalReader<T[]>, reload: () => Promise<void>, save: (items: T[]) => Promise<void>, state: SignalReader<"loading" | "loaded" | "sleeping"> }}
 */
export function automergeEntry(local, remote, document, opts) {
  return {
    collection: computed(() => document().collection),
    reload: remote.reload,
    save: async (/** @type {T[]} */ newItems) => {
      const doc = Automerge.change(document(), (d) => {
        d.collection = newItems.map((item) => {
          const cloned = recursivelyCloneRecords(item);
          return opts?.stripUndefined
            ? removeUndefinedValuesFromRecord(cloned)
            : cloned;
        });
      });

      const bytes = Automerge.save(doc);
      await local()?.save(bytes);
    },
    state: computed(() => local()?.state() ?? "sleeping"),
  };
}

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = AutomergeBytesOutputTransformer;
export const NAME = "dtob-automerge";

customElements.define(NAME, CLASS);
