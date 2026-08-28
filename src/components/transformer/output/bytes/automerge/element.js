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
import { defineElement } from "~/common/element.js";
import {
  INITIAL_FACETS_DOCUMENT,
  INITIAL_PLAYLIST_ITEMS_DOCUMENT,
  INITIAL_SETTINGS_DOCUMENT,
  INITIAL_TRACKS_DOCUMENT,
} from "./constants.js";

/**
 * @import { RenderArg } from "~/common/element.d.ts"
 * @import { SignalReader } from "~/common/signal.d.ts";
 * @import { OutputElement } from "@specs/components/output/types.d.ts";
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
     * @param {SignalReader<{ state: "loading" } | { state: "loaded"; data: Uint8Array | undefined } | { state: "error" }>} localCollection
     * @param {SignalReader<{ state: "loading" } | { state: "loaded"; data: Uint8Array | undefined } | { state: "error" }>} remoteCollection
     * @param {Automerge.Doc<T>} initial
     * @returns {SignalReader<{ doc: Automerge.Doc<T>; diverged: boolean; local: boolean; remote: boolean; remoteLoaded: boolean; }>}
     */
    const state = (localCollection, remoteCollection, initial) =>
      computed(() => {
        const lc = localCollection();
        const rc = remote.ready() ? remoteCollection() : undefined;

        const l = loadDocument(lc?.state === "loaded" ? lc.data : undefined);
        const r = rc?.state === "loaded" ? loadDocument(rc.data) : undefined;
        const remoteLoaded = rc?.state === "loaded";

        if (!r) {
          return l
            ? {
              doc: l,
              diverged: true,
              local: false,
              remote: true,
              remoteLoaded,
            }
            : {
              doc: initial,
              diverged: false,
              local: false,
              remote: false,
              remoteLoaded,
            };
        } else if (!l) {
          return {
            doc: r,
            diverged: true,
            local: true,
            remote: false,
            remoteLoaded,
          };
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
          remoteLoaded,
        };
      });

    const facets = state(
      computed(() => local()?.facets?.collection() ?? { state: "loading" }),
      remote.facets.collection,
      INITIAL_FACETS_DOCUMENT,
    );

    const playlistItems = state(
      computed(() =>
        local()?.playlistItems?.collection() ?? { state: "loading" }
      ),
      remote.playlistItems.collection,
      INITIAL_PLAYLIST_ITEMS_DOCUMENT,
    );

    const settings = state(
      computed(() => local()?.settings?.collection() ?? { state: "loading" }),
      remote.settings.collection,
      INITIAL_SETTINGS_DOCUMENT,
    );

    const tracks = state(
      computed(() => local()?.tracks?.collection() ?? { state: "loading" }),
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

    this.settings = automergeEntry(
      computed(() => local()?.settings),
      remote.settings,
      computed(() => settings().doc),
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
        if (!facets().remoteLoaded) return;
        const s = facets();
        if (s.diverged) {
          const bytes = Automerge.save(s.doc);
          if (l && s.local) l.facets.save(bytes);
          if (s.remote) remote.facets.save(bytes);
        }
      });

      this.effect(() => {
        if (!playlistItems().remoteLoaded) return;
        const s = playlistItems();
        if (s.diverged) {
          const bytes = Automerge.save(s.doc);
          if (l && s.local) l.playlistItems.save(bytes);
          if (s.remote) remote.playlistItems.save(bytes);
        }
      });

      this.effect(() => {
        if (!settings().remoteLoaded) return;
        const s = settings();
        if (s.diverged) {
          const bytes = Automerge.save(s.doc);
          if (l && s.local) l.settings.save(bytes);
          if (s.remote) remote.settings.save(bytes);
        }
      });

      this.effect(() => {
        if (!tracks().remoteLoaded) return;
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
 * @param {Uint8Array | undefined} value
 * @returns {Automerge.Doc<T> | undefined}
 */
export function loadDocument(value) {
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
 * @param {SignalReader<{ collection: SignalReader<{ state: "loading" } | { state: "loaded"; data: Uint8Array | undefined } | { state: "error" }>, reload: () => Promise<void>, save: (bytes: Uint8Array) => Promise<void> } | undefined>} local
 * @param {{ collection: SignalReader<{ state: "loading" } | { state: "loaded"; data: Uint8Array | undefined } | { state: "error" }>, reload: () => Promise<void>, save: (bytes: Uint8Array) => Promise<void> }} remote
 * @param {SignalReader<Automerge.Doc<{ collection: T[] }>>} document
 * @param {{ stripUndefined?: boolean }} [opts]
 * @returns {{ collection: SignalReader<{ state: "loading" } | { state: "loaded"; data: T[] } | { state: "error" }>, reload: () => Promise<void>, save: (items: T[]) => Promise<void> }}
 */
export function automergeEntry(local, remote, document, opts) {
  return {
    collection: computed(() => {
      const col = local()?.collection();
      if (!col || col.state !== "loaded") {
        return { state: col?.state ?? "loading" };
      }
      return { state: "loaded", data: document().collection };
    }),
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
  };
}

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = AutomergeBytesOutputTransformer;
export const NAME = "dtob-automerge";

defineElement(NAME, CLASS);
