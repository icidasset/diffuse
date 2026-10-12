import { computed, signal } from "~/common/signal.js";
import { OutputTransformer } from "~/components/transformer/output/base.js";
import { defineElement } from "~/common/element.js";

/**
 * @import {PlaylistItem, Track} from "~/definitions/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class PathCollectionsOrchestrator extends OutputTransformer {
  static NAME = "diffuse/orchestrator/path-collections";

  static observedAttributes = ["path-part"];

  /** 1-based path part of each track's URI to group playlists by. Defaults to 1. */
  #pathPart = signal(1);

  constructor() {
    super();

    const base = this.base();

    const ephemeralItems = computed(() => {
      const col = base.tracks.collection();
      return createEphemeralItems(
        col.state === "loaded" ? col.data : [],
        this.#pathPart.get(),
      );
    });

    this.facets = base.facets;
    this.settings = base.settings;
    this.playlistItems = {
      ...base.playlistItems,
      collection: computed(() => {
        const col = base.playlistItems.collection();
        if (col.state !== "loaded") return col;
        return { state: "loaded", data: [...col.data, ...ephemeralItems()] };
      }),
      /** @type {(typeof base.playlistItems.save)} */
      save: async (items) => {
        await base.playlistItems.save(
          (items || []).filter((p) => !p.ephemeral),
        );
      },
    };
    this.tracks = base.tracks;

    this.ready = base.ready;
  }

  /**
   * @override
   * @param {string} name
   * @param {string} oldValue
   * @param {string} newValue
   */
  attributeChangedCallback(name, oldValue, newValue) {
    super.attributeChangedCallback(name, oldValue, newValue);

    if (name === "path-part") this.#pathPart.set(parsePathPart(newValue));
  }
}

/**
 * @param {Track[]} tracks
 * @param {number} pathPart
 * @returns {PlaylistItem[]}
 */
function createEphemeralItems(tracks, pathPart) {
  /** @type {PlaylistItem[]} */
  const items = [];

  /** @type {Set<string>} */
  const segmentsAdded = new Set();

  tracks.forEach((track) => {
    const segment = pathSegment(track.uri, pathPart);
    if (!segment) return;

    if (segmentsAdded.has(segment)) return;

    /** @type {PlaylistItem} */
    const item = {
      $type: "sh.diffuse.output.playlistItem",
      id: `path-collections/${track.id}`,
      playlist: segment,
      criteria: [{ field: "uri", value: /** @type {any} */ (track.uri) }],
      ephemeral: true,
    };

    items.push(item);
    segmentsAdded.add(segment);
  });

  return items;
}

/**
 * @param {string} uri
 * @param {number} pathPart
 * @returns {string | undefined}
 */
function pathSegment(uri, pathPart) {
  try {
    const url = new URL(uri);
    const segment = url.pathname.split("/").filter(Boolean)[pathPart - 1];

    if (!segment) return undefined;

    try {
      return decodeURIComponent(segment);
    } catch {
      return segment;
    }
  } catch {
    return undefined;
  }
}

/**
 * @param {string | null} value
 * @returns {number}
 */
function parsePathPart(value) {
  const parsed = Number.parseInt(value ?? "", 10);
  return Number.isNaN(parsed) || parsed < 1 ? 1 : parsed;
}

export default PathCollectionsOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = PathCollectionsOrchestrator;
export const NAME = "do-path-collections";

defineElement(NAME, CLASS);
