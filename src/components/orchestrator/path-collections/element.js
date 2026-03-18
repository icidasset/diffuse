import { computed } from "~/common/signal.js";
import { OutputTransformer } from "~/components/transformer/output/base.js";

/**
 * @import {PlaylistItem, Track} from "~/definitions/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class PathCollectionsOrchestrator extends OutputTransformer {
  static NAME = "diffuse/orchestrator/path-collections";

  constructor() {
    super();

    const base = this.base();

    const ephemeralItems = computed(() => {
      const col = base.tracks.collection();
      return createEphemeralItems(col.state === "loaded" ? col.data : []);
    });

    this.facets = base.facets;
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
}

/**
 * @param {Track[]} tracks
 * @returns {PlaylistItem[]}
 */
function createEphemeralItems(tracks) {
  /** @type {PlaylistItem[]} */
  const items = [];

  /** @type {Set<string>} */
  const segmentsAdded = new Set();

  tracks.forEach((track) => {
    const segment = pathSegment(track.uri);
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
 * @returns {string | undefined}
 */
function pathSegment(uri) {
  try {
    const url = new URL(uri);
    const segment = url.pathname.split("/").filter(Boolean)[0];
    return segment || undefined;
  } catch {
    return undefined;
  }
}

export default PathCollectionsOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = PathCollectionsOrchestrator;
export const NAME = "do-path-collections";

customElements.define(NAME, CLASS);
