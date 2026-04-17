import { defineElement, DiffuseElement, query } from "~/common/element.js";
import { computed, signal } from "~/common/signal.js";

/**
 * @import {SignalReader} from "~/common/signal.d.ts"
 * @import {Track} from "~/definitions/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class CoverGroupsOrchestrator extends DiffuseElement {
  static NAME = "diffuse/orchestrator/cover-groups";

  // SIGNALS

  #provider = signal(
    /** @type {DiffuseElement & { tracks: SignalReader<Track[]> } | null} */ (null),
  );

  // STATE

  artistGroups = computed(() => {
    const groups = /** @type {any} */ (this.#provider.value)?.groups?.();
    const allTracks = this.#provider.value?.tracks() ?? [];

    // Total track counts per artist across all groups
    /** @type {Map<string, number>} */
    const totalCounts = new Map();
    for (const track of allTracks) {
      const key = String(track.tags?.artist ?? "").toLowerCase();
      totalCounts.set(key, (totalCounts.get(key) ?? 0) + 1);
    }

    /** @type {{ label: string; groups: ArtistGroup[] }[]} */
    const result = [];

    if (groups?.length) {
      for (
        const group
          of /** @type {{ label: string; tracks: Track[] }[]} */ (groups)
      ) {
        const artists = deduplicateArtists(group.tracks).map((a) => ({
          ...a,
          trackCount: totalCounts.get(a.artistKey) ?? a.trackCount,
        }));
        if (artists.length) result.push({ label: group.label, groups: artists });
      }
    } else {
      const artists = deduplicateArtists(allTracks);
      if (artists.length) result.push({ label: "", groups: artists });
    }

    return result;
  });

  coverGroups = computed(() => {
    const groups = /** @type {any} */ (this.#provider.value)?.groups?.();

    /** @type {{ label: string; groups: CoverGroup[] }[]} */
    const result = [];

    if (groups?.length) {
      for (
        const group
          of /** @type {{ label: string; tracks: Track[] }[]} */ (groups)
      ) {
        const albums = deduplicateAlbums(group.tracks);
        if (albums.length) result.push({ label: group.label, groups: albums });
      }
    } else {
      const tracks = this.#provider.value?.tracks() ?? [];
      const albums = deduplicateAlbums(tracks);
      if (albums.length) result.push({ label: "", groups: albums });
    }

    return result;
  });

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();

    /** @type {DiffuseElement & { tracks: SignalReader<Track[]> }} */
    const provider = query(this, "tracks-selector");

    await customElements.whenDefined(provider.localName);
    this.#provider.value = provider;
  }
}

export default CoverGroupsOrchestrator;

////////////////////////////////////////////
// HELPERS
////////////////////////////////////////////

/**
 * @typedef {{ albumKey: string; albumName: string; artist: string; track: Track }} CoverGroup
 */

/**
 * @typedef {{ artistKey: string; artistName: string; trackCount: number; track: Track }} ArtistGroup
 */

/**
 * @param {Track[]} tracks
 * @returns {CoverGroup[]}
 */
function deduplicateAlbums(tracks) {
  const sorted = [...tracks].sort((a, b) => {
    const aAlbum = String(a.tags?.album ?? "").toLowerCase();
    const bAlbum = String(b.tags?.album ?? "").toLowerCase();
    return aAlbum.localeCompare(bAlbum);
  });

  /** @type {Map<string, { track: Track; artists: Set<string> }>} */
  const albumMap = new Map();

  for (const track of sorted) {
    const albumKey = String(track.tags?.album ?? "").toLowerCase();
    const existing = albumMap.get(albumKey);
    if (existing) {
      existing.artists.add(track.tags?.artist ?? "Unknown artist");
    } else {
      albumMap.set(albumKey, {
        track,
        artists: new Set([track.tags?.artist ?? "Unknown artist"]),
      });
    }
  }

  return [...albumMap.entries()].map(([albumKey, { track, artists }]) => ({
    albumKey,
    albumName: track.tags?.album ?? "Unknown album",
    artist: artists.size > 1 ? "Various Artists" : /** @type {string} */ ([...artists][0]),
    track,
  }));
}

/**
 * @param {Track[]} tracks
 * @returns {ArtistGroup[]}
 */
function deduplicateArtists(tracks) {
  /** @type {Map<string, { artistName: string; tracks: Track[] }>} */
  const map = new Map();

  for (const track of tracks) {
    const artistKey = String(track.tags?.artist ?? "").toLowerCase();
    const existing = map.get(artistKey);
    if (existing) {
      existing.tracks.push(track);
    } else {
      map.set(artistKey, {
        artistName: track.tags?.artist ?? "Unknown artist",
        tracks: [track],
      });
    }
  }

  return [...map.entries()]
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([artistKey, { artistName, tracks }]) => ({
      artistKey,
      artistName,
      trackCount: tracks.length,
      track: tracks[0],
    }));
}

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = CoverGroupsOrchestrator;
export const NAME = "do-cover-groups";

defineElement(NAME, CLASS);
