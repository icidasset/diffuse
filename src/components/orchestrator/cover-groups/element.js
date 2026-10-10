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
    const provider = this.#provider.value;
    const groups = /** @type {any} */ (provider)?.groups?.();

    /** @type {{ label: string; groups: ArtistGroup[] }[]} */
    const result = [];

    if (groups?.length) {
      const allTracks = provider?.tracks() ?? [];

      // Total track counts per artist across all groups
      /** @type {Map<string, number>} */
      const totalCounts = new Map();
      for (const track of allTracks) {
        const key = String(track.tags?.artist ?? "").toLowerCase();
        totalCounts.set(key, (totalCounts.get(key) ?? 0) + 1);
      }

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
      const allTracks = provider?.tracks() ?? [];
      const artists = deduplicateArtists(allTracks);
      if (artists.length) result.push({ label: "", groups: artists });
    }

    return result;
  });

  coverGroups = computed(() => {
    const provider = this.#provider.value;
    const groups = /** @type {any} */ (provider)?.groups?.();

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
      const tracks = provider?.tracks() ?? [];
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
 * Most common casing variant of a group's name — tracks often disagree
 * on letter case ("Boys Noize" vs "boys noize"), and the label should
 * reflect what most of the library uses. Ties keep the first-seen
 * variant. Cheap by construction: the variant maps hold one entry in
 * the common case, so this is O(variants) after an O(1)-per-track count.
 * @param {Map<string, number>} variants raw name → track count
 * @returns {string}
 */
function mostCommonVariant(variants) {
  let best = "";
  let count = 0;

  for (const [name, n] of variants) {
    if (n > count) {
      count = n;
      best = name;
    }
  }

  return best;
}

/**
 * @param {Track[]} tracks
 * @returns {CoverGroup[]}
 */
function deduplicateAlbums(tracks) {
  /** @type {Map<string, { track: Track; artistVariants: Map<string, Map<string, number>>; nameVariants: Map<string, number> }>} */
  const albumMap = new Map();

  for (const track of tracks) {
    const albumKey = String(track.tags?.album ?? "").toLowerCase();
    const albumName = track.tags?.album ?? "Unknown album";
    const artist = track.tags?.artist ?? "Unknown artist";

    const entry = albumMap.get(albumKey) ?? {
      track,
      artistVariants: new Map(),
      nameVariants: new Map(),
    };
    albumMap.set(albumKey, entry);

    // Track per-artist casing variants, keyed case-insensitively so
    // "Boys Noize" / "boys noize" count as ONE artist (not "Various")
    const artistKey = artist.toLowerCase();
    const variants = entry.artistVariants.get(artistKey) ?? new Map();
    variants.set(artist, (variants.get(artist) ?? 0) + 1);
    entry.artistVariants.set(artistKey, variants);

    entry.nameVariants.set(
      albumName,
      (entry.nameVariants.get(albumName) ?? 0) + 1,
    );
  }

  return [...albumMap.entries()]
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([albumKey, { track, artistVariants, nameVariants }]) => ({
      albumKey,
      albumName: mostCommonVariant(nameVariants),
      artist: artistVariants.size > 1
        ? "Various Artists"
        : mostCommonVariant(
          /** @type {Map<string, number>} */
          (artistVariants.values().next().value),
        ),
      track,
    }));
}

/**
 * @param {Track[]} tracks
 * @returns {ArtistGroup[]}
 */
function deduplicateArtists(tracks) {
  /** @type {Map<string, { nameVariants: Map<string, number>; count: number; track: Track }>} */
  const map = new Map();

  for (const track of tracks) {
    const artistKey = String(track.tags?.artist ?? "").toLowerCase();
    const name = track.tags?.artist ?? "Unknown artist";

    const existing = map.get(artistKey);
    if (existing) {
      existing.count++;
      existing.nameVariants.set(
        name,
        (existing.nameVariants.get(name) ?? 0) + 1,
      );
    } else {
      map.set(artistKey, {
        nameVariants: new Map([[name, 1]]),
        count: 1,
        track,
      });
    }
  }

  return [...map.entries()]
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([artistKey, { nameVariants, count, track }]) => ({
      artistKey,
      artistName: mostCommonVariant(nameVariants),
      trackCount: count,
      track,
    }));
}

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = CoverGroupsOrchestrator;
export const NAME = "do-cover-groups";

defineElement(NAME, CLASS);
