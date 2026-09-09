import foundation from "~/common/foundation.js";
import { computed, effect } from "~/common/signal.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 */

// Marker used to avoid wrapping the same orchestrator instance more than once.
const PATCHED = Symbol("diffuse.deduplicate-tracks.patched");

/**
 * Returns a normalised key for a track's artist + title, or `null` when the
 * track cannot be deduplicated (it's missing either tag).
 *
 * @param {Track} track
 * @returns {string | null}
 */
function trackKey(track) {
  const artist = track.tags?.artist?.trim().toLowerCase();
  const title = track.tags?.title?.trim().toLowerCase();
  if (!artist || !title) return null;
  return `${artist}\u0000${title}`;
}

/**
 * Removes duplicate tracks (same artist + same title) from a list, keeping the
 * first occurrence of each pair. Tracks without an artist or title always pass
 * through untouched. A shared `seen` set can be passed so that deduplication
 * is consistent across multiple lists (e.g. grouped tracks).
 *
 * @param {Track[]} tracks
 * @param {Set<string>} [seen]
 * @returns {Track[]}
 */
function deduplicate(tracks, seen = new Set()) {
  /** @type {Track[]} */
  const result = [];

  for (const track of tracks) {
    const key = trackKey(track);
    if (key === null) {
      result.push(track);
      continue;
    }

    if (seen.has(key)) continue;

    seen.add(key);
    result.push(track);
  }

  return result;
}

/**
 * The `do-scoped-tracks` orchestrator is the single source that all theme
 * browsers read their "presented tracks" from, via its `tracks()` and
 * `groups()` signal readers. This feature facet replaces those two signals
 * with derived computed signals that deduplicate at runtime — it never
 * modifies the underlying data or any component source. Only the tracks shown
 * in the UI are affected; the saved `tracks` collection stays intact.
 */
effect(() => {
  const element = foundation.signals.orchestrator.scopedTracks();
  if (!element) return;

  // The orchestrator is typed by its public readers; the runtime patch works
  // on the element instance, so keep a loosely-typed handle for it.
  const target = /** @type {any} */ (element);
  if (target[PATCHED]) return;

  Object.defineProperty(target, PATCHED, { value: true });

  // `tracks` and `groups` are signals on the orchestrator, so they're re-wrapped
  // as derived computed signals rather than plain functions. Reading the
  // original reader inside the computed keeps the dependency graph intact there.
  if (typeof target.tracks === "function") {
    const originalTracks = target.tracks;
    target.tracks = computed(() => deduplicate(originalTracks()));
  }

  if (typeof target.groups === "function") {
    const originalGroups = target.groups;
    target.groups = computed(() => {
      const groups = originalGroups();
      if (!groups) return groups;
      // A single `seen` set across all groups keeps the whole presented set
      // consistent: a (artist, title) pair is shown at most once, no matter
      // which group(s) it would otherwise appear in.
      const seen = new Set();
      return groups.map((/** @type {{ tracks: Track[] }} */ group) => {
        return { ...group, tracks: deduplicate(group.tracks, seen) };
      });
    });
  }
});