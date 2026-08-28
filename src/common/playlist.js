/**
 * @import {PlaylistItem, Track} from "~/definitions/types.d.ts"
 */

import { compareTimestamps } from "~/common/temporal.js";

/**
 * Filter tracks by playlist membership using an indexed lookup.
 *
 * @param {Track[]} tracks
 * @param {PlaylistItem[]} playlistItems
 *
 * @example Returns only tracks matching playlist criteria
 * ```js
 * import { filterByPlaylist } from "~/common/playlist.js";
 *
 * const tracks = [
 *   { $type: "sh.diffuse.output.track", id: "a", uri: "http://x.com/a.mp3", tags: { artist: "A", title: "T1" } },
 *   { $type: "sh.diffuse.output.track", id: "b", uri: "http://x.com/b.mp3", tags: { artist: "B", title: "T2" } },
 * ];
 * const items = [
 *   { $type: "sh.diffuse.output.playlistItem", id: "i1", playlist: "p", criteria: [{ field: "tags.artist", value: "A" }] },
 * ];
 * // @ts-ignore
 * const result = filterByPlaylist(tracks, items);
 * if (result.length !== 1 || result[0].id !== "a") throw new Error("expected only track 'a'");
 * ```
 *
 * @example Returns empty array when no tracks match or no items given
 * ```js
 * import { filterByPlaylist } from "~/common/playlist.js";
 *
 * const tracks = [
 *   { $type: "sh.diffuse.output.track", id: "a", uri: "http://x.com/a.mp3", tags: { artist: "A" } },
 * ];
 * const noMatchItems = [
 *   { $type: "sh.diffuse.output.playlistItem", id: "i1", playlist: "p", criteria: [{ field: "tags.artist", value: "Z" }] },
 * ];
 * // @ts-ignore
 * if (filterByPlaylist(tracks, noMatchItems).length !== 0) throw new Error("expected no matches");
 * // @ts-ignore
 * if (filterByPlaylist(tracks, []).length !== 0) throw new Error("expected empty for no items");
 * ```
 *
 * @example Applies transformations before comparing
 * ```js
 * import { filterByPlaylist } from "~/common/playlist.js";
 *
 * const tracks = [{ $type: "sh.diffuse.output.track", id: "a", uri: "http://x.com/a.mp3", tags: { artist: "Artist" } }];
 * const items = [{
 *   $type: "sh.diffuse.output.playlistItem",
 *   id: "i1",
 *   playlist: "p",
 *   criteria: [{ field: "tags.artist", value: "ARTIST", transformations: ["toLowerCase"] }],
 * }];
 * // @ts-ignore
 * if (filterByPlaylist(tracks, items).length !== 1) throw new Error("transformation should match");
 * ```
 */
export function filterByPlaylist(tracks, playlistItems) {
  // Group playlist items by criteria shape, building a Set index per shape.
  const shapes = playlistItems
    .reduce(
      (acc, playlistItem) => {
        const shapeKey = playlistItem.criteria
          .map((c) => `${c.field}\0${(c.transformations ?? []).join(",")}`)
          .join("\0\0");

        const group = acc.get(shapeKey) ?? acc
          .set(shapeKey, { criteria: playlistItem.criteria, keys: new Set() })
          .get(shapeKey);

        group?.keys.add(
          playlistItem.criteria.map((c) =>
            transform(c.value, c.transformations)
          ).join(
            "\0",
          ),
        );

        return acc;
      },
      /** @type {Map<string, { criteria: PlaylistItem["criteria"], keys: Set<string> }>} */ (new Map()),
    )
    .values()
    .map((group) => ({
      fields: group.criteria.map((c) => ({
        parts: c.field.split("."),
        transformations: c.transformations,
      })),
      keys: group.keys,
    }))
    .toArray();

  return tracks.filter((track) =>
    shapes.some((shape) =>
      shape.keys.has(
        shape.fields
          .map(({ parts, transformations }) =>
            transform(
              parts.reduce((v, f) => v?.[f], /** @type {any} */ (track)),
              transformations,
            )
          )
          .join("\0"),
      )
    )
  );
}

/**
 * Order already-playlist-member tracks by their playlist's `positionedAfter`
 * chain. Tracks are bucketed to the first playlist item (in chain order) that
 * they match; each track appears only once. Tracks not matched by any item are
 * dropped (the caller is expected to pre-filter with `filterByPlaylist`).
 *
 * When the playlist is unordered (or empty), the input tracks are returned
 * unchanged so the caller can fall back to its own sort.
 *
 * @param {Track[]} tracks  Tracks already known to belong to the playlist.
 * @param {PlaylistItem[]} items  Items of a single playlist.
 * @returns {Track[]}
 *
 * @example Orders tracks by their playlist item chain
 * ```js
 * import { orderByPlaylist } from "~/common/playlist.js";
 *
 * const tracks = [
 *   { $type: "sh.diffuse.output.track", id: "t-b", uri: "http://x/b.mp3", tags: { title: "B" } },
 *   { $type: "sh.diffuse.output.track", id: "t-a", uri: "http://x/a.mp3", tags: { title: "A" } },
 *   { $type: "sh.diffuse.output.track", id: "t-c", uri: "http://x/c.mp3", tags: { title: "C" } },
 * ];
 * const items = [
 *   { $type: "sh.diffuse.output.playlistItem", id: "i-b", playlist: "p", criteria: [{ field: "tags.title", value: "B" }], positionedAfter: "i-a" },
 *   { $type: "sh.diffuse.output.playlistItem", id: "i-a", playlist: "p", criteria: [{ field: "tags.title", value: "A" }], positionedAfter: undefined },
 *   { $type: "sh.diffuse.output.playlistItem", id: "i-c", playlist: "p", criteria: [{ field: "tags.title", value: "C" }], positionedAfter: "i-b" },
 * ];
 * // @ts-ignore
 * const result = orderByPlaylist(tracks, items);
 * if (result.map((t) => t.id).join(",") !== "t-a,t-b,t-c") throw new Error("should follow chain order");
 * ```
 *
 * @example Returns tracks unchanged for unordered playlists
 * ```js
 * import { orderByPlaylist } from "~/common/playlist.js";
 *
 * const tracks = [{ $type: "sh.diffuse.output.track", id: "t-a", uri: "http://x/a.mp3", tags: { title: "A" } }];
 * const items = [{ $type: "sh.diffuse.output.playlistItem", id: "i-a", playlist: "p", criteria: [{ field: "tags.title", value: "A" }] }];
 * // @ts-ignore
 * const result = orderByPlaylist(tracks, items);
 * if (result !== tracks) throw new Error("unordered playlists should return input untouched");
 * ```
 *
 * @example A track matching multiple items appears only under the first
 * ```js
 * import { orderByPlaylist } from "~/common/playlist.js";
 *
 * const tracks = [
 *   { $type: "sh.diffuse.output.track", id: "t-a", uri: "http://x/a.mp3", tags: { artist: "X", title: "A" } },
 * ];
 * const items = [
 *   { $type: "sh.diffuse.output.playlistItem", id: "i-a", playlist: "p", criteria: [{ field: "tags.artist", value: "X" }], positionedAfter: undefined },
 *   { $type: "sh.diffuse.output.playlistItem", id: "i-b", playlist: "p", criteria: [{ field: "tags.title", value: "A" }], positionedAfter: "i-a" },
 * ];
 * // @ts-ignore
 * const result = orderByPlaylist(tracks, items);
 * if (result.length !== 1) throw new Error("track should appear only once");
 * ```
 */
export function orderByPlaylist(tracks, items) {
  if (!items.length) return tracks;

  // Detect ordered state for the (single) playlist the items belong to.
  const grouped = gather(items);
  const meta = grouped.values().next().value;
  if (!meta || meta.unordered) return tracks;

  const orderedItems = sort(items);
  const claimed = new Set();
  /** @type {Track[]} */
  const out = [];

  for (const item of orderedItems) {
    let matchedAny = false;
    for (const track of tracks) {
      if (claimed.has(track.id)) continue;
      if (match(track, item)) {
        claimed.add(track.id);
        out.push(track);
        matchedAny = true;
      }
    }
    // Items with no matching tracks don't contribute to the output but the
    // chain still proceeds so following items can claim what they can.
    void matchedAny;
  }

  return out;
}

/**
 * Bundle playlist items into their respective playlists.
 *
 * @param {PlaylistItem[]} items
 *
 * @example Groups items by playlist name and tracks ordered/unordered state
 * ```js
 * import { gather } from "~/common/playlist.js";
 *
 * const items = [
 *   { $type: "sh.diffuse.output.playlistItem", id: "1", playlist: "Rock", criteria: [], positionedAfter: "prev" },
 *   { $type: "sh.diffuse.output.playlistItem", id: "2", playlist: "Pop", criteria: [], positionedAfter: "prev" },
 *   { $type: "sh.diffuse.output.playlistItem", id: "3", playlist: "Rock", criteria: [], positionedAfter: "prev" },
 * ];
 * // @ts-ignore
 * const map = gather(items);
 * const rock = map.get("Rock");
 * const pop = map.get("Pop");
 * if (!rock || !pop) throw new Error("expected Rock and Pop playlists");
 * if (rock.items.length !== 2) throw new Error("expected 2 rock items");
 * if (pop.items.length !== 1) throw new Error("expected 1 pop item");
 *
 * // @ts-ignore
 * const unordered = gather([
 *   { $type: "sh.diffuse.output.playlistItem", id: "1", playlist: "Mix", criteria: [] },
 *   { $type: "sh.diffuse.output.playlistItem", id: "2", playlist: "Mix", criteria: [] },
 * ]);
 * const unorderedMix = unordered.get("Mix");
 * if (!unorderedMix) throw new Error("expected Mix playlist");
 * if (!unorderedMix.unordered) throw new Error("playlist without positionedAfter should be unordered");
 *
 * // @ts-ignore
 * const ordered = gather([
 *   { $type: "sh.diffuse.output.playlistItem", id: "1", playlist: "Mix", criteria: [], positionedAfter: undefined },
 *   { $type: "sh.diffuse.output.playlistItem", id: "2", playlist: "Mix", criteria: [], positionedAfter: "1" },
 * ]);
 * const orderedMix = ordered.get("Mix");
 * if (!orderedMix) throw new Error("expected Mix playlist");
 * if (orderedMix.unordered) throw new Error("playlist with positionedAfter should be ordered");
 * ```
 */
export function gather(items) {
  /**
   * @type {Map<string, { items: PlaylistItem[]; name: string; unordered: boolean }>}
   */
  const playlistMap = new Map();

  for (const item of items) {
    const existing = playlistMap.get(item.playlist);

    if (!existing) {
      playlistMap.set(item.playlist, {
        items: [item],
        name: item.playlist,
        unordered: item.positionedAfter == null,
      });
    } else {
      existing.items.push(item);
      existing.unordered = existing.unordered === false
        ? false
        : item.positionedAfter == null;
    }
  }

  return playlistMap;
}

/**
 * Check if a track matches the criteria of a playlist item.
 *
 * @param {Track} track
 * @param {PlaylistItem} item
 *
 * @example Returns true when all criteria match, false when any criterion fails
 * ```js
 * import { match } from "~/common/playlist.js";
 *
 * const track = { $type: "sh.diffuse.output.track", id: "t", uri: "http://x.com/t.mp3", tags: { artist: "Artist A", title: "Song A" } };
 * const item = {
 *   $type: "sh.diffuse.output.playlistItem", id: "i", playlist: "p",
 *   criteria: [{ field: "tags.artist", value: "Artist A" }, { field: "tags.title", value: "Song A" }],
 * };
 * // @ts-ignore
 * if (!match(track, item)) throw new Error("should match when all criteria pass");
 *
 * const mismatch = { ...item, criteria: [{ field: "tags.artist", value: "Artist A" }, { field: "tags.title", value: "Wrong" }] };
 * // @ts-ignore
 * if (match(track, mismatch)) throw new Error("should not match when a criterion fails");
 * ```
 *
 * @example Applies transformations before comparing
 * ```js
 * import { match } from "~/common/playlist.js";
 *
 * const track = { $type: "sh.diffuse.output.track", id: "t", uri: "http://x.com/t.mp3", tags: { artist: "Artist A" } };
 * const item = {
 *   $type: "sh.diffuse.output.playlistItem", id: "i", playlist: "p",
 *   criteria: [{ field: "tags.artist", value: "ARTIST A", transformations: ["toLowerCase"] }],
 * };
 * // @ts-ignore
 * if (!match(track, item)) throw new Error("transformation should match lowercase");
 * ```
 */
export function match(track, item) {
  return item.criteria.every((c) => {
    /** @type {any} */
    let value = track;

    /** @type {any} */
    let critValue = c.value;

    c.field.split(".").forEach((f) => {
      if (value) value = value[f];
    });

    if (value && c.transformations) {
      c.transformations.forEach((t) => {
        try {
          value = value[t]();
          critValue = critValue[t]();
        } catch (err) {}
      });
    }

    return critValue === value;
  });
}

/**
 * Sort playlist items by their `positionedAfter` linked-list order.
 * Items with no `positionedAfter` are placed first.
 *
 * @param {PlaylistItem[]} items
 * @returns {PlaylistItem[]}
 *
 * @example Sorts a linked list in order and appends orphaned items at end
 * ```js
 * import { sort } from "~/common/playlist.js";
 *
 * const single = [{ $type: "sh.diffuse.output.playlistItem", id: "a", playlist: "p", criteria: [], positionedAfter: undefined }];
 * // @ts-ignore
 * if (sort(single).map((i) => i.id).join(",") !== "a") throw new Error("single item should be unchanged");
 *
 * const linked = [
 *   { $type: "sh.diffuse.output.playlistItem", id: "c", playlist: "p", criteria: [], positionedAfter: "b" },
 *   { $type: "sh.diffuse.output.playlistItem", id: "a", playlist: "p", criteria: [], positionedAfter: undefined },
 *   { $type: "sh.diffuse.output.playlistItem", id: "b", playlist: "p", criteria: [], positionedAfter: "a" },
 * ];
 * // @ts-ignore
 * if (sort(linked).map((i) => i.id).join(",") !== "a,b,c") throw new Error("should sort linked list in order");
 *
 * const withOrphan = [
 *   { $type: "sh.diffuse.output.playlistItem", id: "a", playlist: "p", criteria: [], positionedAfter: undefined },
 *   { $type: "sh.diffuse.output.playlistItem", id: "b", playlist: "p", criteria: [], positionedAfter: "a" },
 *   { $type: "sh.diffuse.output.playlistItem", id: "orphan", playlist: "p", criteria: [], positionedAfter: "missing" },
 * ];
 * // @ts-ignore
 * const sorted = sort(withOrphan);
 * if (sorted[sorted.length - 1].id !== "orphan") throw new Error("orphaned item should be last");
 * ```
 *
 * @example Sorts multiple heads by updatedAt ascending
 * ```js
 * import { sort } from "~/common/playlist.js";
 *
 * const items = [
 *   { $type: "sh.diffuse.output.playlistItem", id: "b", playlist: "p", criteria: [], positionedAfter: undefined, updatedAt: "2024-06-01T00:00:00.000Z" },
 *   { $type: "sh.diffuse.output.playlistItem", id: "a", playlist: "p", criteria: [], positionedAfter: undefined, updatedAt: "2024-01-01T00:00:00.000Z" },
 * ];
 * // @ts-ignore
 * const result = sort(items);
 * if (result[0].id !== "a" || result[1].id !== "b") throw new Error("heads should be sorted by updatedAt");
 * ```
 */
export function sort(items) {
  if (items.length <= 1) return items;

  /** @type {Map<string | null, PlaylistItem[]>} */
  const afterMap = new Map();

  for (const item of items) {
    const key = item.positionedAfter ?? null;
    const group = afterMap.get(key);
    if (group) {
      group.push(item);
    } else {
      afterMap.set(key, [item]);
    }
  }

  // Sort each group by updatedAt so collisions have a deterministic order.
  for (const group of afterMap.values()) {
    if (group.length > 1) {
      group.sort((a, b) => {
        if (!a.updatedAt || !b.updatedAt) return a.updatedAt ? 1 : -1;
        return compareTimestamps(a.updatedAt, b.updatedAt);
      });
    }
  }

  /** @type {PlaylistItem[]} */
  const sorted = [];
  const visited = new Set();

  /** @type {PlaylistItem[]} */
  const queue = [...(afterMap.get(null) ?? [])];

  while (queue.length > 0) {
    const current = /** @type {PlaylistItem} */ (queue.shift());
    if (visited.has(current.id)) continue;
    visited.add(current.id);
    sorted.push(current);

    const next = afterMap.get(current.id);
    if (next) queue.unshift(...next);
  }

  // Append any items not reachable from a head (e.g. broken chains).
  for (const item of items) {
    if (!visited.has(item.id)) {
      sorted.push(item);
    }
  }

  return sorted;
}

/**
 * @param {any} val
 * @param {string[] | undefined} transformations
 */
export function transform(val, transformations) {
  if (!val || !transformations) return val;
  return transformations.reduce((v, t) => {
    try {
      return v[t]();
    } catch (_) {
      return v;
    }
  }, val);
}
