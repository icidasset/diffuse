/**
 * @import {PlaylistItem, Track} from "~/definitions/types.d.ts"
 */

import { compareTimestamps } from "~/common/temporal.js";

/**
 * Filter tracks by playlist membership using an indexed lookup.
 *
 * @param {Track[]} tracks
 * @param {PlaylistItem[]} playlistItems
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
 * Bundle playlist items into their respective playlists.
 *
 * @param {PlaylistItem[]} items
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
