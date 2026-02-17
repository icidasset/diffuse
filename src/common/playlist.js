/**
 * @import {Playlist, PlaylistItem, Track} from "@definitions/types.d.ts"
 */

/**
 * @param {any} val
 * @param {string[] | undefined} transformations
 */
function transform(val, transformations) {
  if (!val || !transformations) return val;
  return transformations.reduce((v, t) => {
    try {
      return v[t]();
    } catch (_) {
      return v;
    }
  }, val);
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
 * Filter tracks by playlist membership using an indexed lookup.
 *
 * @param {Track[]} tracks
 * @param {Playlist} playlist
 */
export function filterByPlaylist(tracks, playlist) {
  // Group playlist items by criteria shape, building a Set index per shape.
  const shapes = playlist.items
    .reduce(
      (acc, item) => {
        const shapeKey = item.criteria
          .map((c) => `${c.field}\0${(c.transformations ?? []).join(",")}`)
          .join("\0\0");

        const group = acc.get(shapeKey) ?? acc
          .set(shapeKey, { criteria: item.criteria, keys: new Set() })
          .get(shapeKey);

        group?.keys.add(
          item.criteria.map((c) => transform(c.value, c.transformations)).join(
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
