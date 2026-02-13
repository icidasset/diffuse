/**
 * @import {Playlist, PlaylistItem, Track} from "@definitions/types.d.ts"
 */

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
