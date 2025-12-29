/**
 * Maps directly on the `Track` definition
 * (ie. `definitions/output/tracks.json`)
 */
export const SCHEMA = {
  id: /** @type {const} */ ("string"),
  kind: /** @type {const} */ ("string"),
  tags: {
    album: /** @type {const} */ ("string"),
    artist: /** @type {const} */ ("string"),
    genre: /** @type {const} */ ("string"),
    title: /** @type {const} */ ("string"),
    year: /** @type {const} */ ("number"),

    disc: {
      no: /** @type {const} */ ("number"),
    },
    track: {
      no: /** @type {const} */ ("number"),
    },
  },

  // TODO:
  // isFavorite: "boolean" as const,
  // inPlaylists: [ ... ],

  embeddings: /** @type {const} */ ("vector[512]"),
};
