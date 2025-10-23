export const SCHEMA = {
  id: /** @type {const} */ ("string"),
  kind: /** @type {const} */ ("string"),
  tags: {
    album: /** @type {const} */ ("string"),
    artist: /** @type {const} */ ("string"),
    genre: /** @type {const} */ ("string"),
    title: /** @type {const} */ ("string"),
    year: /** @type {const} */ ("number"),
  },

  // TODO:
  // isFavorite: "boolean" as const,
  // inPlaylists: [ ... ],

  embeddings: /** @type {const} */ ("vector[512]"),
};
