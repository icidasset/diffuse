export const SCHEMA = {
  id: "string" as const,
  kind: "string" as const,
  tags: {
    album: "string" as const,
    artist: "string" as const,
    genre: "string" as const,
    title: "string" as const,
    year: "number" as const,
  },

  // TODO:
  // isFavorite: "boolean" as const,
  // inPlaylists: [ ... ],

  embeddings: "vector[512]" as const,
};
