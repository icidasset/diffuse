import type {
  Constituent,
  Playlist,
  Theme,
  Track,
} from "@definitions/types.d.ts";

export type ConstituentsDocument = { collection: Constituent[] };
export type PlaylistsDocument = { collection: Playlist[] };
export type ThemesDocument = { collection: Theme[] };
export type TracksDocument = { collection: Track[] };
