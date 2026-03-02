import type {
  Facet,
  PlaylistItem,
  Theme,
  Track,
} from "~/definitions/types.d.ts";

export type FacetsDocument = { collection: Facet[] };
export type PlaylistItemsDocument = { collection: PlaylistItem[] };
export type ThemesDocument = { collection: Theme[] };
export type TracksDocument = { collection: Track[] };
