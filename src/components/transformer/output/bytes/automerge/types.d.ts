import type {
  Facet,
  PlaylistItem,
  Track,
} from "~/definitions/types.d.ts";

export type FacetsDocument = { collection: Facet[] };
export type PlaylistItemsDocument = { collection: PlaylistItem[] };
export type TracksDocument = { collection: Track[] };
